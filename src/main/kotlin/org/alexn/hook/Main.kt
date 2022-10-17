package org.alexn.hook

import io.ktor.client.HttpClient
import io.ktor.client.engine.cio.CIO
import io.ktor.client.request.get
import io.ktor.client.request.headers
import io.ktor.client.request.post
import io.ktor.client.request.setBody
import io.ktor.client.statement.HttpResponse
import io.ktor.client.statement.bodyAsText
import io.ktor.http.HttpHeaders
import io.ktor.http.HttpStatusCode
import kotlinx.cli.ArgParser
import kotlinx.cli.ArgType
import kotlinx.cli.ExperimentalCli
import kotlinx.cli.Subcommand
import kotlinx.cli.required
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withTimeout
import org.apache.commons.codec.digest.HmacAlgorithms
import org.apache.commons.codec.digest.HmacUtils
import org.slf4j.LoggerFactory
import java.io.File
import kotlin.time.Duration.Companion.seconds

@OptIn(ExperimentalCli::class)
fun main(args: Array<String>) {
    val parser = ArgParser(programName = "github-webhook-listener")
    val configPath by parser
        .option(
            ArgType.String,
            fullName = "config-path",
            shortName = "c",
            description = "Path to the application configuration"
        )
        .required()

    val startServer = object : Subcommand(
        "start-server",
        "Starts the long-running server process."
    ) {
        override fun execute() = startServer(configPath)
    }

    val runScenario = object : Subcommand(
        "run-test-scenario",
        "Runs a full test scenario."
    ) {
        override fun execute() = runTestScenario(configPath)
    }

    parser.subcommands(startServer, runScenario)
    parser.parse(args)
}

fun startServer(configPath: String) = runBlocking {
    val config = AppConfig.loadFromFile(File(configPath))
    startServer(config)
}

fun runTestScenario(configPath: String) {
    suspend fun post(
        client: HttpClient,
        baseURL: String,
        projName: String,
        projCfg: AppConfig.Project,
        algo: HmacAlgorithms,
        body: String
    ): HttpResponse {
        val signKey = projCfg.secret
        return client.post("$baseURL/$projName") {
            headers {
                append(HttpHeaders.ContentType, "application/json")
                if (algo == HmacAlgorithms.HMAC_SHA_256) {
                    append(
                        "X-Hub-Signature-256",
                        "sha256=" + HmacUtils(HmacAlgorithms.HMAC_SHA_256, signKey).hmacHex(body)
                    )
                } else if (algo == HmacAlgorithms.HMAC_SHA_512) {
                    append(
                        "X-Hub-Signature-256",
                        "sha512=" + HmacUtils(HmacAlgorithms.HMAC_SHA_512, signKey).hmacHex(body)
                    )
                } else {
                    append(
                        "X-Hub-Signature",
                        "sha1=" + HmacUtils(HmacAlgorithms.HMAC_SHA_1, signKey).hmacHex(body)
                    )
                }
            }
            setBody(body)
        }
    }

    runBlocking {
        val logger = LoggerFactory.getLogger("Main")
        val config = AppConfig.loadFromFile(File(configPath))
        val server = launch { startServer(config) }
        val client = HttpClient(CIO)
        val baseURL = "http://${config.http.host ?: "0.0.0.0"}:${config.http.port}${config.http.basePath}"
        val supportedAlgos = listOf(
            HmacAlgorithms.HMAC_SHA_1,
            HmacAlgorithms.HMAC_SHA_256,
            HmacAlgorithms.HMAC_SHA_512
        )
        try {
            // Wait for the server to start, by sending a ping
            withTimeout(3.seconds) {
                var isUpAndRunning = false
                do {
                    try {
                        val resp = client.get("$baseURL/")
                        isUpAndRunning = resp.status == HttpStatusCode.OK
                    } catch (_: Exception) {}
                } while (!isUpAndRunning)
                logger.info("Test — server is up and running at $baseURL")
            }
            // Test each project
            val projects = config.projects + mapOf(
                "nonExistentProj" to AppConfig.Project(
                    ref = "refs/heads/gh-pages",
                    directory = "/tmp",
                    command = "echo",
                    secret = "xxxxx"
                )
            )
            for (proj in projects) {
                for (algo in supportedAlgos) {
                    val resp1 = post(
                        client,
                        baseURL,
                        proj.key,
                        proj.value,
                        algo,
                        """
                        {
                            "action": "${proj.value.action ?: "push"}",
                            "ref": "${proj.value.ref}"
                        }
                        """.trimIndent()
                    )
                    logger.info("Test (1) — $baseURL/${proj.key} ($algo) — ${resp1.status.description} — ${resp1.bodyAsText()}")
                    val resp2 = post(
                        client,
                        baseURL,
                        proj.key,
                        proj.value,
                        algo,
                        """
                        {
                            "action": "blah-389iu2",
                            "ref": "blah/blah/293"
                        }
                        """.trimIndent()
                    )
                    logger.info("Test (2) — $baseURL/${proj.key} ($algo) — ${resp2.status.description} — ${resp2.bodyAsText()}")
                }
            }
        } finally {
            server.cancel()
        }
    }
}
