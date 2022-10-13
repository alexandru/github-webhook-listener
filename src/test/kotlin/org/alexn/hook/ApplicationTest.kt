package org.alexn.hook

import io.ktor.client.request.get
import io.ktor.client.request.headers
import io.ktor.client.request.post
import io.ktor.client.request.setBody
import io.ktor.http.ContentType
import io.ktor.http.HttpStatusCode
import io.ktor.http.contentType
import io.ktor.server.testing.ApplicationTestBuilder
import io.ktor.server.testing.testApplication
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import org.apache.commons.codec.digest.HmacAlgorithms
import org.apache.commons.codec.digest.HmacUtils
import java.io.File
import java.io.FileNotFoundException
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import kotlin.test.Test
import kotlin.test.assertEquals

class ApplicationTest {
    private fun appConfig(directory: File) = AppConfig(
        http = AppConfig.Http(port = 9374),
        projects = mapOf(
            "monix" to AppConfig.Project(
                ref = "refs/heads/gh-pages",
                directory = directory.absolutePath.toString(),
                command = "touch ./i-was-here.txt",
                secret = "kdJlfnKd0Llkjddl",
                action = null,
                timeout = null
            )
        )
    )

    data class Helpers(
        val project: AppConfig.Project,
        val json: String,
        val hmacSha1: String,
        val hmacSha256: String,
        val hmacSha512: String,
        val createdFile: File,
    )

    private suspend fun ApplicationTestBuilder.withInitializedApp(block: suspend ApplicationTestBuilder.(Helpers) -> Unit) {
        val tmpDir = withContext(Dispatchers.IO) {
            Files.createTempDirectory("test").toFile()
        }
        try {
            val cfg = appConfig(tmpDir)
            val project = cfg.projects["monix"] ?: throw IllegalArgumentException("Missing project key in config (monix)")
            val cmdTrigger = CommandTrigger(cfg.projects)
            application {
                configureRouting(cfg, cmdTrigger)
            }
            val json = withContext(Dispatchers.IO) {
                javaClass.getResourceAsStream("/real-payload.json")?.readAllBytes()?.toString(StandardCharsets.UTF_8)
                    ?: throw FileNotFoundException("/resources/real-payload.json")
            }
            block(
                Helpers(
                    project = project,
                    json = json,
                    hmacSha512 = "sha512=" + HmacUtils(HmacAlgorithms.HMAC_SHA_512, project.secret).hmacHex(json),
                    hmacSha256 = "sha256=" + HmacUtils(HmacAlgorithms.HMAC_SHA_256, project.secret).hmacHex(json),
                    hmacSha1 = "sha1=" + HmacUtils(HmacAlgorithms.HMAC_SHA_1, project.secret).hmacHex(json),
                    createdFile = File(tmpDir, "i-was-here.txt")
                )
            )
        } finally {
            tmpDir.deleteRecursively()
        }
    }

    @Test
    fun `root ping`() = testApplication {
        withInitializedApp {
            client.get("/").apply {
                assertEquals(HttpStatusCode.OK, status)
            }
        }
    }

    @Test
    fun `trigger with sha256 authentication`() = testApplication {
        withInitializedApp { sample ->
            client.post("/monix") {
                contentType(ContentType.Application.Json)
                headers {
                    append("X-Hub-Signature-256", sample.hmacSha256)
                }
                setBody(sample.json)
            }.apply {
                assertEquals(HttpStatusCode.OK, status)
            }
            assert(sample.createdFile.exists()) {
                "File should exist: `${sample.createdFile.absolutePath}`"
            }
        }
    }

    @Test
    fun `trigger with sha1 authentication`() = testApplication {
        withInitializedApp { sample ->
            client.post("/monix") {
                contentType(ContentType.Application.Json)
                headers {
                    append("X-Hub-Signature", sample.hmacSha1)
                }
                setBody(sample.json)
            }.apply {
                assertEquals(HttpStatusCode.OK, status)
            }
            assert(sample.createdFile.exists()) {
                "File should exist: `${sample.createdFile.absolutePath}`"
            }
        }
    }

    @Test
    fun `reject unauthenticated payload`() = testApplication {
        withInitializedApp { sample ->
            client.post("/monix") {
                contentType(ContentType.Application.Json)
                setBody(sample.json)
            }.apply {
                assertEquals(HttpStatusCode.Forbidden, status)
            }
        }
    }

    @Test
    fun `reject unsupported algorithm`() = testApplication {
        withInitializedApp { sample ->
            client.post("/monix") {
                contentType(ContentType.Application.Json)
                headers {
                    append("X-Hub-Signature", sample.hmacSha512)
                }
                setBody(sample.json)
            }.apply {
                assertEquals(HttpStatusCode.Forbidden, status)
            }
        }
    }

    @Test
    fun `project must exist`() = testApplication {
        withInitializedApp { sample ->
            client.post("/notavailable") {
                contentType(ContentType.Application.Json)
                headers {
                    append("X-Hub-Signature", sample.hmacSha1)
                }
                setBody(sample.json)
            }.apply {
                assertEquals(HttpStatusCode.NotFound, status)
            }
        }
    }
}
