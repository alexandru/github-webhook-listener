package org.alexn.hook

import org.junit.jupiter.api.Timeout
import org.junit.jupiter.api.io.TempDir
import java.io.IOException
import java.net.InetAddress
import java.net.ServerSocket
import java.net.URI
import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.http.HttpResponse
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Path
import java.time.Duration
import java.util.HexFormat
import java.util.concurrent.TimeUnit
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import kotlin.test.Test
import kotlin.test.assertContains
import kotlin.test.assertEquals
import kotlin.test.assertTrue
import kotlin.test.fail

class NativeExecutableSmokeTest {
    @field:TempDir
    lateinit var workDirectory: Path

    @Test
    @Timeout(value = 2, unit = TimeUnit.MINUTES)
    fun `production native executable handles webhooks`() {
        val executable =
            System
                .getProperty(EXECUTABLE_PROPERTY)
                ?.let(Path::of)
                ?.toAbsolutePath()
                ?: fail("Missing system property: $EXECUTABLE_PROPERTY")
        assertTrue(Files.isRegularFile(executable), "Native executable does not exist: $executable")
        assertTrue(Files.isExecutable(executable), "Native executable is not executable: $executable")

        var lastPortCollision: PortCollision? = null
        for (attempt in 1..PORT_ATTEMPTS) {
            try {
                runAttempt(executable, availablePort(), attempt)
                return
            } catch (error: PortCollision) {
                lastPortCollision = error
            }
        }
        fail("Could not start the server after $PORT_ATTEMPTS port attempts", lastPortCollision)
    }

    private fun runAttempt(
        executable: Path,
        port: Int,
        attempt: Int,
    ) {
        val config = writeConfig(port)
        val log = workDirectory.resolve("native-process-$attempt.log")
        val marker = workDirectory.resolve(MARKER_FILE)
        var process: Process? = null
        try {
            process =
                ProcessBuilder(executable.toString(), config.toString())
                    .directory(workDirectory.toFile())
                    .redirectInput(ProcessBuilder.Redirect.from(NULL_DEVICE.toFile()))
                    .redirectErrorStream(true)
                    .redirectOutput(log.toFile())
                    .start()

            val baseUri = URI("http://$HOST:$port")
            waitUntilReady(process, baseUri)

            val root = sendRequest(baseUri.resolve("/"))
            assertEquals(200, root.statusCode(), "GET /")
            assertContains(root.body(), "GitHub Webhook Listener")
            assertContains(root.body(), PROJECT)

            val validSignature = "sha256=${hmacSha256(SECRET, PAYLOAD)}"
            val success = sendWebhook(baseUri.resolve("/$PROJECT"), validSignature)
            assertResponse(success, 200, "OK", "authenticated webhook")
            waitForMarker(marker)

            val invalidSignature = sendWebhook(baseUri.resolve("/$PROJECT"), "sha256=${"0".repeat(64)}")
            assertResponse(invalidSignature, 403, "Invalid checksum (sha256)", "invalid signature")

            val unknownProject = sendWebhook(baseUri.resolve("/missing-project"), validSignature)
            assertResponse(
                unknownProject,
                404,
                "Project `missing-project` does not exist",
                "unknown project",
            )

            assertEquals(
                "invoked\n",
                Files.readString(marker, UTF_8),
                "Rejected requests must not execute the configured command",
            )
        } catch (error: Throwable) {
            process?.let(::stopProcess)
            val output = processOutput(log)
            if (isPortCollision(output)) {
                throw PortCollision(output, error)
            }
            throw AssertionError(
                "${error.message ?: error::class.simpleName}\n\nnative process output:\n$output",
                error,
            )
        } finally {
            process?.let(::stopProcess)
        }
    }

    private fun writeConfig(port: Int): Path {
        val config = workDirectory.resolve("application-smoke.yaml")
        Files.writeString(
            config,
            """
            http:
              host: ${yamlString(HOST)}
              port: $port
              path: "/"
            projects:
              $PROJECT:
                action: "push"
                ref: ${yamlString(REF)}
                directory: ${yamlString(workDirectory.toString())}
                command: ${yamlString("printf 'invoked\\n' >> $MARKER_FILE")}
                timeout: "PT5S"
                secret: ${yamlString(SECRET)}
            """.trimIndent() + "\n",
            UTF_8,
        )
        return config
    }

    private fun waitUntilReady(
        process: Process,
        baseUri: URI,
    ) {
        val deadline = System.nanoTime() + STARTUP_TIMEOUT.toNanos()
        var lastError = "server did not accept a connection"
        while (System.nanoTime() < deadline) {
            if (!process.isAlive) {
                fail("Native process exited before readiness with code ${process.exitValue()}")
            }
            try {
                val response = sendRequest(baseUri.resolve("/"))
                if (response.statusCode() == 200 && PROJECT in response.body()) {
                    return
                }
                lastError = "GET / returned HTTP ${response.statusCode()}: ${response.body()}"
            } catch (error: IOException) {
                lastError = error.message ?: error::class.simpleName.orEmpty()
            }
            Thread.sleep(POLL_INTERVAL.toMillis())
        }
        fail("Server was not ready after ${STARTUP_TIMEOUT.seconds}s: $lastError")
    }

    private fun waitForMarker(marker: Path) {
        val deadline = System.nanoTime() + MARKER_TIMEOUT.toNanos()
        while (System.nanoTime() < deadline) {
            if (Files.exists(marker) && Files.readString(marker, UTF_8) == "invoked\n") {
                return
            }
            Thread.sleep(MARKER_POLL_INTERVAL.toMillis())
        }
        val actual = if (Files.exists(marker)) Files.readString(marker, UTF_8) else "<missing>"
        fail("Command marker was ${actual.quote()}; expected ${"invoked\n".quote()}")
    }

    private fun sendWebhook(
        uri: URI,
        signature: String,
    ): HttpResponse<String> =
        sendRequest(
            uri,
            body = PAYLOAD,
            headers =
                mapOf(
                    "Content-Type" to "application/json",
                    "X-Hub-Signature-256" to signature,
                ),
        )

    private fun sendRequest(
        uri: URI,
        body: String? = null,
        headers: Map<String, String> = emptyMap(),
    ): HttpResponse<String> {
        val builder =
            HttpRequest
                .newBuilder(uri)
                .timeout(REQUEST_TIMEOUT)
        headers.forEach(builder::header)
        if (body == null) {
            builder.GET()
        } else {
            builder.POST(HttpRequest.BodyPublishers.ofString(body, UTF_8))
        }
        return httpClient.send(builder.build(), HttpResponse.BodyHandlers.ofString(UTF_8))
    }

    private fun assertResponse(
        response: HttpResponse<String>,
        expectedStatus: Int,
        expectedBody: String,
        requestName: String,
    ) {
        assertEquals(expectedStatus, response.statusCode(), requestName)
        assertEquals(expectedBody, response.body(), requestName)
    }

    private fun stopProcess(process: Process) {
        if (!process.isAlive) return

        val descendants = process.descendants().toList()
        descendants.forEach(ProcessHandle::destroy)
        process.destroy()
        if (process.waitFor(SHUTDOWN_TIMEOUT.seconds, TimeUnit.SECONDS)) return

        descendants.filter(ProcessHandle::isAlive).forEach(ProcessHandle::destroyForcibly)
        process.destroyForcibly()
        process.waitFor(SHUTDOWN_TIMEOUT.seconds, TimeUnit.SECONDS)
    }

    private fun processOutput(log: Path): String =
        if (Files.exists(log)) {
            Files.readString(log, UTF_8).trim().ifEmpty { "<no process output>" }
        } else {
            "<no process output>"
        }

    private fun availablePort(): Int =
        ServerSocket(0, 1, InetAddress.getByName(HOST)).use { socket ->
            socket.localPort
        }

    private fun hmacSha256(
        secret: String,
        body: String,
    ): String {
        val mac = Mac.getInstance("HmacSHA256")
        mac.init(SecretKeySpec(secret.toByteArray(UTF_8), "HmacSHA256"))
        return HexFormat.of().formatHex(mac.doFinal(body.toByteArray(UTF_8)))
    }

    private fun yamlString(value: String): String =
        buildString {
            append('"')
            value.forEach { char ->
                when (char) {
                    '\\' -> append("\\\\")
                    '"' -> append("\\\"")
                    '\n' -> append("\\n")
                    '\r' -> append("\\r")
                    '\t' -> append("\\t")
                    else -> append(char)
                }
            }
            append('"')
        }

    private fun String.quote(): String = "\"$this\""

    private fun isPortCollision(output: String): Boolean =
        output.contains("address already in use", ignoreCase = true) ||
            output.contains("BindException", ignoreCase = true)

    private class PortCollision(
        output: String,
        cause: Throwable,
    ) : RuntimeException(output, cause)

    companion object {
        private const val EXECUTABLE_PROPERTY = "native.executable"
        private const val HOST = "127.0.0.1"
        private const val PROJECT = "native-smoke"
        private const val REF = "refs/heads/native-smoke"
        private const val SECRET = "native-smoke-secret"
        private const val PAYLOAD = "{\"action\":\"push\",\"ref\":\"$REF\"}"
        private const val MARKER_FILE = "smoke-marker.txt"
        private const val PORT_ATTEMPTS = 3
        private val NULL_DEVICE = Path.of("/dev/null")
        private val STARTUP_TIMEOUT = Duration.ofSeconds(30)
        private val REQUEST_TIMEOUT = Duration.ofSeconds(5)
        private val MARKER_TIMEOUT = Duration.ofSeconds(2)
        private val POLL_INTERVAL = Duration.ofMillis(100)
        private val MARKER_POLL_INTERVAL = Duration.ofMillis(50)
        private val SHUTDOWN_TIMEOUT = Duration.ofSeconds(5)
        private val httpClient =
            HttpClient
                .newBuilder()
                .connectTimeout(REQUEST_TIMEOUT)
                .build()
    }
}
