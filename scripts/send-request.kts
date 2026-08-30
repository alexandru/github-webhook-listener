#!/usr/bin/env -S jbang --source-type kotlin
//JAVA 17+
//KOTLIN 2.4.10
//DEPS com.github.ajalt.clikt:clikt-jvm:5.1.0
//DEPS org.jetbrains.kotlinx:kotlinx-serialization-json-jvm:1.11.0

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.core.Context
import com.github.ajalt.clikt.core.main
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.required
import kotlinx.serialization.json.buildJsonObject
import kotlinx.serialization.json.put
import java.net.URI
import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.http.HttpResponse
import java.nio.charset.StandardCharsets.UTF_8
import java.util.HexFormat
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

class SendRequest :
    CliktCommand(
        name = "send-request",
    ) {
    private val url by option(
        "--url",
        metavar = "URL",
        help = "Full webhook URL, including the project slug",
    ).required()

    private val secret by option(
        "--secret",
        metavar = "KEY",
        help = "Secret configured for the project",
        envvar = "GITHUB_WEBHOOK_SECRET",
    ).required()

    private val ref by option(
        "--ref",
        metavar = "GIT_REF",
        help = "Git ref configured for the project",
    ).required()

    private val action by option(
        "--action",
        metavar = "ACTION",
        help = "Webhook action",
    ).default("push")

    override fun help(context: Context) = "Send a signed webhook request"

    override fun run() {
        val body =
            buildJsonObject {
                put("action", action)
                put("ref", ref)
            }.toString()
        val signature = "sha256=${hmacSha256(secret, body)}"
        val request =
            HttpRequest
                .newBuilder(URI(url))
                .header("Content-Type", "application/json")
                .header("X-Hub-Signature-256", signature)
                .POST(HttpRequest.BodyPublishers.ofString(body, UTF_8))
                .build()
        val response = HttpClient.newHttpClient().send(request, HttpResponse.BodyHandlers.ofString(UTF_8))

        echo("HTTP ${response.statusCode()}: ${response.body()}")
    }
}

private fun hmacSha256(secret: String, body: String): String {
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(SecretKeySpec(secret.toByteArray(UTF_8), "HmacSHA256"))
    return HexFormat.of().formatHex(mac.doFinal(body.toByteArray(UTF_8)))
}

fun main(args: Array<String>) {
    SendRequest().main(args)
}
