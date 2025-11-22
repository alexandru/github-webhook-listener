package org.alexn.hook

import io.ktor.http.HttpStatusCode
import io.ktor.server.application.Application
import io.ktor.server.application.call
import io.ktor.server.cio.CIO
import io.ktor.server.engine.embeddedServer
import io.ktor.server.html.respondHtml
import io.ktor.server.request.contentType
import io.ktor.server.request.header
import io.ktor.server.request.receiveText
import io.ktor.server.response.respondRedirect
import io.ktor.server.response.respondText
import io.ktor.server.routing.get
import io.ktor.server.routing.post
import io.ktor.server.routing.routing
import kotlinx.html.body
import kotlinx.html.head
import kotlinx.html.li
import kotlinx.html.p
import kotlinx.html.title
import kotlinx.html.ul

suspend fun startServer(appConfig: AppConfig) {
    val commandTrigger = CommandTrigger(appConfig.projects)
    val server =
        embeddedServer(
            CIO,
            port = appConfig.http.port,
            host = appConfig.http.host ?: "0.0.0.0",
        ) {
            configureRouting(appConfig, commandTrigger)
        }
    server.start(wait = true)
}

fun Application.configureRouting(
    config: AppConfig,
    commandTriggerService: CommandTrigger,
) {
    val basePath = config.http.basePath

    routing {
        if (config.http.basePath.isNotEmpty()) {
            get(config.http.basePath) {
                call.respondRedirect("$basePath/")
            }
        }

        get("$basePath/") {
            call.respondHtml(HttpStatusCode.OK) {
                head {
                    title { +"GitHub Webhook Listener" }
                }
                body {
                    p { +"Configured hooks:" }
                    ul {
                        for (p in config.projects) {
                            li { +urlEncode(p.key) }
                        }
                    }
                }
            }
        }

        post("$basePath/{project}") {
            val projectKey = call.parameters["project"]
            if (projectKey == null) {
                call.respondText("Project key not specified", status = HttpStatusCode.BadRequest)
                return@post
            }

            val project = config.projects[projectKey]
            if (project == null) {
                val err = RequestError.NotFound("Project `$projectKey` does not exist")
                call.respondText(err.message, status = HttpStatusCode.fromValue(err.httpCode))
                println("POST /$projectKey — Not Found")
                return@post
            }

            val signature = call.request.header("X-Hub-Signature-256") ?: call.request.header("X-Hub-Signature")
            val body = call.receiveText()
            
            val authResult = EventPayload.authenticateRequest(body, project.secret, signature)
            if (authResult is Result.Error) {
                val err = authResult.exception as? RequestError.Forbidden ?: RequestError.Forbidden("Authentication failed")
                call.respondText(err.message, status = HttpStatusCode.fromValue(err.httpCode))
                println("POST /$projectKey — Forbidden: ${err.message}")
                return@post
            }

            val parsed = EventPayload.parse(call.request.contentType(), body)
            if (parsed is Result.Error) {
                val err = (parsed.exception as? RequestError) ?: RequestError.BadInput("Parse error", parsed.exception)
                call.respondText(err.message, status = HttpStatusCode.fromValue(err.httpCode))
                println("POST /$projectKey — Bad Input: ${err.message}")
                return@post
            }

            val payload = (parsed as Result.Success).value
            if (!payload.shouldProcess(project)) {
                call.respondText("Nothing to do for project `$projectKey`", status = HttpStatusCode.OK)
                println("POST /$projectKey — Skipped")
                return@post
            }

            val result = commandTriggerService.triggerCommand(projectKey)
            when (result) {
                is Result.Success -> {
                    call.respondText("OK", status = HttpStatusCode.OK)
                    println("POST /$projectKey — OK")
                }
                is Result.Error -> {
                    val err = (result.exception as? RequestError) ?: RequestError.Internal("Command execution failed", result.exception, null)
                    call.respondText(err.message, status = HttpStatusCode.fromValue(err.httpCode))
                    println("POST /$projectKey — Error: ${err.message}")
                }
            }
        }
    }
}

// Simple URL encoding function
private fun urlEncode(str: String): String {
    return str.replace("%", "%25")
        .replace(" ", "%20")
        .replace("!", "%21")
        .replace("\"", "%22")
        .replace("#", "%23")
        .replace("$", "%24")
        .replace("&", "%26")
        .replace("'", "%27")
        .replace("(", "%28")
        .replace(")", "%29")
        .replace("*", "%2A")
        .replace("+", "%2B")
        .replace(",", "%2C")
        .replace("/", "%2F")
        .replace(":", "%3A")
        .replace(";", "%3B")
        .replace("=", "%3D")
        .replace("?", "%3F")
        .replace("@", "%40")
        .replace("[", "%5B")
        .replace("]", "%5D")
}
