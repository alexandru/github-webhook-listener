package org.alexn.hook

import arrow.core.Either
import arrow.core.left
import arrow.core.raise.either
import arrow.core.raise.ensureNotNull
import io.github.oshai.kotlinlogging.KotlinLogging
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

private val logger = KotlinLogging.logger {}

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

            val response =
                either {
                    val project = config.projects[projectKey]
                    ensureNotNull(project) {
                        RequestError.NotFound("Project `$projectKey` does not exist")
                    }

                    val signature = call.request.header("X-Hub-Signature-256") ?: call.request.header("X-Hub-Signature")
                    val body = call.receiveText()
                    EventPayload
                        .authenticateRequest(body, project.secret, signature)
                        .bind()

                    val parsed =
                        EventPayload.parse(call.request.contentType(), body).bind()

                    val result =
                        if (parsed.shouldProcess(project)) {
                            commandTriggerService.triggerCommand(projectKey)
                        } else {
                            RequestError.Skipped("Nothing to do for project `$projectKey`").left()
                        }

                    result.bind()
                }

            when (response) {
                is Either.Right -> {
                    call.respondText("OK", status = HttpStatusCode.OK)
                    logger.info { "POST /$projectKey — OK" }
                }
                is Either.Left -> {
                    val err = response.value
                    call.respondText(err.message, status = HttpStatusCode.fromValue(err.httpCode))
                    when (err) {
                        is RequestError.Skipped ->
                            logger.info { "POST /$projectKey — Skipped" }
                        else -> {
                            val ex = err.toException()
                            logger.warn(ex) { "POST /$projectKey — ${ex.message}" }
                        }
                    }
                }
            }
        }
    }
}

// Simple URL encoding function for native
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
