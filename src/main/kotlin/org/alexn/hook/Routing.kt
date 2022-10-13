package org.alexn.hook

import arrow.core.Either
import arrow.core.continuations.either
import arrow.core.left
import io.ktor.http.HttpStatusCode
import io.ktor.serialization.kotlinx.json.json
import io.ktor.server.application.Application
import io.ktor.server.application.call
import io.ktor.server.application.install
import io.ktor.server.plugins.contentnegotiation.ContentNegotiation
import io.ktor.server.request.contentType
import io.ktor.server.request.header
import io.ktor.server.request.receiveText
import io.ktor.server.response.respond
import io.ktor.server.response.respondRedirect
import io.ktor.server.response.respondText
import io.ktor.server.routing.get
import io.ktor.server.routing.post
import io.ktor.server.routing.routing
import kotlinx.serialization.json.Json
import org.slf4j.Logger
import org.slf4j.LoggerFactory

fun Application.configureRouting(
    config: AppConfig,
    commandTriggerService: CommandTrigger
) {
    val logger: Logger by lazy {
        LoggerFactory.getLogger("org.alexn.hook.Routing")
    }
    val basePath = config.http.basePath

    routing {
        install(ContentNegotiation) {
            json(
                Json {
                    prettyPrint = true
                    isLenient = true
                }
            )
        }

        if (config.http.basePath.isNotEmpty())
            get(config.http.basePath) {
                call.respondRedirect("$basePath/")
            }

        get("$basePath/") {
            call.respond(
                mapOf(
                    "configured" to config.projects.map { it.key }
                )
            )
        }

        post("$basePath/{project}") {
            val projectKey = call.parameters["project"]
            if (projectKey == null) {
                call.respondText("Project key not specified", status = HttpStatusCode.BadRequest)
                return@post
            }

            val response = either {
                val project = Either
                    .fromNullable(config.projects[projectKey])
                    .mapLeft { RequestError.NotFound("Project `$projectKey` does not exist") }
                    .bind()

                val signature = call.request.header("X-Hub-Signature-256") ?: call.request.header("X-Hub-Signature")
                val body = call.receiveText()
                EventPayload
                    .authenticateRequest(body, project.secret, signature)
                    .bind()

                val parsed =
                    EventPayload.parse(call.request.contentType(), body).bind()

                val result = if (parsed.shouldProcess(project))
                    commandTriggerService.triggerCommand(projectKey)
                else
                    RequestError.Skipped("Nothing to do for project `$projectKey`").left()

                result.bind()
            }

            when (response) {
                is Either.Right -> {
                    call.respondText("OK", status = HttpStatusCode.OK)
                    logger.info("POST /$projectKey — OK")
                }
                is Either.Left -> {
                    val err = response.value
                    call.respondText(err.message, status = HttpStatusCode.fromValue(err.httpCode))
                    when (err) {
                        is RequestError.Skipped ->
                            logger.info("POST /$projectKey — Skipped")
                        else -> {
                            val ex = err.toException()
                            logger.warn("POST /$projectKey — ${ex.message}", ex.cause)
                        }
                    }
                }
            }
        }
    }
}
