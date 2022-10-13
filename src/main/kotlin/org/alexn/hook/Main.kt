package org.alexn.hook

import io.ktor.server.cio.CIO
import io.ktor.server.engine.embeddedServer
import kotlinx.cli.ArgParser
import kotlinx.cli.ArgType
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.runInterruptible
import java.io.File

fun main(args: Array<String>): Unit = runBlocking {
    val appConfig = loadConfig(args)
    val commandTrigger = CommandTrigger(appConfig.projects)
    val server = embeddedServer(
        CIO,
        port = appConfig.http.port,
        host = appConfig.http.host ?: "0.0.0.0",
    ) {
        configureRouting(appConfig, commandTrigger)
    }
    runInterruptible {
        server.start(wait = true)
    }
}

fun loadConfig(args: Array<String>): AppConfig {
    val parser = ArgParser(programName = "github-webhook-listener")
    val config by parser
        .argument(
            ArgType.String,
            fullName = "config-path",
            description = "Path to the application configuration"
        )

    parser.parse(args)
    return AppConfig.loadFromFile(File(config))
}
