package org.alexn.hook

import arrow.continuations.SuspendApp
import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.arguments.argument
import java.io.File

class RunServer : CliktCommand(
    name = "github-webhook-listener",
    help = "Start the server",
) {
    val configPath by argument(help = "Path to the application configuration")

    override fun run() =
        SuspendApp {
            val config = AppConfig.parseYaml(File(configPath))
            startServer(config)
        }
}

fun main(args: Array<String>) {
    RunServer().main(args)
}
