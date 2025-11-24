package org.alexn.hook

import arrow.continuations.SuspendApp
import arrow.core.getOrElse
import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.core.Context
import com.github.ajalt.clikt.core.main
import com.github.ajalt.clikt.parameters.arguments.argument

class RunServer :
    CliktCommand(
        name = "github-webhook-listener",
    ) {
    val configPath by argument(help = "Path to the application configuration")

    override fun help(context: Context) = "Start the server"

    override fun run() =
        SuspendApp {
            val config = AppConfig.parseFile(configPath)
            startServer(config.getOrElse { throw it })
        }
}

fun main(args: Array<String>) {
    RunServer().main(args)
}
