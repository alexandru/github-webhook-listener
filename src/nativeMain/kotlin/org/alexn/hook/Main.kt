package org.alexn.hook

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.core.Context
import com.github.ajalt.clikt.core.main
import com.github.ajalt.clikt.parameters.arguments.argument
import kotlinx.coroutines.runBlocking

class RunServer :
    CliktCommand(
        name = "github-webhook-listener",
    ) {
    val configPath by argument(help = "Path to the application configuration")

    override fun help(context: Context) = "Start the server"

    override fun run() = runBlocking {
        val config = AppConfig.parseFile(configPath)
        when (config) {
            is Result.Success -> startServer(config.value)
            is Result.Error -> throw config.exception
        }
    }
}

fun main(args: Array<String>) {
    RunServer().main(args)
}
