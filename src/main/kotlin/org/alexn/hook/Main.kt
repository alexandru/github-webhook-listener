package org.alexn.hook

import kotlinx.cli.ArgParser
import kotlinx.cli.ArgType
import kotlinx.coroutines.runBlocking
import java.io.File

fun main(args: Array<String>) {
    val parser = ArgParser(programName = "github-webhook-listener")
    val configPath by parser.argument(
        ArgType.String,
        fullName = "config-path",
        description = "Path to the application configuration"
    )

    parser.parse(args)
    val config = AppConfig.parseYaml(File(configPath))
    runBlocking { startServer(config) }
}
