package org.alexn.hook

import com.charleskorn.kaml.Yaml
import com.charleskorn.kaml.YamlConfiguration
import com.typesafe.config.ConfigFactory
import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.Serializable
import kotlinx.serialization.hocon.Hocon
import java.io.File
import kotlin.time.Duration

@Serializable
data class AppConfig(
    val http: Http,
    val projects: Map<String, Project>,
) {
    @Serializable
    data class Http(
        val port: Int,
        val host: String? = null,
        val path: String? = null,
    ) {
        val basePath: String
            get() {
                var bp = path ?: return ""
                if (bp.endsWith("/")) bp = bp.dropLast(1)
                return bp
            }
    }

    @Serializable
    data class Project(
        val ref: String,
        val directory: String,
        val command: String,
        val secret: String,
        val action: String? = null,
        val timeout: Duration? = null,
    )

    companion object {
        @OptIn(ExperimentalSerializationApi::class)
        fun parseHocon(string: String): AppConfig =
            Hocon.decodeFromConfig(
                serializer(),
                ConfigFactory.parseString(string).resolve()
            )

        fun parseYaml(string: String): AppConfig =
            yamlParser.decodeFromString(
                serializer(),
                string
            )

        fun loadFromFile(file: File): AppConfig {
            val txt = file.readText()
            return if (file.extension.matches("(?i)yaml|yml".toRegex()))
                parseYaml(txt)
            else
                parseHocon(txt)
        }

        private val yamlParser = Yaml(
            configuration = YamlConfiguration(
                strictMode = false
            )
        )
    }
}
