package org.alexn.hook

import com.charleskorn.kaml.Yaml
import com.charleskorn.kaml.YamlConfiguration
import kotlinx.serialization.Serializable
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
        fun parseYaml(string: String): AppConfig =
            yamlParser.decodeFromString(
                serializer(),
                string,
            )

        fun parseYaml(file: File): AppConfig {
            val txt = file.readText()
            return parseYaml(txt)
        }

        private val yamlParser =
            Yaml(
                configuration =
                    YamlConfiguration(
                        strictMode = false,
                    ),
            )
    }
}
