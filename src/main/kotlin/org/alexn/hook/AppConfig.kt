@file:OptIn(ExperimentalSerializationApi::class)

package org.alexn.hook

import arrow.core.Either
import com.charleskorn.kaml.Yaml
import com.charleskorn.kaml.YamlConfiguration
import com.typesafe.config.ConfigFactory
import kotlinx.serialization.Serializable
import kotlinx.serialization.hocon.Hocon
import java.io.File
import kotlin.time.Duration
import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.hocon.decodeFromConfig

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
        fun parseFile(file: File) =
            when (file.extension.lowercase()) {
                "hocon", "conf" -> parseHocon(file)
                "yaml", "yml" -> parseYaml(file)
                else -> Either.Left(
                    ConfigException(
                        "Unsupported configuration file format: ${file.extension}",
                    ),
                )
            }

        fun parseHocon(string: String): Either<ConfigException, AppConfig> =
            try {
                val r = Hocon.decodeFromConfig(
                    serializer(),
                    ConfigFactory.parseString(string).resolve()
                )
                Either.Right(r)
            } catch (ex: Exception) {
                Either.Left(
                    ConfigException(
                        "Failed to parse HOCON configuration",
                        ex,
                    ),
                )
            }

        fun parseHocon(file: File): Either<ConfigException, AppConfig> =
            try {
                val txt = file.readText()
                parseHocon(txt)
            } catch (ex: Exception) {
                Either.Left(
                    ConfigException(
                        "Failed to read configuration file: ${file.absolutePath}",
                        ex,
                    ),
                )
            }

        fun parseYaml(string: String): Either<ConfigException, AppConfig> =
            try {
                Either.Right(yamlParser.decodeFromString(
                    serializer(),
                    string,
                ));
            } catch (ex: Exception) {
                Either.Left(ConfigException(
                    "Failed to parse YAML configuration",
                    ex,
                ))
            }

        fun parseYaml(file: File): Either<ConfigException, AppConfig> =
            try {
                val txt = file.readText()
                parseYaml(txt)
            } catch (ex: Exception) {
                Either.Left(
                    ConfigException(
                        "Failed to read configuration file: ${file.absolutePath}",
                        ex,
                    ),
                )
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
