@file:OptIn(ExperimentalSerializationApi::class)

package org.alexn.hook

import arrow.core.Either
import com.charleskorn.kaml.Yaml
import com.charleskorn.kaml.YamlConfiguration
import kotlinx.cinterop.ExperimentalForeignApi
import kotlinx.cinterop.toKString
import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.Serializable
import platform.posix.fclose
import platform.posix.fgets
import platform.posix.fopen
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
        @OptIn(ExperimentalForeignApi::class)
        fun parseFile(filePath: String): Either<ConfigException, AppConfig> {
            val extension = filePath.substringAfterLast('.', "").lowercase()
            
            val content = try {
                readFile(filePath)
            } catch (ex: Exception) {
                return Either.Left(
                    ConfigException(
                        "Failed to read configuration file: $filePath",
                        ex,
                    )
                )
            }

            return when (extension) {
                "yaml", "yml" -> parseYaml(content)
                else ->
                    Either.Left(
                        ConfigException(
                            "Unsupported configuration file format: $extension (only YAML/YML supported for native)",
                        ),
                    )
            }
        }

        fun parseYaml(string: String): Either<ConfigException, AppConfig> =
            try {
                Either.Right(
                    yamlParser.decodeFromString(
                        serializer(),
                        string,
                    ),
                )
            } catch (ex: Exception) {
                Either.Left(
                    ConfigException(
                        "Failed to parse YAML configuration",
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

        @OptIn(ExperimentalForeignApi::class)
        private fun readFile(path: String): String {
            val file = fopen(path, "r") ?: throw Exception("Cannot open file: $path")
            try {
                val content = StringBuilder()
                val buffer = ByteArray(4096)
                while (true) {
                    val line = fgets(buffer.refTo(0), buffer.size, file)?.toKString()
                    if (line == null) break
                    content.append(line)
                }
                return content.toString()
            } finally {
                fclose(file)
            }
        }
    }
}

/**
 * Exception thrown when there is a configuration error,
 * see [AppConfig].
 */
class ConfigException(
    message: String,
    cause: Throwable? = null,
) : Exception(message, cause)
