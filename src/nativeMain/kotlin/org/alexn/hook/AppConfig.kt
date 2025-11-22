@file:OptIn(ExperimentalSerializationApi::class)

package org.alexn.hook

import kotlinx.cinterop.ExperimentalForeignApi
import kotlinx.cinterop.toKString
import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
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
        fun parseFile(filePath: String): Result<AppConfig> {
            val extension = filePath.substringAfterLast('.', "").lowercase()
            
            val content = try {
                readFile(filePath)
            } catch (ex: Exception) {
                return Result.Error(
                    ConfigException(
                        "Failed to read configuration file: $filePath",
                        ex,
                    )
                )
            }

            return when (extension) {
                "json" -> parseJson(content)
                "yaml", "yml" -> {
                    // For now, we'll convert simple YAML to JSON
                    // Full YAML parsing would require a native YAML library
                    parseSimpleYaml(content)
                }
                else ->
                    Result.Error(
                        ConfigException(
                            "Unsupported configuration file format: $extension",
                        ),
                    )
            }
        }

        fun parseJson(string: String): Result<AppConfig> =
            try {
                val config = jsonParser.decodeFromString(
                    serializer(),
                    string,
                )
                Result.Success(config)
            } catch (ex: Exception) {
                Result.Error(
                    ConfigException(
                        "Failed to parse JSON configuration",
                        ex,
                    ),
                )
            }

        // Simple YAML parser for basic configurations
        // This is a simplified version that handles the basic structure
        private fun parseSimpleYaml(yaml: String): Result<AppConfig> {
            try {
                val lines = yaml.lines().filter { it.isNotBlank() && !it.trim().startsWith("#") }
                val json = buildString {
                    append("{")
                    var inHttp = false
                    var inProjects = false
                    var currentProject: String? = null
                    var indent = 0
                    
                    for ((index, line) in lines.withIndex()) {
                        val trimmed = line.trim()
                        val currentIndent = line.takeWhile { it == ' ' }.length
                        
                        when {
                            trimmed.startsWith("http:") -> {
                                if (index > 0) append(",")
                                append("\"http\":{")
                                inHttp = true
                                inProjects = false
                                currentProject = null
                            }
                            trimmed.startsWith("projects:") -> {
                                if (inHttp) append("}")
                                append(",\"projects\":{")
                                inHttp = false
                                inProjects = true
                                currentProject = null
                            }
                            inHttp && trimmed.contains(":") -> {
                                val (key, value) = trimmed.split(":", limit = 2)
                                val cleanValue = value.trim().trim('"')
                                if (trimmed != lines.first { it.contains("http:") }) append(",")
                                append("\"${key.trim()}\":${if (cleanValue.toIntOrNull() != null) cleanValue else "\"$cleanValue\""}")
                            }
                            inProjects && currentIndent == 2 && trimmed.contains(":") && !trimmed.contains("  ") -> {
                                // Project name
                                if (currentProject != null) append("}")
                                val projectName = trimmed.removeSuffix(":")
                                if (currentProject != null) append(",")
                                append("\"$projectName\":{")
                                currentProject = projectName
                            }
                            currentProject != null && trimmed.contains(":") -> {
                                // Project property
                                val (key, value) = trimmed.split(":", limit = 2)
                                val cleanValue = value.trim().trim('"')
                                if (trimmed != lines.first { it.contains("$currentProject:") }.let { lines.indexOf(it) + 1 }.let { if (it < lines.size) lines[it] else trimmed }) append(",")
                                append("\"${key.trim()}\":\"$cleanValue\"")
                            }
                        }
                    }
                    if (currentProject != null) append("}")
                    if (inProjects) append("}")
                    append("}")
                }
                
                return parseJson(json)
            } catch (ex: Exception) {
                return Result.Error(
                    ConfigException(
                        "Failed to parse YAML configuration",
                        ex,
                    ),
                )
            }
        }

        private val jsonParser =
            Json {
                isLenient = true
                ignoreUnknownKeys = true
                explicitNulls = false
            }

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

// Simple Result type to replace Arrow's Either
sealed class Result<out T> {
    data class Success<T>(val value: T) : Result<T>()
    data class Error(val exception: Exception) : Result<Nothing>()
}
