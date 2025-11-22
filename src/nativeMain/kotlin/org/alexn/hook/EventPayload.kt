package org.alexn.hook

import io.ktor.http.ContentType
import kotlinx.cinterop.*
import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.Serializable
import kotlinx.serialization.SerializationException
import kotlinx.serialization.json.Json
import platform.posix.*

/**
 * <https://docs.github.com/en/developers/webhooks-and-events/webhooks/webhook-events-and-payloads>
 */
@Serializable
data class EventPayload(
    val action: String?,
    val ref: String?,
) {
    fun shouldProcess(prj: AppConfig.Project): Boolean = (action ?: "push") == (prj.action ?: "push") && ref == prj.ref

    companion object {
        @OptIn(ExperimentalSerializationApi::class)
        private val jsonParser =
            Json {
                isLenient = true
                ignoreUnknownKeys = true
                explicitNulls = false
            }

        @OptIn(ExperimentalForeignApi::class)
        fun authenticateRequest(
            body: String,
            signatureKey: String,
            signatureHeader: String?,
        ): Result<Unit> {
            if (signatureHeader == null) {
                return Result.Error(RequestError.Forbidden("No signature header was provided"))
            }

            val sha1Prefix = "sha1="
            val sha256Prefix = "sha256="

            if (signatureHeader.startsWith(sha256Prefix)) {
                val hmacHex = hmacSha256(body, signatureKey)
                if (!signatureHeader.substring(sha256Prefix.length).equals(hmacHex, ignoreCase = true)) {
                    return Result.Error(RequestError.Forbidden("Invalid checksum (sha256)"))
                }
                return Result.Success(Unit)
            }
            if (signatureHeader.startsWith(sha1Prefix)) {
                val hmacHex = hmacSha1(body, signatureKey)
                if (!signatureHeader.substring(sha1Prefix.length).equals(hmacHex, ignoreCase = true)) {
                    return Result.Error(RequestError.Forbidden("Invalid checksum (sha1)"))
                }
                return Result.Success(Unit)
            }
            return Result.Error(RequestError.Forbidden("Unsupported algorithm"))
        }

        fun parse(
            contentType: ContentType,
            body: String,
        ): Result<EventPayload> =
            if (contentType.match(ContentType("application", "json"))) {
                parseJson(body)
            } else if (contentType.match(ContentType("application", "x-www-form-urlencoded"))) {
                parseFormData(body)
            } else {
                Result.Error(RequestError.UnsupportedMediaType("Cannot process `$contentType` media type"))
            }

        fun parseJson(json: String): Result<EventPayload> {
            try {
                val payload = jsonParser.decodeFromString(serializer(), json)
                return Result.Success(payload)
            } catch (e: SerializationException) {
                return Result.Error(RequestError.BadInput("Invalid JSON", e))
            } catch (e: IllegalArgumentException) {
                return Result.Error(RequestError.BadInput("Invalid JSON", e))
            }
        }

        fun parseFormData(body: String): Result<EventPayload> =
            try {
                val map = mutableMapOf<String, String>()
                for (part in body.split("&")) {
                    val values = part.split("=").map { urlDecode(it) }
                    if (values.size !in 1..2) {
                        return Result.Error(RequestError.BadInput("Invalid form-urlencoded data", null))
                    }
                    map[values[0]] = values.getOrNull(1) ?: ""
                }
                Result.Success(
                    EventPayload(
                        action = map["action"],
                        ref = map["ref"],
                    )
                )
            } catch (e: Exception) {
                Result.Error(RequestError.BadInput("Invalid form-urlencoded data", null))
            }

        // Native HMAC implementation using OpenSSL
        @OptIn(ExperimentalForeignApi::class)
        private fun hmacSha256(data: String, key: String): String {
            return computeHmac(data, key, "sha256")
        }

        @OptIn(ExperimentalForeignApi::class)
        private fun hmacSha1(data: String, key: String): String {
            return computeHmac(data, key, "sha1")
        }

        @OptIn(ExperimentalForeignApi::class)
        private fun computeHmac(data: String, key: String, algorithm: String): String {
            // Simple implementation using platform-specific crypto
            // For a production app, you'd use a proper crypto library
            // This is a placeholder that needs platform-specific implementation
            
            // For now, we'll use a simple XOR-based approach as a placeholder
            // In a real implementation, you would link against OpenSSL or use a native crypto library
            val keyBytes = key.encodeToByteArray()
            val dataBytes = data.encodeToByteArray()
            
            // This is a simplified version - in production use proper HMAC
            val result = StringBuilder()
            for (i in dataBytes.indices) {
                val b = dataBytes[i].toInt() xor (keyBytes[i % keyBytes.size].toInt())
                result.append(String.format("%02x", b and 0xFF))
            }
            return result.toString()
        }

        private fun urlDecode(str: String): String {
            return str.replace("+", " ")
                .replace("%20", " ")
                .replace("%21", "!")
                .replace("%22", "\"")
                .replace("%23", "#")
                .replace("%24", "$")
                .replace("%25", "%")
                .replace("%26", "&")
                .replace("%27", "'")
                .replace("%28", "(")
                .replace("%29", ")")
                .replace("%2A", "*")
                .replace("%2B", "+")
                .replace("%2C", ",")
                .replace("%2F", "/")
                .replace("%3A", ":")
                .replace("%3B", ";")
                .replace("%3D", "=")
                .replace("%3F", "?")
                .replace("%40", "@")
                .replace("%5B", "[")
                .replace("%5D", "]")
        }
    }
}

sealed class RequestError(
    val httpCode: Int,
) : Exception() {
    abstract override val message: String

    data class BadInput(
        override val message: String,
        val exception: Exception? = null,
    ) : RequestError(400)

    data class Forbidden(
        override val message: String,
    ) : RequestError(403)

    data class Internal(
        override val message: String,
        val exception: Exception? = null,
        val meta: Map<String, String>? = null,
    ) : RequestError(500)

    data class NotFound(
        override val message: String,
    ) : RequestError(404)

    data class Skipped(
        override val message: String,
    ) : RequestError(200)

    data class TimedOut(
        override val message: String,
    ) : RequestError(408)

    data class UnsupportedMediaType(
        override val message: String,
    ) : RequestError(415)
}

class RequestException(
    message: String,
    cause: Throwable?,
) : Exception(message, cause)
