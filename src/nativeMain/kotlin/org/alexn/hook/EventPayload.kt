package org.alexn.hook

import arrow.core.Either
import arrow.core.left
import arrow.core.right
import com.soywiz.krypto.HMAC
import com.soywiz.krypto.encoding.Hex
import io.ktor.http.ContentType
import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.Serializable
import kotlinx.serialization.SerializationException
import kotlinx.serialization.json.Json

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

        fun authenticateRequest(
            body: String,
            signatureKey: String,
            signatureHeader: String?,
        ): Either<RequestError.Forbidden, Unit> {
            if (signatureHeader == null) {
                return RequestError.Forbidden("No signature header was provided").left()
            }

            val sha1Prefix = "sha1="
            val sha256Prefix = "sha256="

            if (signatureHeader.startsWith(sha256Prefix)) {
                val hmacHex = hmacSha256(body, signatureKey)
                if (!signatureHeader.substring(sha256Prefix.length).equals(hmacHex, ignoreCase = true)) {
                    return RequestError.Forbidden("Invalid checksum (sha256)").left()
                }
                return Unit.right()
            }
            if (signatureHeader.startsWith(sha1Prefix)) {
                val hmacHex = hmacSha1(body, signatureKey)
                if (!signatureHeader.substring(sha1Prefix.length).equals(hmacHex, ignoreCase = true)) {
                    return RequestError.Forbidden("Invalid checksum (sha1)").left()
                }
                return Unit.right()
            }
            return RequestError.Forbidden("Unsupported algorithm").left()
        }

        fun parse(
            contentType: ContentType,
            body: String,
        ): Either<RequestError, EventPayload> =
            if (contentType.match(ContentType("application", "json"))) {
                parseJson(body)
            } else if (contentType.match(ContentType("application", "x-www-form-urlencoded"))) {
                parseFormData(body)
            } else {
                RequestError.UnsupportedMediaType("Cannot process `$contentType` media type").left()
            }

        fun parseJson(json: String): Either<RequestError.BadInput, EventPayload> {
            try {
                val payload = jsonParser.decodeFromString(serializer(), json)
                return payload.right()
            } catch (e: SerializationException) {
                return RequestError.BadInput("Invalid JSON", e).left()
            } catch (e: IllegalArgumentException) {
                return RequestError.BadInput("Invalid JSON", e).left()
            }
        }

        fun parseFormData(body: String): Either<RequestError.BadInput, EventPayload> =
            try {
                val map = mutableMapOf<String, String>()
                for (part in body.split("&")) {
                    val values = part.split("=").map { urlDecode(it) }
                    if (values.size !in 1..2) {
                        return RequestError.BadInput("Invalid form-urlencoded data", null).left()
                    }
                    map[values[0]] = values.getOrNull(1) ?: ""
                }
                EventPayload(
                    action = map["action"],
                    ref = map["ref"],
                ).right()
            } catch (e: Exception) {
                RequestError.BadInput("Invalid form-urlencoded data", null).left()
            }

        // HMAC using KCrypto library with native support
        private fun hmacSha256(data: String, key: String): String {
            val hmac = HMAC.hmacSHA256(
                key.encodeToByteArray(),
                data.encodeToByteArray()
            )
            return Hex.encode(hmac).lowercase()
        }

        private fun hmacSha1(data: String, key: String): String {
            val hmac = HMAC.hmacSHA1(
                key.encodeToByteArray(),
                data.encodeToByteArray()
            )
            return Hex.encode(hmac).lowercase()
        }

        // Simple URL decoding for native
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
                }
                return Unit.right()
            }
            if (signatureHeader.startsWith(sha1Prefix)) {
                val hmacHex = HmacUtils(HmacAlgorithms.HMAC_SHA_1, signatureKey).hmacHex(body)
                if (!signatureHeader.substring(sha1Prefix.length).equals(hmacHex, ignoreCase = true)) {
                    return RequestError.Forbidden("Invalid checksum (sha1)").left()
                }
                return Unit.right()
            }
            return RequestError.Forbidden("Unsupported algorithm").left()
        }

        fun parse(
            contentType: ContentType,
            body: String,
        ): Either<RequestError, EventPayload> =
            if (contentType.match(ContentType("application", "json"))) {
                parseJson(body)
            } else if (contentType.match(ContentType("application", "x-www-form-urlencoded"))) {
                parseFormData(body)
            } else {
                RequestError.UnsupportedMediaType("Cannot process `$contentType` media type").left()
            }

        fun parseJson(json: String): Either<RequestError.BadInput, EventPayload> {
            try {
                val payload = jsonParser.decodeFromString(serializer(), json)
                return payload.right()
            } catch (e: SerializationException) {
                return RequestError.BadInput("Invalid JSON", e).left()
            } catch (e: IllegalArgumentException) {
                return RequestError.BadInput("Invalid JSON", e).left()
            }
        }

        fun parseFormData(body: String): Either<RequestError.BadInput, EventPayload> =
            try {
                val map = mutableMapOf<String, String>()
                for (part in body.split("&")) {
                    val values = part.split("=").map { URLDecoder.decode(it, UTF_8) }
                    assert(values.size in 1..2)
                    map[values[0]] = values[1] ?: ""
                }
                EventPayload(
                    action = map["action"],
                    ref = map["ref"],
                ).right()
            } catch (e: AssertionError) {
                RequestError.BadInput("Invalid form-urlencoded data", null).left()
            }
    }
}

sealed class RequestError(
    val httpCode: Int,
) {
    abstract val message: String

    fun toException(): Exception =
        when (this) {
            is BadInput ->
                RequestException("$httpCode Bad Input — $message", exception)
            is Forbidden ->
                RequestException("$httpCode Forbidden — $message", null)
            is Internal -> {
                val metaStr = (meta ?: mapOf()).map { "\n  ${it.key}:${it.value}" }.joinToString("")
                RequestException("$httpCode Internal Server Error — $message$metaStr", exception)
            }
            is NotFound ->
                RequestException("$httpCode Not Found — $message", null)
            is Skipped ->
                RequestException("$httpCode Skipped — $message", null)
            is TimedOut ->
                RequestException("$httpCode Timed out — $message", null)
            is UnsupportedMediaType ->
                RequestException("$httpCode Unsupported Media Type — $message", null)
        }

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
    ) : RequestError(
            500,
        )

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
) : java.lang.Exception(message, cause)

sealed class RequestError(
    val httpCode: Int,
) {
    abstract val message: String

    fun toException(): Exception =
        when (this) {
            is BadInput ->
                RequestException("$httpCode Bad Input — $message", exception)
            is Forbidden ->
                RequestException("$httpCode Forbidden — $message", null)
            is Internal -> {
                val metaStr = (meta ?: mapOf()).map { "\n  ${it.key}:${it.value}" }.joinToString("")
                RequestException("$httpCode Internal Server Error — $message$metaStr", exception)
            }
            is NotFound ->
                RequestException("$httpCode Not Found — $message", null)
            is Skipped ->
                RequestException("$httpCode Skipped — $message", null)
            is TimedOut ->
                RequestException("$httpCode Timed out — $message", null)
            is UnsupportedMediaType ->
                RequestException("$httpCode Unsupported Media Type — $message", null)
        }

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
    ) : RequestError(
            500,
        )

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
