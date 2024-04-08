package org.alexn.hook

import cats.syntax.all.*
import io.circe.Codec
import io.circe.parser.*
import org.apache.commons.codec.digest.{HmacAlgorithms, HmacUtils}

import java.net.URLDecoder
import java.nio.charset.StandardCharsets.UTF_8

final case class EventPayload(
    action: Option[String],
    ref: Option[String]
) derives Codec:
    def shouldProcess(prj: ProjectConfig): Boolean =
        action == prj.action && ref.contains(prj.ref)

object EventPayload:
    def authenticateRequest(
        body: String,
        signatureKey: String,
        signatureHeader: Option[String]
    ): Either[RequestError.Forbidden, Unit] =
        signatureHeader match
        case None =>
            Left(RequestError.Forbidden("No signature header was provided"))
        case Some(header) =>
            val sha1Prefix = "sha1="
            val sha256Prefix = "sha256="

            if header.startsWith(sha256Prefix) then
                val hmacHex = new HmacUtils(HmacAlgorithms.HMAC_SHA_256, signatureKey).hmacHex(body)
                if !header.substring(sha256Prefix.length).equalsIgnoreCase(hmacHex) then
                    Left(RequestError.Forbidden("Invalid checksum (sha256)"))
                else
                    Right(())
            else if header.startsWith(sha1Prefix) then
                val hmacHex = new HmacUtils(HmacAlgorithms.HMAC_SHA_1, signatureKey).hmacHex(body)
                if !header.substring(sha1Prefix.length).equalsIgnoreCase(hmacHex) then
                    Left(RequestError.Forbidden("Invalid checksum (sha1)"))
                else
                    Right(())
            else
                Left(RequestError.Forbidden("Unsupported algorithm"))

    def parse(ct: ContentType, body: String): Either[RequestError, EventPayload] =
        ct match
        case ContentType.Json => parseJson(body)
        case ContentType.Form => parseFormData(body)
        case ContentType.Other(other) =>
            Left(RequestError.UnsupportedMediaType(s"Cannot process `$other` media type"))

    def parseJson(json: String): Either[RequestError, EventPayload] =
        decode[EventPayload](json) match
        case Right(payload) => Right(payload)
        case Left(error) => Left(RequestError.BadInput("Invalid JSON", error))

    def parseFormData(body: String): Either[RequestError, EventPayload] =
        body.split("&").toList.traverse: part =>
            part.split("=").map(URLDecoder.decode(_, UTF_8)).toList match
            case key :: Nil => Right(None)
            case key :: value :: Nil => Right(Some(key -> value))
            case _ => Left(RequestError.BadInput(s"Invalid form-urlencoded data part: $part"))
        .map: list =>
            val map = list.flatten.toMap
            EventPayload(
                action = map.get("action"),
                ref = map.get("ref")
            )
end EventPayload

enum ContentType(val value: String):
    case Json extends ContentType("application/json")
    case Form extends ContentType("application/x-www-form-urlencoded")
    case Other(override val value: String) extends ContentType(value)

    def apply(value: String): ContentType =
        if value.startsWith(Json.value) then Json
        else if value.startsWith(Form.value) then Form
        else Other(value)

enum RequestError(val httpCode: Int, val message: String):
    case BadInput(override val message: String, cause: Throwable | Null = null)
        extends RequestError(400, message)
    case Forbidden(override val message: String)
        extends RequestError(403, message)
    case NotFound(override val message: String)
        extends RequestError(404, message)
    case Skipped(override val message: String)
        extends RequestError(200, message)
    case TimedOut(override val message: String)
        extends RequestError(408, message)
    case UnsupportedMediaType(override val message: String)
        extends RequestError(415, message)
    case InternalServerError(
        override val message: String,
        cause: Throwable | Null = null,
        meta: Map[String, String] | Null = null
    ) extends RequestError(500, message)

    def toException: Exception = this match
    case BadInput(message, exception) =>
        RequestException(s"$httpCode Bad Input — $message", exception)
    case Forbidden(message) =>
        RequestException(s"$httpCode Forbidden — $message", null)
    case InternalServerError(message, exception, meta) =>
        val metaStr =
            (meta ?: Map.empty).map:
                case (k, v) => s"\n  $k:$v"
            .mkString("")
        RequestException(s"$httpCode Internal Server Error — $message$metaStr", exception)
    case NotFound(message) =>
        RequestException(s"$httpCode Not Found — $message", null)
    case Skipped(message) =>
        RequestException(s"$httpCode Skipped — $message", null)
    case TimedOut(message) =>
        RequestException(s"$httpCode Timed out — $message", null)
    case UnsupportedMediaType(message) =>
        RequestException(s"$httpCode Unsupported Media Type — $message", null)
end RequestError

final class RequestException(message: String, cause: Throwable | Null = null)
    extends RuntimeException(message, cause)
