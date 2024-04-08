package org.alexn.hook

import cats.effect.kernel.Sync
import cats.syntax.all.given
import com.comcast.ip4s.{Host, Port}
import io.circe.generic.semiauto.*
import io.circe.yaml.{Printer, parser}
import io.circe.{Codec, Decoder, Encoder}
import io.circe.syntax.*

import java.io.File
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

final case class AppConfig(
    http: HttpConfig,
    projects: Map[String, ProjectConfig]
) derives Codec:
    def serializeToYaml(spaces: Int = 2, dropNullKeys: Boolean = true) =
        io.circe.yaml.Printer(
            dropNullKeys = dropNullKeys,
            mappingStyle = Printer.FlowStyle.Block,
            indent = spaces
        ).pretty(this.asJson)

object AppConfig:
    def parseYaml(yaml: String) =
        parser.parse(yaml).flatMap(_.as[AppConfig])
            .leftMap(e => ConfigException("Failed to parse YAML app config", e))

    def loadYaml[F[_]](path: File)(using Sync[F]) =
        Sync[F].delay:
            val source = scala.io.Source.fromFile(path)
            try
                parseYaml(source.mkString).leftMap: e =>
                    ConfigException(s"Failed to load YAML app config from $path", e)
            finally
                source.close()

final case class HttpConfig(
    port: Port,
    host: Option[Host],
    path: Option[String]
) derives Codec

object HttpConfig:
    given Codec[Port] =
        Codec.from(
            Decoder[Int].emap(Port.fromInt(_).toRight("Invalid port")),
            Encoder[Int].contramap(_.value)
        )

    given Codec[Host] =
        Codec.from(
            Decoder[String].emap(Host.fromString(_).toRight("Invalid host")),
            Encoder[String].contramap(_.toString)
        )

final case class ProjectConfig(
    ref: String,
    directory: String,
    command: String,
    secret: String,
    action: Option[String],
    timeout: Option[FiniteDuration]
) derives Codec

object ProjectConfig:
    given Codec[FiniteDuration] =
        val ExtractWithUnit = """^(\d+)\\s*([a-zA-Z]+)$""".r
        val decoder = Decoder.decodeString.emap:
            case ExtractWithUnit(value, unit) =>
                try Right(FiniteDuration(value.toLong, unit))
                catch case NonFatal(e) => Left("Invalid duration")
            case other =>
                other.toIntOption match
                case Some(value) => Right(FiniteDuration(value, TimeUnit.MILLISECONDS))
                case None => Left("Invalid duration")
        .orElse(Decoder[java.time.Duration].map: d =>
            FiniteDuration(d.toMillis, TimeUnit.MILLISECONDS))
        val encoder = Encoder.encodeString.contramap[FiniteDuration]: duration =>
            s"${duration.length} ${duration.unit}"
        Codec.from(decoder, encoder)

class ConfigException(msg: String, cause: Exception)
    extends RuntimeException(msg, cause)
