package org.alexn.hook

import cats.effect.kernel.Sync
import cats.syntax.all.given
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
    def serializeToYaml =
        io.circe.yaml.Printer(
            dropNullKeys = true,
            mappingStyle = Printer.FlowStyle.Block
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
    port: Int,
    host: Option[String],
    path: Option[String]
) derives Codec

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
        val encoder = Encoder.encodeString.contramap[FiniteDuration]: duration =>
            s"${duration.length} ${duration.unit}"
        Codec.from(decoder, encoder)

class ConfigException(msg: String, cause: Exception)
    extends RuntimeException(msg, cause)
