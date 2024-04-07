package org.alexn.hook

import com.typesafe.config.ConfigValue
import com.typesafe.config.ConfigValueFactory
import pureconfig.ConfigReader
import pureconfig.ConfigWriter
import pureconfig.generic.derivation.default.*
import scala.jdk.CollectionConverters.*

final case class AppConfig(
    http: HttpConfig
) derives ConfigReader

object AppConfig:
    given ConfigWriter[AppConfig] = new ConfigWriter[AppConfig]:
        override def to(a: AppConfig): ConfigValue =
            ConfigValueFactory.fromMap(
                Map(
                    "http" -> ConfigWriter[HttpConfig].to(a.http)
                ).asJava
            )

final case class HttpConfig(
    host: String,
    port: Int
)

object HttpConfig:
    given ConfigWriter[HttpConfig] = new ConfigWriter[HttpConfig]:
        override def to(a: HttpConfig): ConfigValue =
            ConfigValueFactory.fromMap(
                Map(
                    "host" -> a.host,
                    "port" -> a.port
                ).asJava
            )
