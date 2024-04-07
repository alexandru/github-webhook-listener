package org.alexn.hook

import cats.effect.IO
import com.comcast.ip4s.Host
import munit.CatsEffectSuite

class AppConfigSuite extends CatsEffectSuite:
    val configText =
        """
        |http:
        |  path: "/"
        |  port: 8080
        |  host: myhost
        |
        |runtime:
        |  workers: 2
        |  output: stdout
        |
        |projects:
        |  myproject:
        |    ref: "refs/heads/gh-pages"
        |    directory: "/var/www/myproject"
        |    command: "git pull"
        |    secret: "xxxxxxxxxxxxxxxxxxxxxxxxxx"
        |""".stripMargin

    val expected = AppConfig(
        http = HttpConfig(
            path = Some("/"),
            port = Port.fromInt(8080).get,
            host = Host.fromString("myhost").get
        ),
        projects = Map(
            "myproject" -> ProjectConfig(
                ref = "refs/heads/gh-pages",
                directory = "/var/www/myproject",
                command = "git pull",
                secret = "xxxxxxxxxxxxxxxxxxxxxxxxxx",
                action = None,
                timeout = None
            )
        )
    )

    test("codec works"):
        for
            serialized <- IO(expected.serializeToYaml)
            parsed <- IO.fromEither(AppConfig.parseYaml(serialized))
        yield assertEquals(parsed, expected)

    test("load from legacy yaml"):
        for
            parsed <- IO.fromEither(AppConfig.parseYaml(configText))
        yield assertEquals(parsed, expected)
end AppConfigSuite
