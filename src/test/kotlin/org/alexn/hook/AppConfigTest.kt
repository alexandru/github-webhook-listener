@file:OptIn(ExperimentalSerializationApi::class, ExperimentalSerializationApi::class)

package org.alexn.hook

import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.decodeFromString
import kotlinx.serialization.encodeToString
import kotlinx.serialization.hocon.Hocon
import kotlinx.serialization.hocon.decodeFromConfig
import kotlinx.serialization.hocon.encodeToConfig
import kotlinx.serialization.json.Json
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.time.Duration.Companion.seconds

class AppConfigTest {
    val expected = AppConfig(
        http = AppConfig.Http(
            host = "0.0.0.0",
            port = 8080
        ),
        projects = mapOf(
            "monix" to AppConfig.Project(
                action = null,
                ref = "refs/heads/gh-pages",
                directory = "/var/www/myproject",
                command = "git pull",
                timeout = 3.seconds,
                secret = "xxxxx"
            )
        )
    )

    @Test
    fun jsonCodecWorks() {
        val encoded = Json.encodeToString(expected)
        val received = Json.decodeFromString<AppConfig>(encoded)
        assertEquals(expected, received)
    }

    @Test
    fun hoconCodecWorks() {
        val encoded = Hocon.encodeToConfig(expected)
        val received = Hocon.decodeFromConfig<AppConfig>(encoded)
        assertEquals(expected, received)
    }

    @Test
    fun parseFromHoconString() {
        val config = """
        http {
            host = "0.0.0.0"
            port = 8080
        }

        projects {
            monix {
                ref: "refs/heads/gh-pages"
                directory: "/var/www/myproject"
                command: "git pull"
                timeout: "PT3S"
                secret: "xxxxx"
            }
        }
        """.trimIndent()

        assertEquals(
            expected,
            AppConfig.parseHocon(config)
        )
    }

    @Test
    fun parseLegacyYamlConfig() {
        val config = """
        http:
          path: "/"
          port: 8080

        runtime:
          workers: 2
          output: stdout

        projects:
          myproject:
            ref: "refs/heads/gh-pages"
            directory: "/var/www/myproject"
            command: "git pull"
            secret: "xxxxxxxxxxxxxxxxxxxxxxxxxx"
        """.trimIndent()

        assertEquals(
            AppConfig(
                http = AppConfig.Http(
                    host = null,
                    port = 8080,
                    path = "/"
                ),
                projects = mapOf(
                    "myproject" to AppConfig.Project(
                        action = null,
                        ref = "refs/heads/gh-pages",
                        directory = "/var/www/myproject",
                        command = "git pull",
                        timeout = null,
                        secret = "xxxxxxxxxxxxxxxxxxxxxxxxxx"
                    )
                )
            ),
            AppConfig.parseYaml(config)
        )
    }
}
