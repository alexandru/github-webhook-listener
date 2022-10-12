///usr/bin/env jbang "$0" "$@" ; exit $?
//JAVA 17+
//KOTLIN 1.7.20
//DEPS org.apache.commons:commons-text:1.9
//DEPS commons-codec:commons-codec:1.15
//DEPS org.jetbrains.kotlinx:kotlinx-coroutines-core:1.6.4
//DEPS io.ktor:ktor-client-core-jvm:2.1.2
//DEPS io.ktor:ktor-client-cio-jvm:2.1.2

import kotlinx.coroutines.*
import io.ktor.client.*
import io.ktor.client.engine.cio.*
import io.ktor.client.request.*
import io.ktor.client.statement.*
import io.ktor.http.*
import org.apache.commons.codec.digest.HmacAlgorithms
import org.apache.commons.codec.digest.HmacUtils

fun main() = runBlocking {
    val signKey = "xxxxxxxxxxxxxxxxxxxxxxxxxx"
    val bodyText = """
        {
            "action": "push",
            "ref": "refs/heads/gh-pages"
        }
    """.trimIndent()

    val client = HttpClient(CIO)
    val response1 =
        client.post("http://localhost:8080/myproject") {
            headers {
                append(HttpHeaders.ContentType, "application/json")
                append(
                    "X-Hub-Signature-256",
                    "sha256=" + HmacUtils(HmacAlgorithms.HMAC_SHA_256, signKey).hmacHex(bodyText)
                )
            }
            setBody(bodyText)
        }
    println("HTTP ${response1.status}: ${response1.bodyAsText()}")
    //--------
    val response2 =
        client.post("http://localhost:8080/myproject") {
            headers {
                append(HttpHeaders.ContentType, "application/json")
                append(
                    "X-Hub-Signature",
                    "sha1=" + HmacUtils(HmacAlgorithms.HMAC_SHA_1, signKey).hmacHex(bodyText)
                )
            }
            setBody(bodyText)
        }
    println("HTTP ${response2.status}: ${response2.bodyAsText()}")
    //--------
    val response3 =
        client.post("http://localhost:8080/notAvailable") {
            headers {
                append(HttpHeaders.ContentType, "application/json")
                append(
                    "X-Hub-Signature",
                    "sha1=" + HmacUtils(HmacAlgorithms.HMAC_SHA_1, signKey).hmacHex(bodyText)
                )
            }
            setBody(bodyText)
        }
    println("HTTP ${response3.status}: ${response3.bodyAsText()}")
}
