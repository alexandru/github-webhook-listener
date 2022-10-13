package org.alexn.hook

import arrow.core.getOrHandle
import org.apache.commons.codec.digest.HmacAlgorithms
import org.apache.commons.codec.digest.HmacUtils
import java.io.FileNotFoundException
import java.net.URLEncoder
import java.nio.charset.StandardCharsets.UTF_8
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class EventPayloadTest {
    @Test
    fun `authenticate message with hmac-sha1`() {
        val key = "some-key"
        val json = """
        {
            "action": "some-action",
            "ref": "some-ref",
            "additional": { "field": true }
        }
        """.trimIndent()

        EventPayload
            .authenticateRequest(
                json,
                key,
                "sha1=" + HmacUtils(HmacAlgorithms.HMAC_SHA_1, key).hmacHex(json),
            )
            .getOrHandle { throw it.toException() }
    }

    @Test
    fun `authenticate message with hmac-sha256`() {
        val key = "some-key"
        val json = """
        {
            "action": "some-action",
            "ref": "some-ref",
            "additional": { "field": true }
        }
        """.trimIndent()

        EventPayload
            .authenticateRequest(
                json,
                key,
                "sha256=" + HmacUtils(HmacAlgorithms.HMAC_SHA_256, key).hmacHex(json),
            )
            .getOrHandle { throw it.toException() }
    }

    @Test
    fun `authenticate fails on unknown algorithm`() {
        val key = "some-key"
        val json = """
        {
            "action": "some-action",
            "ref": "some-ref",
            "additional": { "field": true }
        }
        """.trimIndent()

        val r = EventPayload
            .authenticateRequest(
                json,
                key,
                "sha512=" + HmacUtils(HmacAlgorithms.HMAC_SHA_512, key).hmacHex(json),
            )
        assertTrue(r.isLeft(), "Unexpected result: $r")
    }

    @Test
    fun `parse JSON`() {
        val json = """
        {
            "action": "some-action",
            "ref": "some-ref",
            "additional": { "field": true }
        }
        """.trimIndent()

        val received = EventPayload.parseJson(json).getOrHandle { throw it.toException() }
        assertEquals(
            EventPayload(
                action = "some-action",
                ref = "some-ref"
            ),
            received
        )
    }

    @Test
    fun `parse multipart-form data`() {
        val formData =
            "action=${URLEncoder.encode("some action", UTF_8)}&" +
                "ref=${URLEncoder.encode("some ref", UTF_8)}"
        val received = EventPayload.parseFormData(formData).getOrHandle { throw it.toException() }
        assertEquals(
            EventPayload(
                action = "some action",
                ref = "some ref"
            ),
            received
        )
    }

    @Test
    fun `parse fails on invalid JSON`() {
        val yaml = """
        action: some-action
        ref: some-ref
        additional:
            field: true
        """.trimIndent()

        val r = EventPayload.parseJson(yaml)
        assertTrue(r.isLeft(), "Unexpected result: $r")
    }

    @Test
    fun `parse real payload`() {
        val json =
            javaClass.getResourceAsStream("/real-payload.json")?.readAllBytes()?.toString(UTF_8)
                ?: throw FileNotFoundException("/resources/real-payload.json")

        val parsed = EventPayload
            .parseJson(json)
            .getOrHandle { throw it.toException() }

        assertEquals(
            parsed,
            EventPayload(
                action = null,
                ref = "refs/heads/gh-pages",
            )
        )
    }
}
