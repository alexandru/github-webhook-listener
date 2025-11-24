package org.alexn.hook

import com.soywiz.krypto.HMAC
import com.soywiz.krypto.encoding.Hex

actual fun hmacSha256(data: String, key: String): String {
    val hmac = HMAC.hmacSHA256(
        key.encodeToByteArray(),
        data.encodeToByteArray()
    )
    return Hex.encode(hmac).lowercase()
}

actual fun hmacSha1(data: String, key: String): String {
    val hmac = HMAC.hmacSHA1(
        key.encodeToByteArray(),
        data.encodeToByteArray()
    )
    return Hex.encode(hmac).lowercase()
}
