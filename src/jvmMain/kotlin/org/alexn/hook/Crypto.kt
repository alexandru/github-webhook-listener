package org.alexn.hook

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

actual fun hmacSha256(data: String, key: String): String {
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(SecretKeySpec(key.toByteArray(), "HmacSHA256"))
    val bytes = mac.doFinal(data.toByteArray())
    return bytes.joinToString("") { "%02x".format(it) }
}

actual fun hmacSha1(data: String, key: String): String {
    val mac = Mac.getInstance("HmacSHA1")
    mac.init(SecretKeySpec(key.toByteArray(), "HmacSHA1"))
    val bytes = mac.doFinal(data.toByteArray())
    return bytes.joinToString("") { "%02x".format(it) }
}
