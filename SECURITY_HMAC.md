# Security: HMAC Implementation for Kotlin/Native

## Current Status

⚠️ **WARNING**: The current HMAC implementation in `EventPayload.kt` uses a simple XOR approach and is **NOT cryptographically secure**. It is a placeholder for demonstration purposes only.

## Why This Needs to Be Fixed

GitHub webhooks use HMAC-SHA256 or HMAC-SHA1 to sign payloads. A proper cryptographic HMAC implementation is required to:
1. Verify webhook authenticity
2. Prevent replay attacks
3. Ensure message integrity

The current XOR-based approach does not provide any of these guarantees.

## Recommended Solutions

### Option 1: Use KCrypto (Recommended)

KCrypto is a Kotlin Multiplatform cryptography library that supports Kotlin/Native.

**Dependencies:**
```kotlin
// In settings.gradle.kts version catalog
version("kcrypto", "5.4.0")
library("kcrypto", "com.soywiz.korlibs.krypto", "krypto")
    .versionRef("kcrypto")

// In build.gradle.kts
dependencies {
    implementation(libs.kcrypto)
}
```

**Implementation:**
```kotlin
import com.soywiz.krypto.encoding.hex
import com.soywiz.krypto.encoding.Hex
import com.soywiz.krypto.SHA256
import com.soywiz.krypto.HMAC

private fun hmacSha256(data: String, key: String): String {
    val keyBytes = key.encodeToByteArray()
    val dataBytes = data.encodeToByteArray()
    val hmac = HMAC.hmacSHA256(keyBytes, dataBytes)
    return Hex.encode(hmac).lowercase()
}

private fun hmacSha1(data: String, key: String): String {
    val keyBytes = key.encodeToByteArray()
    val dataBytes = data.encodeToByteArray()
    val hmac = HMAC.hmacSHA1(keyBytes, dataBytes)
    return Hex.encode(hmac).lowercase()
}
```

### Option 2: OpenSSL Interop (More Complex)

Use Kotlin/Native's C interop to call OpenSSL directly.

**Create def file** (`src/nativeInterop/cinterop/openssl.def`):
```def
headers = openssl/evp.h openssl/hmac.h
headerFilter = openssl/*
compilerOpts.linux = -I/usr/include
linkerOpts.linux = -L/usr/lib/x86_64-linux-gnu -lssl -lcrypto
```

**Build configuration:**
```kotlin
kotlin {
    linuxX64("native") {
        compilations.getByName("main") {
            cinterops {
                val openssl by creating
            }
        }
    }
}
```

**Implementation:**
```kotlin
import kotlinx.cinterop.*
import openssl.*

@OptIn(ExperimentalForeignApi::class)
private fun hmacSha256(data: String, key: String): String {
    val keyBytes = key.encodeToByteArray()
    val dataBytes = data.encodeToByteArray()
    
    return keyBytes.usePinned { keyPin ->
        dataBytes.usePinned { dataPin ->
            val result = ByteArray(32) // SHA256 produces 32 bytes
            result.usePinned { resultPin ->
                HMAC(
                    EVP_sha256(),
                    keyPin.addressOf(0),
                    keyBytes.size,
                    dataPin.addressOf(0).reinterpret(),
                    dataBytes.size.toULong(),
                    resultPin.addressOf(0).reinterpret(),
                    null
                )
                
                // Convert to hex string
                result.joinToString("") { "%02x".format(it.toInt() and 0xFF) }
            }
        }
    }
}
```

### Option 3: Pure Kotlin Implementation

Implement HMAC-SHA256 in pure Kotlin. This is the most portable but also most complex.

**Note**: This requires implementing SHA256 from scratch or using a library like `kotlin-crypto` which may have native support.

## Migration Steps

### Step 1: Add Dependency

Choose Option 1 (KCrypto) and add to `settings.gradle.kts`:

```kotlin
version("kcrypto", "5.4.0")
library("kcrypto", "com.soywiz.korlibs.krypto", "krypto")
    .versionRef("kcrypto")
```

And to `build.gradle.kts`:

```kotlin
sourceSets {
    val nativeMain by getting {
        dependencies {
            // ... existing dependencies ...
            implementation("com.soywiz.korlibs.krypto:krypto:5.4.0")
        }
    }
}
```

### Step 2: Replace Implementation

In `src/nativeMain/kotlin/org/alexn/hook/EventPayload.kt`, replace the `computeHmac`, `hmacSha256`, and `hmacSha1` functions:

```kotlin
import com.soywiz.krypto.HMAC
import com.soywiz.krypto.encoding.Hex

@OptIn(ExperimentalForeignApi::class)
private fun hmacSha256(data: String, key: String): String {
    val hmac = HMAC.hmacSHA256(
        key.encodeToByteArray(),
        data.encodeToByteArray()
    )
    return Hex.encode(hmac).lowercase()
}

@OptIn(ExperimentalForeignApi::class)
private fun hmacSha1(data: String, key: String): String {
    val hmac = HMAC.hmacSHA1(
        key.encodeToByteArray(),
        data.encodeToByteArray()
    )
    return Hex.encode(hmac).lowercase()
}

// Remove the computeHmac function as it's no longer needed
```

### Step 3: Test

Create a test to verify HMAC correctness:

```kotlin
@Test
fun testHmacSha256() {
    val key = "my-secret-key"
    val data = "test-data"
    
    // Expected value computed with: echo -n "test-data" | openssl dgst -sha256 -hmac "my-secret-key"
    val expected = "..." // Add expected hex value
    
    val result = EventPayload.Companion.hmacSha256(data, key)
    assertEquals(expected, result)
}
```

### Step 4: Verify with GitHub

Test with actual GitHub webhook payloads to ensure signature verification works correctly.

## Security Checklist

Before deploying to production:

- [ ] Replace placeholder HMAC implementation
- [ ] Add comprehensive HMAC tests
- [ ] Test with real GitHub webhook signatures
- [ ] Verify both SHA256 and SHA1 signatures work
- [ ] Add timing-safe comparison for signatures
- [ ] Document the crypto library version used
- [ ] Security audit of the implementation

## Timing-Safe Comparison

Also implement constant-time comparison to prevent timing attacks:

```kotlin
private fun constantTimeEquals(a: String, b: String): Boolean {
    if (a.length != b.length) return false
    
    var result = 0
    for (i in a.indices) {
        result = result or (a[i].code xor b[i].code)
    }
    return result == 0
}

// Use in authenticateRequest:
if (!constantTimeEquals(signatureHeader.substring(sha256Prefix.length), hmacHex)) {
    return Result.Error(RequestError.Forbidden("Invalid checksum (sha256)"))
}
```

## Resources

- [KCrypto GitHub](https://github.com/korlibs/krypto)
- [GitHub Webhook Security](https://docs.github.com/en/developers/webhooks-and-events/webhooks/securing-your-webhooks)
- [Kotlin/Native C Interop](https://kotlinlang.org/docs/native-c-interop.html)
- [HMAC RFC 2104](https://www.ietf.org/rfc/rfc2104.txt)
