package org.alexn.hook

import kotlinx.cinterop.ExperimentalForeignApi
import kotlinx.cinterop.toKString
import platform.posix.fclose
import platform.posix.fgets
import platform.posix.fopen

@OptIn(ExperimentalForeignApi::class)
actual fun readFileContent(path: String): String {
    val file = fopen(path, "r") ?: throw Exception("Cannot open file: $path")
    try {
        val content = StringBuilder()
        val buffer = ByteArray(4096)
        while (true) {
            val line = fgets(buffer.refTo(0), buffer.size, file)?.toKString()
            if (line == null) break
            content.append(line)
        }
        return content.toString()
    } finally {
        fclose(file)
    }
}
