package org.alexn.hook

import java.io.File

actual fun readFileContent(path: String): String {
    return File(path).readText()
}
