package org.alexn.hook

import java.io.File as JFile
import java.nio.charset.StandardCharsets.UTF_8

actual fun executeCommand(command: String, dir: File?): CommandResult {
    val fullCommand = if (dir != null) {
        "cd '${dir.path}' && $command"
    } else {
        command
    }
    
    val proc = Runtime.getRuntime().exec(
        arrayOf("/bin/sh", "-c", fullCommand),
        arrayOf(),
        null
    )
    
    try {
        val stdout = String(proc.inputStream.readAllBytes(), UTF_8)
        val stderr = String(proc.errorStream.readAllBytes(), UTF_8)
        val exitCode = proc.waitFor()
        
        return CommandResult(
            exitCode = exitCode,
            stdout = stdout,
            stderr = stderr,
        )
    } finally {
        proc.destroy()
    }
}

actual fun getUserHomeDir(): String? {
    return System.getProperty("user.home") ?: System.getenv("HOME")
}
