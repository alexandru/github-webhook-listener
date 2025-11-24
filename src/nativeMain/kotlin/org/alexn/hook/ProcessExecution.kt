package org.alexn.hook

import kotlinx.cinterop.*
import platform.posix.*

@OptIn(ExperimentalForeignApi::class)
actual fun executeCommand(command: String, dir: File?): CommandResult {
    val fullCommand = if (dir != null) {
        "cd '${dir.path}' && $command"
    } else {
        command
    }
    
    val stdout = StringBuilder()
    
    // Execute command and capture stdout
    val pipe = popen(fullCommand, "r")
    if (pipe != null) {
        try {
            val buffer = ByteArray(4096)
            while (true) {
                val result = fgets(buffer.refTo(0), buffer.size, pipe)?.toKString()
                if (result == null) break
                stdout.append(result)
            }
        } finally {
            val exitCode = pclose(pipe)
            // pclose returns the exit status
            val actualExitCode = if (exitCode == -1) 1 else WEXITSTATUS(exitCode)
            return CommandResult(
                exitCode = actualExitCode,
                stdout = stdout.toString(),
                stderr = "",
            )
        }
    }
    
    return CommandResult(
        exitCode = 1,
        stdout = "",
        stderr = "Failed to execute command",
    )
}

// Helper function to extract exit status from pclose result
private fun WEXITSTATUS(status: Int): Int {
    return (status shr 8) and 0xFF
}

@OptIn(ExperimentalForeignApi::class)
actual fun getUserHomeDir(): String? {
    return getenv("HOME")?.toKString() ?: getenv("USERPROFILE")?.toKString()
}
