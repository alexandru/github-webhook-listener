package org.alexn.hook

import kotlinx.cinterop.*
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.IO
import kotlinx.coroutines.withContext
import platform.posix.*

data class CommandResult(
    val exitCode: Int,
    val stdout: String,
    val stderr: String,
) {
    val isSuccessful get() = exitCode == 0
}

/**
 * Executes shell commands using native POSIX APIs.
 */
@OptIn(ExperimentalForeignApi::class)
suspend fun executeRawShellCommand(
    command: String,
    dir: String? = null,
): CommandResult =
    withContext(Dispatchers.IO) {
        // Use popen to execute command
        val fullCommand = if (dir != null) {
            "cd '$dir' && $command"
        } else {
            command
        }
        
        executeCommand(fullCommand)
    }

@OptIn(ExperimentalForeignApi::class)
private fun executeCommand(command: String): CommandResult {
    val stdout = StringBuilder()
    val stderr = StringBuilder()
    
    // Execute command and capture stdout
    val pipe = popen(command, "r")
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
                stderr = stderr.toString(),
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

val USER_HOME: String? by lazy {
    getenv("HOME")?.toKString() ?: getenv("USERPROFILE")?.toKString()
}
