package org.alexn.hook

import arrow.core.Option
import arrow.core.recover
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
    dir: File? = null,
): CommandResult =
    withContext(Dispatchers.IO) {
        val fullCommand = if (dir != null) {
            "cd '${dir.path}' && $command"
        } else {
            command
        }
        
        executeCommand(fullCommand)
    }

@OptIn(ExperimentalForeignApi::class)
private fun executeCommand(command: String): CommandResult {
    val stdout = StringBuilder()
    
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

// File abstraction for native
data class File(val path: String)

val USER_HOME: File? by lazy {
    Option
        .fromNullable(getenv("HOME")?.toKString())
        .filter { it.isNotEmpty() }
        .recover { Option.fromNullable(getenv("USERPROFILE")?.toKString()).bind() }
        .filter { it.isNotEmpty() }
        .map { File(it) }
        .getOrNull()
}
                dir,
            )
        try {
            // Concurrent execution ensures the stream's buffer doesn't
            // block processing when overflowing
            val stdout =
                async {
                    runInterruptible(Dispatchers.IO) {
                        // That `InputStream.read` doesn't listen to thread interruption
                        // signals; but for future development it doesn't hurt
                        String(proc.inputStream.readAllBytes(), UTF_8)
                    }
                }
            val stderr =
                async {
                    runInterruptible(Dispatchers.IO) {
                        String(proc.errorStream.readAllBytes(), UTF_8)
                    }
                }
            CommandResult(
                exitCode = runInterruptible(Dispatchers.IO) { proc.waitFor() },
                stdout = stdout.await(),
                stderr = stderr.await(),
            )
        } finally {
            proc.destroy()
        }
    }

/**
 * Executes shell commands.
 *
 * This version does shell escaping of command arguments.
 * WARN: command arguments need be given explicitly because
 * they need to be properly escaped.

 * @see [executeRawShellCommand]
 */
suspend fun executeEscapedShellCommand(
    command: String,
    args: List<String>? = null,
    dir: File? = null,
): CommandResult =
    executeRawShellCommand(
        command =
            (listOf(command) + (args ?: listOf()))
                .map(StringEscapeUtils::escapeXSI)
                .joinToString(" "),
        dir = dir,
    )

/**
 * Executes shell commands.
 */
suspend fun executeRawShellCommand(
    command: String,
    dir: File? = null,
): CommandResult =
    executeCommand(
        executable = Path.of("/bin/sh"),
        args = listOf("-c", command),
        dir = dir,
    )

val USER_HOME: File? by lazy {
    Option
        .fromNullable(System.getProperty("user.home"))
        .filter { it.isNotEmpty() }
        .recover { Option.fromNullable(System.getenv("HOME")).bind() }
        .filter { it.isNotEmpty() }
        .map { File(it) }
        .getOrNull()
}
