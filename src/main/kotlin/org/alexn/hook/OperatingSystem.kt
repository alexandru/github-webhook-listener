package org.alexn.hook

import arrow.core.Option
import arrow.core.recover
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.async
import kotlinx.coroutines.runInterruptible
import kotlinx.coroutines.withContext
import org.apache.commons.text.StringEscapeUtils
import java.io.File
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path

data class CommandResult(
    val exitCode: Int,
    val stdout: String,
    val stderr: String,
) {
    val isSuccessful get() = exitCode == 0
}

/**
 * Executes a program. This needs to be a valid path on the
 * file system.
 *
 * See [executeEscapedShellCommand] for the version that executes
 * `/bin/sh` commands.
 */
suspend fun executeCommand(
    executable: Path,
    args: List<String>? = null,
    dir: File? = null,
): CommandResult =
    // Blocking I/O should use threads designated for I/O
    withContext(Dispatchers.IO) {
        val cmdArgs = listOf(executable.toAbsolutePath().toString()) + (args ?: listOf())
        val proc =
            Runtime.getRuntime().exec(
                cmdArgs.toTypedArray(),
                arrayOf(),
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
    Option.fromNullable(System.getProperty("user.home"))
        .filter { it.isNotEmpty() }
        .recover { Option.fromNullable(System.getenv("HOME")).bind() }
        .filter { it.isNotEmpty() }
        .map { File(it) }
        .getOrNull()
}
