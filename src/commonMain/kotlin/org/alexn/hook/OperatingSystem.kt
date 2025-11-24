package org.alexn.hook

import arrow.core.Option
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.IO
import kotlinx.coroutines.withContext

data class CommandResult(
    val exitCode: Int,
    val stdout: String,
    val stderr: String,
) {
    val isSuccessful get() = exitCode == 0
}

/**
 * Executes shell commands.
 */
suspend fun executeRawShellCommand(
    command: String,
    dir: File? = null,
): CommandResult =
    withContext(Dispatchers.IO) {
        executeCommand(command, dir)
    }

// File abstraction
data class File(val path: String)

val USER_HOME: File? by lazy {
    Option
        .fromNullable(getUserHomeDir())
        .filter { it.isNotEmpty() }
        .map { File(it) }
        .getOrNull()
}

// Platform-specific implementations
expect fun executeCommand(command: String, dir: File?): CommandResult
expect fun getUserHomeDir(): String?

