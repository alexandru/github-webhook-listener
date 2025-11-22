package org.alexn.hook

import kotlinx.coroutines.TimeoutCancellationException
import kotlinx.coroutines.sync.Mutex
import kotlinx.coroutines.withTimeout
import kotlin.time.Duration.Companion.seconds

/**
 * Handles the actual shell command execution, per project.
 */
class CommandTrigger private constructor(
    private val projects: Map<String, AppConfig.Project>,
    private val locks: MutableMap<String, Mutex>,
) {
    private fun lockFor(key: String): Mutex {
        return locks.getOrPut(key) { Mutex() }
    }

    suspend fun triggerCommand(key: String): Result<Unit> {
        val project =
            projects[key]
                ?: return Result.Error(RequestError.NotFound("Project `$key` does not exist"))

        val timeoutDuration = project.timeout ?: 30.seconds
        val mutex = lockFor(key)
        mutex.lock()
        return try {
            val result =
                withTimeout(timeoutDuration) {
                    executeRawShellCommand(
                        command = project.command,
                        dir = project.directory,
                    )
                }
            if (result.isSuccessful) {
                Result.Success(Unit)
            } else {
                Result.Error(
                    RequestError.Internal(
                        "Command execution failed",
                        null,
                        meta =
                            mapOf(
                                "exit-code" to result.exitCode.toString(),
                                "stdout" to result.stdout,
                                "stderr" to result.stderr,
                            ),
                    )
                )
            }
        } catch (e: TimeoutCancellationException) {
            Result.Error(
                RequestError.TimedOut(
                    "Command execution timed-out after $timeoutDuration",
                )
            )
        } finally {
            mutex.unlock()
        }
    }

    companion object {
        /**
         * Builder with side effects.
         */
        operator fun invoke(projects: Map<String, AppConfig.Project>): CommandTrigger =
            CommandTrigger(
                projects,
                mutableMapOf(),
            )
    }
}
