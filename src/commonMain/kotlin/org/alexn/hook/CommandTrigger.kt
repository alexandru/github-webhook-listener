package org.alexn.hook

import arrow.core.Either
import arrow.core.left
import arrow.core.right
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
        return synchronized(locks) {
            locks.getOrPut(key) { Mutex() }
        }
    }

    suspend fun triggerCommand(key: String): Either<RequestError, Unit> {
        val project =
            projects[key]
                ?: return RequestError.NotFound("Project `$key` does not exist").left()

        val timeoutDuration = project.timeout ?: 30.seconds
        val mutex = lockFor(key)
        mutex.lock()
        return try {
            val result =
                withTimeout(timeoutDuration) {
                    executeRawShellCommand(
                        command = project.command,
                        dir = File(project.directory),
                    )
                }
            if (result.isSuccessful) {
                Unit.right()
            } else {
                RequestError
                    .Internal(
                        "Command execution failed",
                        null,
                        meta =
                            mapOf(
                                "exit-code" to result.exitCode.toString(),
                                "stdout" to result.stdout,
                                "stderr" to result.stderr,
                            ),
                    ).left()
            }
        } catch (e: TimeoutCancellationException) {
            RequestError
                .TimedOut(
                    "Command execution timed-out after $timeoutDuration",
                ).left()
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
