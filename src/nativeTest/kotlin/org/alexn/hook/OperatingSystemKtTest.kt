package org.alexn.hook

import kotlinx.coroutines.runBlocking
import kotlin.test.Test
import kotlin.test.assertTrue

class OperatingSystemKtTest {
    @Test
    fun `execute shell command with escaped arguments`() {
        runBlocking {
            val r =
                executeEscapedShellCommand(
                    command = "ls",
                    args = listOf("-alh"),
                    dir = USER_HOME,
                )
            assertTrue(r.isSuccessful, "isSuccessful")
            assertTrue(r.stdout.isNotEmpty(), "stdout.isNotEmpty")
            assertTrue(r.stderr.isEmpty(), "stderr.isEmpty")
        }
    }

    @Test
    fun `execute raw shell command`() {
        runBlocking {
            val r =
                executeRawShellCommand(
                    command = "ls -alh",
                    dir = USER_HOME,
                )
            assertTrue(r.isSuccessful, "isSuccessful")
            assertTrue(r.stdout.isNotEmpty(), "stdout.isNotEmpty")
            assertTrue(r.stderr.isEmpty(), "stderr.isEmpty")
        }
    }
}
