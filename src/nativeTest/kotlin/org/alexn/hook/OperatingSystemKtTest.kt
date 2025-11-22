package org.alexn.hook

// TODO: Update tests for Kotlin/Native compatibility
// 
// Required changes:
// 1. Test native popen/pclose implementation
// 2. Add tests for command execution with different exit codes
// 3. Test stdout/stderr capture
//
// Original tests are preserved in git history (src/test/kotlin/org/alexn/hook/OperatingSystemKtTest.kt)

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue
import kotlinx.coroutines.runBlocking

class OperatingSystemKtTest {
    @Test
    fun testExecuteSimpleCommand() = runBlocking {
        val result = executeRawShellCommand("echo 'Hello, Native!'")
        assertTrue(result.isSuccessful)
        assertEquals(0, result.exitCode)
    }
    
    @Test
    fun testExecuteCommandWithExitCode() = runBlocking {
        val result = executeRawShellCommand("exit 1")
        assertEquals(1, result.exitCode)
    }
    
    // TODO: Add more comprehensive command execution tests
    // TODO: Test directory changing
    // TODO: Test environment variables
}
