package org.alexn.hook

// TODO: Update tests for Kotlin/Native compatibility
// 
// Required changes:
// 1. Replace Java File I/O with native file operations
// 2. Update test data loading for native
// 3. Test simplified YAML parser limitations
//
// Original tests are preserved in git history (src/test/kotlin/org/alexn/hook/AppConfigTest.kt)

import kotlin.test.Test
import kotlin.test.assertEquals

class AppConfigTest {
    @Test
    fun testParseSimpleJson() {
        val json = """
        {
          "http": {
            "port": 8080,
            "path": "/hooks"
          },
          "projects": {
            "test-project": {
              "ref": "refs/heads/main",
              "directory": "/tmp/test",
              "command": "echo test",
              "secret": "test-secret"
            }
          }
        }
        """.trimIndent()
        
        val result = AppConfig.parseJson(json)
        when (result) {
            is Result.Success -> {
                assertEquals(8080, result.value.http.port)
                assertEquals("/hooks", result.value.http.basePath)
                assertEquals(1, result.value.projects.size)
            }
            is Result.Error -> throw result.exception
        }
    }
    
    // TODO: Add YAML parsing tests
    // TODO: Test file reading with native I/O
    // TODO: Test configuration validation
}
