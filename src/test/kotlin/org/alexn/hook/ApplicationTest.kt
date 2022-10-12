// package org.alexn.hook
//
// import io.ktor.client.request.get
// import io.ktor.client.statement.bodyAsText
// import io.ktor.http.HttpStatusCode
// import io.ktor.server.testing.testApplication
// import org.alexn.hook.plugins.configureRouting
// import kotlin.test.Test
// import kotlin.test.assertEquals
//
// class ApplicationTest {
//     @Test
//     fun testRoot() = testApplication {
//         application {
//             configureRouting()
//         }
//         client.get("/").apply {
//             assertEquals(HttpStatusCode.OK, status)
//             assertEquals("Hello World!", bodyAsText())
//         }
//     }
// }
