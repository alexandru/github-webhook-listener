rootProject.name = "github-webhook-listener"

pluginManagement {
    repositories {
        mavenCentral()
        gradlePluginPortal()
    }
}

dependencyResolutionManagement {
    versionCatalogs {
        create("libs") {
            version("arrow", "2.2.0")
            version("clikt", "5.0.3")
            version("coroutines", "1.10.2")
            version("kaml", "0.104.0")
            version("kotlin", "2.2.21")
            version("kotlinLogging", "7.0.13")
            version("ktlint", "14.0.1")
            version("ktor", "3.3.2")
            version("serialization", "1.9.0")
            version("suspendapp", "2.2.0")
            version("versions", "0.53.0")

            // https://plugins.gradle.org/plugin/org.jetbrains.kotlin.multiplatform
            plugin("kotlin-multiplatform", "org.jetbrains.kotlin.multiplatform")
                .versionRef("kotlin")
            library("kotlin-test", "org.jetbrains.kotlin", "kotlin-test")
                .versionRef("kotlin")
            
            // https://github.com/Kotlin/kotlinx.serialization
            plugin("kotlin-serialization", "org.jetbrains.kotlin.plugin.serialization")
                .versionRef("kotlin")
            library("kotlinx-serialization-json", "org.jetbrains.kotlinx", "kotlinx-serialization-json")
                .versionRef("serialization")

            // https://github.com/Kotlin/kotlinx.coroutines
            library("kotlinx-coroutines-core", "org.jetbrains.kotlinx", "kotlinx-coroutines-core")
                .versionRef("coroutines")

            // https://ktor.io/
            library("ktor-server-cio", "io.ktor", "ktor-server-cio")
                .versionRef("ktor")
            library("ktor-server-core", "io.ktor", "ktor-server-core")
                .versionRef("ktor")
            library("ktor-server-html-builder", "io.ktor", "ktor-server-html-builder")
                .versionRef("ktor")
            library("ktor-serialization-kotlinx-json", "io.ktor", "ktor-serialization-kotlinx-json")
                .versionRef("ktor")

            // https://github.com/JLLeitschuh/ktlint-gradle
            plugin("ktlint", "org.jlleitschuh.gradle.ktlint")
                .versionRef("ktlint")
            
            // https://github.com/ben-manes/gradle-versions-plugin
            plugin("versions", "com.github.ben-manes.versions")
                .versionRef("versions")

            // https://arrow-kt.io/
            library("arrow-core", "io.arrow-kt", "arrow-core")
                .versionRef("arrow")
            library("arrow-fx-coroutines", "io.arrow-kt", "arrow-fx-coroutines")
                .versionRef("arrow")
            library("arrow-fx-stm", "io.arrow-kt", "arrow-fx-stm")
                .versionRef("arrow")
            // https://arrow-kt.io/ecosystem/suspendapp/
            library("arrow-suspendapp", "io.arrow-kt", "suspendapp")
                .versionRef("suspendapp")

            // https://github.com/charleskorn/kaml
            library("kaml", "com.charleskorn.kaml", "kaml")
                .versionRef("kaml")
            
            // https://github.com/oshai/kotlin-logging
            library("kotlin-logging", "io.github.oshai", "kotlin-logging")
                .versionRef("kotlinLogging")
            
            // Crypto for HMAC (native support)
            version("kcrypto", "5.4.0")
            library("kcrypto", "com.soywiz.korlibs.krypto", "krypto")
                .versionRef("kcrypto")
            
            // https://github.com/ajalt/clikt
            library("clikt", "com.github.ajalt.clikt", "clikt")
                .versionRef("clikt")
        }
    }
}
