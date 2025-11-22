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
            version("clikt", "5.0.3")
            version("coroutines", "1.10.2")
            version("kotlin", "2.2.21")
            version("ktlint", "14.0.1")
            version("ktor", "3.3.2")
            version("serialization", "1.9.0")
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

            // https://github.com/JLLeitschuh/ktlint-gradle
            plugin("ktlint", "org.jlleitschuh.gradle.ktlint")
                .versionRef("ktlint")
            
            // https://github.com/ben-manes/gradle-versions-plugin
            plugin("versions", "com.github.ben-manes.versions")
                .versionRef("versions")

            // https://github.com/ajalt/clikt
            library("clikt", "com.github.ajalt.clikt", "clikt")
                .versionRef("clikt")
        }
    }
}
