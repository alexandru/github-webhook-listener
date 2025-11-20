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
            version("buildToolsNative", "0.11.3")
            version("clikt", "5.0.3")
            version("commonsCodec", "1.20.0")
            version("commonsText", "1.14.0")
            version("kaml", "0.104.0")
            version("kotlin", "2.2.21")
            version("kotlinLogging", "7.0.13")
            version("ktlint", "14.0.1")
            version("ktor", "3.3.2")
            version("ktorServerTests", "2.3.13")
            version("logback", "1.5.21")
            version("serialization", "1.9.0")
            version("suspendapp", "2.2.0")
            version("versions", "0.53.0")

            // https://plugins.gradle.org/plugin/org.jetbrains.kotlin.jvm
            plugin("kotlin-jvm", "org.jetbrains.kotlin.jvm")
                .versionRef("kotlin")
            library("kotlin-stdlib-jdk8", "org.jetbrains.kotlin", "kotlin-stdlib-jdk8")
                .versionRef("kotlin")
            library("kotlin-test-junit", "org.jetbrains.kotlin", "kotlin-test-junit")
                .versionRef("kotlin")
            // https://github.com/Kotlin/kotlinx.serialization
            plugin("kotlin-serialization", "org.jetbrains.kotlin.plugin.serialization")
                .versionRef("kotlin")
            library("kotlinx-serialization-json", "org.jetbrains.kotlinx", "kotlinx-serialization-json")
                .versionRef("serialization")
            library("kotlinx-serialization-hocon", "org.jetbrains.kotlinx", "kotlinx-serialization-hocon")
                .versionRef("serialization")

            // https://ktor.io/
            plugin("ktor", "io.ktor.plugin")
                .versionRef("ktor")
            library("ktor-serialization-kotlinx-json", "io.ktor", "ktor-serialization-kotlinx-json")
                .versionRef("ktor")
            library("ktor-server-cio", "io.ktor", "ktor-server-cio")
                .versionRef("ktor")
            library("ktor-server-core", "io.ktor", "ktor-server-core")
                .versionRef("ktor")
            library("ktor-server-html-builder", "io.ktor", "ktor-server-html-builder")
                .versionRef("ktor")
            library("ktor-server-tests-jvm", "io.ktor", "ktor-server-tests-jvm")
                .versionRef("ktorServerTests")

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

            library("commons-codec", "commons-codec", "commons-codec")
                .versionRef("commonsCodec")
            library("commons-text", "org.apache.commons", "commons-text")
                .versionRef("commonsText")
            library("kaml", "com.charleskorn.kaml", "kaml")
                .versionRef("kaml")
            library("logback-classic", "ch.qos.logback", "logback-classic")
                .versionRef("logback")
            library("kotlin-logging", "io.github.oshai", "kotlin-logging-jvm")
                .versionRef("kotlinLogging")
            library("clikt", "com.github.ajalt.clikt", "clikt")
                .versionRef("clikt")

            plugin("graalvm-buildtools-native", "org.graalvm.buildtools.native")
                .versionRef("buildToolsNative")
        }
    }
}
