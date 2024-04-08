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
            version("arrow", "1.2.4")
            version("buildToolsNative", "0.10.1")
            version("clikt", "4.3.0")
            version("commonsCodec", "1.16.1")
            version("commonsText", "1.11.0")
            version("kaml", "0.58.0")
            version("kotlin", "1.9.23")
            version("ktlint", "12.1.0")
            version("ktor", "2.3.10")
            version("logback", "1.5.3")
            version("serialization", "1.6.3")
            version("suspendapp", "0.4.0")
            version("versions", "0.51.0")

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

            library("commons-codec", "commons-codec", "commons-codec")
                .versionRef("commonsCodec")
            library("commons-text", "org.apache.commons", "commons-text")
                .versionRef("commonsText")
            library("kaml", "com.charleskorn.kaml", "kaml")
                .versionRef("kaml")
            library("logback-classic", "ch.qos.logback", "logback-classic")
                .versionRef("logback")
            library("clikt", "com.github.ajalt.clikt", "clikt")
                .versionRef("clikt")

            plugin("graalvm-buildtools-native", "org.graalvm.buildtools.native")
                .versionRef("buildToolsNative")
        }
    }
}
