import com.github.benmanes.gradle.versions.updates.DependencyUpdatesTask

plugins {
    application
    alias(libs.plugins.kotlin.jvm)
    alias(libs.plugins.ktlint)
    alias(libs.plugins.versions)
    alias(libs.plugins.ktor)
    alias(libs.plugins.kotlin.serialization)
    alias(libs.plugins.graalvm.buildtools.native)
}

group = "org.alexn.hook"
version = "0.0.1"

application {
    mainClass.set("org.alexn.hook.MainKt")

    if (project.ext.has("development")) {
        applicationDefaultJvmArgs = listOf("-Dio.ktor.development=true")
    }
    // https://www.graalvm.org/22.0/reference-manual/native-image/Agent/
    if (project.ext.has("nativeAgent")) {
        applicationDefaultJvmArgs = listOf("-agentlib:native-image-agent=config-output-dir=./src/main/resources/META-INF/native-image")
    }
}

// https://ktor.io/docs/graalvm.html#execute-the-native-image-tool
// https://github.com/ktorio/ktor-samples/blob/main/graalvm/build.gradle.kts
graalvmNative {
    // https://github.com/oracle/graalvm-reachability-metadata
    // https://graalvm.github.io/native-build-tools/latest/gradle-plugin.html#metadata-support
    metadataRepository {
        enabled = true
        // https://github.com/oracle/graalvm-reachability-metadata/releases/
        version = "0.3.7"
    }

    binaries {
        named("main") {
            fallback.set(false)
            verbose.set(true)

            buildArgs.add("--initialize-at-build-time=io.ktor,kotlinx,kotlin")
            buildArgs.add("--initialize-at-build-time=org.slf4j.LoggerFactory,ch.qos.logback,org.slf4j.impl.StaticLoggerBinder")
            buildArgs.add("--no-fallback")
            buildArgs.add("-H:+UnlockExperimentalVMOptions")
            buildArgs.add("-H:+InstallExitHandlers")
            buildArgs.add("-H:+ReportExceptionStackTraces")
            buildArgs.add("-H:+ReportUnsupportedElementsAtRuntime")
            buildArgs.add("-R:MaxHeapSize=30m")
            buildArgs.add("-R:MaxNewSize=2m")
            buildArgs.add("-R:MinHeapSize=2m")
            buildArgs.add("-march=native")

            imageName.set("github-webhook-listener")
        }
    }
}

repositories {
    mavenCentral()
}

dependencies {
    implementation(libs.arrow.core)
    implementation(libs.arrow.fx.coroutines)
    implementation(libs.arrow.fx.stm)
    implementation(libs.arrow.suspendapp)
    implementation(libs.clikt)
    implementation(libs.commons.codec)
    implementation(libs.commons.text)
    implementation(libs.kaml)
    implementation(libs.kotlin.stdlib.jdk8)
    implementation(libs.kotlin.test.junit)
    implementation(libs.kotlinx.serialization.json)
    implementation(libs.ktor.serialization.kotlinx.json)
    implementation(libs.ktor.server.cio)
    implementation(libs.ktor.server.core)
    implementation(libs.ktor.server.html.builder)
    implementation(libs.ktor.server.tests.jvm)
    implementation(libs.logback.classic)
}

kotlin {
    jvmToolchain(21)
}

java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(21))
    }
}

tasks {
    withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile> {
        kotlinOptions.jvmTarget = JavaVersion.VERSION_21.toString()
        kotlinOptions.javaParameters = true
    }

    named<DependencyUpdatesTask>("dependencyUpdates").configure {
        fun isNonStable(version: String): Boolean {
            val stableKeyword = listOf("RELEASE", "FINAL", "GA").any { version.uppercase().contains(it) }
            val regex = "^[0-9,.v-]+(-r)?$".toRegex()
            val isStable = stableKeyword || regex.matches(version)
            return isStable.not()
        }

        rejectVersionIf {
            isNonStable(candidate.version) && !isNonStable(currentVersion)
        }
        checkForGradleUpdate = true
        outputFormatter = "html"
        outputDir = "build/dependencyUpdates"
        reportfileName = "report"
    }

    test {
    }
}

ktor {
    fatJar {
        archiveFileName.set("github-webhook-listener-fat.jar")
    }
}
