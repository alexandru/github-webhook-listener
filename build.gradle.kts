plugins {
    application
    kotlin("jvm")
    id("io.ktor.plugin")
    id("org.jetbrains.kotlin.plugin.serialization")
    // See https://github.com/JLLeitschuh/ktlint-gradle
    id("org.jlleitschuh.gradle.ktlint")
    // https://graalvm.github.io/native-build-tools/0.9.14/gradle-plugin.html
    id("org.graalvm.buildtools.native")
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
    binaries {
        named("main") {
            fallback.set(false)
            verbose.set(true)

            buildArgs.add("--initialize-at-build-time=org.slf4j.LoggerFactory,ch.qos.logback")
            buildArgs.add("--initialize-at-build-time=io.ktor,kotlinx,kotlin")

            buildArgs.add("-H:+InstallExitHandlers")
            buildArgs.add("-H:+ReportUnsupportedElementsAtRuntime")
            buildArgs.add("-H:+ReportExceptionStackTraces")
            buildArgs.add("-R:MinHeapSize=2m")
            buildArgs.add("-R:MaxHeapSize=30m")
            buildArgs.add("-R:MaxNewSize=2m")
            buildArgs.add("--no-fallback")

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
    implementation(libs.commons.codec)
    implementation(libs.commons.text)
    implementation(libs.kaml)
    implementation(libs.kotlin.stdlib.jdk8)
    implementation(libs.kotlin.test.junit)
    implementation(libs.kotlinx.cli)
    implementation(libs.kotlinx.serialization.json)
    implementation(libs.ktor.serialization.kotlinx.json)
    implementation(libs.ktor.server.cio)
    implementation(libs.ktor.server.core)
    implementation(libs.ktor.server.html.builder)
    implementation(libs.ktor.server.tests.jvm)
    implementation(libs.logback.classic)
}

tasks {
    withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile> {
        kotlinOptions.jvmTarget = JavaVersion.VERSION_17.toString()
        kotlinOptions.javaParameters = true
    }

    test {
    }
}

ktor {
    fatJar {
        archiveFileName.set("github-webhook-listener-fat.jar")
    }
}
