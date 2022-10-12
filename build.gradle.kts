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

    val isDevelopment: Boolean = project.ext.has("development")
    applicationDefaultJvmArgs = listOf(
        "-Dio.ktor.development=$isDevelopment",
        // https://www.graalvm.org/22.0/reference-manual/native-image/Agent/
        // "-agentlib:native-image-agent=config-output-dir=./src/main/resources/META-INF/native-image"
    )
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
            buildArgs.add("--no-fallback")

            imageName.set("github-webhook-listener")
        }
    }
}

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.jetbrains.kotlin:kotlin-stdlib-jdk8")
    implementation(libs.logback.classic)
    implementation(libs.kaml)
    implementation(libs.commons.codec)
    implementation(libs.arrow.core)
    implementation(libs.arrow.fx.coroutines)
    implementation(libs.arrow.fx.stm)
    implementation(libs.ktor.serialization.kotlinx.json)
    implementation(libs.ktor.server.content.negotiation)
    implementation(libs.ktor.server.core)
    // implementation(libs.ktor.server.netty)
    implementation(libs.ktor.server.cio)
    implementation(libs.ktor.server.tests.jvm)
    implementation(libs.commons.text)
    implementation(libs.kotlin.test.junit)
    implementation(libs.kotlinx.cli)
    implementation(libs.kotlinx.serialization.hocon)
    implementation(libs.kotlinx.serialization.json)
}

tasks {
    withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile> {
        kotlinOptions.jvmTarget = JavaVersion.VERSION_17.toString()
        kotlinOptions.javaParameters = true
    }
}

ktor {
    fatJar {
        archiveFileName.set("github-webhook-listener-fat.jar")
    }
}
