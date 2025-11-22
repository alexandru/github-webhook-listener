import com.github.benmanes.gradle.versions.updates.DependencyUpdatesTask
// import org.jetbrains.kotlin.gradle.dsl.JvmTarget

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

            buildArgs.add("--initialize-at-build-time=io.ktor,kotlinx,kotlin,org.xml.sax.helpers,org.slf4j.helpers")
            buildArgs.add("--initialize-at-build-time=org.slf4j.LoggerFactory,ch.qos.logback,org.slf4j.impl.StaticLoggerBinder")
            buildArgs.add("--initialize-at-build-time=com.github.ajalt.mordant.internal.nativeimage.NativeImagePosixMppImpls")
            buildArgs.add("--initialize-at-build-time=ch.qos.logback.classic.Logger")

            buildArgs.add("--no-fallback")
            buildArgs.add("-H:+UnlockExperimentalVMOptions")
            buildArgs.add("-H:+InstallExitHandlers")
            buildArgs.add("-H:+ReportExceptionStackTraces")
            buildArgs.add("-H:+ReportUnsupportedElementsAtRuntime")
            buildArgs.add("-R:MaxHeapSize=30m")
            buildArgs.add("-R:MaxNewSize=2m")
            buildArgs.add("-R:MinHeapSize=2m")

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
    implementation(libs.kotlin.logging)
    implementation(libs.kotlin.stdlib.jdk8)
    implementation(libs.kotlin.test.junit)
    implementation(libs.kotlinx.serialization.json)
    implementation(libs.kotlinx.serialization.hocon)
    implementation(libs.ktor.serialization.kotlinx.json)
    implementation(libs.ktor.server.cio)
    implementation(libs.ktor.server.core)
    implementation(libs.ktor.server.html.builder)
    implementation(libs.ktor.server.tests.jvm)
    implementation(libs.logback.classic)
}

// kotlin {
//     jvmToolchain(22)
// }

tasks {
    // UPX compression configuration
    val upxVersion = "4.2.4"
    val upxDir = layout.buildDirectory.dir("upx").get().asFile
    val upxExecutable = File(upxDir, "upx")

    val downloadUpx by registering {
        description = "Downloads UPX (Ultimate Packer for eXecutables) for binary compression"
        group = "build"
        
        onlyIf { !upxExecutable.exists() }
        
        doLast {
            upxDir.mkdirs()
            
            val osArch = System.getProperty("os.arch")
            val upxArch = when {
                osArch.contains("aarch64") || osArch.contains("arm64") -> "arm64"
                osArch.contains("amd64") || osArch.contains("x86_64") -> "amd64"
                else -> throw GradleException("Unsupported architecture: $osArch")
            }
            
            val upxArchive = File(upxDir, "upx.tar.xz")
            val upxUrl = "https://github.com/upx/upx/releases/download/v$upxVersion/upx-$upxVersion-${upxArch}_linux.tar.xz"
            
            println("Downloading UPX from $upxUrl")
            // Download UPX using Ant task
            ant.invokeMethod("get", mapOf("src" to upxUrl, "dest" to upxArchive))
            
            // Extract the archive using ProcessBuilder
            println("Extracting UPX archive...")
            val extractProcess = ProcessBuilder("tar", "-xf", upxArchive.name)
                .directory(upxDir)
                .redirectOutput(ProcessBuilder.Redirect.INHERIT)
                .redirectError(ProcessBuilder.Redirect.INHERIT)
                .start()
            val extractResult = extractProcess.waitFor()
            if (extractResult != 0) {
                throw GradleException("Failed to extract UPX archive")
            }
            
            // Move the upx binary to the root of upxDir
            val extractedDir = File(upxDir, "upx-$upxVersion-${upxArch}_linux")
            val upxBinary = File(extractedDir, "upx")
            upxBinary.copyTo(upxExecutable, overwrite = true)
            upxExecutable.setExecutable(true)
            
            // Clean up
            upxArchive.delete()
            extractedDir.deleteRecursively()
            
            println("UPX downloaded successfully to ${upxExecutable.absolutePath}")
        }
    }

    val compressNativeBinary by registering(Exec::class) {
        description = "Compresses the native binary with UPX"
        group = "build"
        
        dependsOn(downloadUpx, "nativeCompile")
        
        doFirst {
            val binaryPath = layout.buildDirectory.file("native/nativeCompile/github-webhook-listener").get().asFile
            
            if (!binaryPath.exists()) {
                throw GradleException("Native binary not found at: ${binaryPath.absolutePath}")
            }
            
            val sizeKB = binaryPath.length() / 1024
            println("Original binary size: $sizeKB KB")
        }
        
        workingDir = layout.buildDirectory.get().asFile
        commandLine(
            upxExecutable.absolutePath,
            "--best",
            "--lzma",
            "native/nativeCompile/github-webhook-listener"
        )
        
        doLast {
            val binaryPath = layout.buildDirectory.file("native/nativeCompile/github-webhook-listener").get().asFile
            val sizeKB = binaryPath.length() / 1024
            println("Compressed binary size: $sizeKB KB")
        }
    }

    // Make nativeCompile automatically compress the binary
    named("nativeCompile") {
        finalizedBy(compressNativeBinary)
    }

    withType<JavaCompile>().configureEach {
        options.release.set(21)
    }

    withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile>().configureEach {
        compilerOptions {
            jvmTarget.set(org.jetbrains.kotlin.gradle.dsl.JvmTarget.JVM_21)
            javaParameters.set(true)
        }
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
