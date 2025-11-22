import com.github.benmanes.gradle.versions.updates.DependencyUpdatesTask
import org.jetbrains.kotlin.gradle.plugin.mpp.KotlinNativeTarget

plugins {
    alias(libs.plugins.kotlin.multiplatform)
    alias(libs.plugins.kotlin.serialization)
    alias(libs.plugins.ktlint)
    alias(libs.plugins.versions)
}

group = "org.alexn.hook"
version = "0.0.1"

repositories {
    mavenCentral()
}

kotlin {
    // Configure native targets for Linux
    linuxX64("native") {
        binaries {
            executable {
                entryPoint = "org.alexn.hook.main"
                baseName = "github-webhook-listener"
                
                // Optimize for size and memory
                freeCompilerArgs += listOf(
                    "-opt",
                    "-Xallocator=mimalloc",
                )
            }
        }
    }

    sourceSets {
        val nativeMain by getting {
            dependencies {
                implementation(libs.kotlinx.coroutines.core)
                implementation(libs.kotlinx.serialization.json)
                implementation(libs.ktor.server.core)
                implementation(libs.ktor.server.cio)
                implementation(libs.ktor.server.html.builder)
                implementation(libs.clikt)
            }
        }

        val nativeTest by getting {
            dependencies {
                implementation(libs.kotlin.test)
            }
        }
    }
}

tasks {
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
}

// Wrapper task for easy execution
tasks.register("runNative") {
    dependsOn("nativeBinaries")
    group = "application"
    description = "Build and run the native executable"
    doLast {
        val executable = kotlin.targets.getByName<KotlinNativeTarget>("native")
            .binaries.getExecutable("main", "RELEASE")
        println("Executable: ${executable.outputFile.absolutePath}")
    }
}
