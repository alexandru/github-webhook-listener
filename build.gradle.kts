import com.github.benmanes.gradle.versions.updates.DependencyUpdatesTask

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
                // Arrow libraries with native support
                implementation(libs.arrow.core)
                implementation(libs.arrow.fx.coroutines)
                implementation(libs.arrow.fx.stm)
                implementation(libs.arrow.suspendapp)
                
                // Ktor with native support
                implementation(libs.ktor.server.core)
                implementation(libs.ktor.server.cio)
                implementation(libs.ktor.server.html.builder)
                implementation(libs.ktor.serialization.kotlinx.json)
                
                // Serialization
                implementation(libs.kotlinx.serialization.json)
                implementation(libs.kaml)
                
                // CLI
                implementation(libs.clikt)
                
                // Coroutines
                implementation(libs.kotlinx.coroutines.core)
                
                // Crypto for HMAC
                implementation(libs.kcrypto)
                
                // Logging - using kotlin-logging with native support
                implementation(libs.kotlin.logging)
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

// kotlin {
//     jvmToolchain(22)
// }

tasks {
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
