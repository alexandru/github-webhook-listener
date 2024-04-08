val CirceVersion = "0.14.6"
val CirceYamlVersion = "0.15.1"
val DeclineVersion = "2.4.1"
val Http4sVersion = "0.23.26"
val LogbackVersion = "1.5.3"
val MunitCatsEffectVersion = "1.0.7"
val MunitVersion = "0.7.29"
val TapirVersion = "1.10.3"
val CommonsCodecVersion = "1.16.1"
val CommonsTextVersion = "1.11.0"

lazy val root = (project in file("."))
    .enablePlugins(NativeImagePlugin)
    .settings(
        organization := "org.alexn",
        name := "github-webhook-listener",
        version := "0.0.1-SNAPSHOT",
        scalaVersion := "3.3.1",
        libraryDependencies ++= Seq(
            "ch.qos.logback" % "logback-classic" % LogbackVersion,
            "com.monovore" %% "decline-effect" % DeclineVersion,
            "com.softwaremill.sttp.tapir" %% "tapir-http4s-server" % TapirVersion,
            "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % TapirVersion,
            "com.softwaremill.sttp.tapir" %% "tapir-swagger-ui-bundle" % TapirVersion,
            "commons-codec" % "commons-codec" % CommonsCodecVersion,
            "io.circe" %% "circe-core" % CirceVersion,
            "io.circe" %% "circe-generic" % CirceVersion,
            "io.circe" %% "circe-parser" % CirceVersion,
            "io.circe" %% "circe-yaml" % CirceYamlVersion,
            "org.apache.commons" % "commons-text" % CommonsTextVersion,
            "org.http4s" %% "http4s-ember-server" % Http4sVersion,
            // -- For testing
            "com.softwaremill.sttp.tapir" %% "tapir-sttp-stub-server" % TapirVersion % Test,
            "org.scalameta" %% "munit" % MunitVersion % Test,
            "org.typelevel" %% "munit-cats-effect-3" % MunitCatsEffectVersion % Test,
            "com.softwaremill.sttp.client3" %% "circe" % "3.9.5" % Test
        ),
        assembly / assemblyMergeStrategy := {
            case "module-info.class" => MergeStrategy.discard
            case x => (assembly / assemblyMergeStrategy).value.apply(x)
        },
        Compile / mainClass := Some("org.alexn.hook.Main"),
        Compile / run / fork := true,
        nativeImageJvm := "graalvm-java21",
        nativeImageVersion := "21",
        nativeImageOptions ++= Seq(
            "--verbose",
            "-march=native",
            "--no-fallback",
            // "--initialize-at-build-time=org.slf4j.LoggerFactory,ch.qos.logback,org.slf4j.impl.StaticLoggerBinder",
            // https://www.graalvm.org/latest/reference-manual/native-image/dynamic-features/Resources/
            "-H:+UnlockExperimentalVMOptions",
            "-H:IncludeResources=logback.xml",
            "-H:Log=registerResource:info",
            "-H:+InstallExitHandlers",
            "-H:+ReportUnsupportedElementsAtRuntime",
            "-H:+ReportExceptionStackTraces",
            "-H:-UnlockExperimentalVMOptions"
        ),
        Compile / resourceGenerators += Def.task {
            import NativeImageGenerateMetadataFiles.*
            implicit val logger: sbt.util.Logger = sbt.Keys.streams.value.log
            generateResourceFiles(
                // Path needed for cloning the metadata repository
                (Compile / target).value,
                // Path where the metadata files will be generated
                (Compile / resourceManaged).value / "META-INF" / "native-image",
                // List all tranzitive dependencies (can also add our own files)
                update.value
                    .allModules
                    .map(m => Artefact(s"${m.organization}:${m.name}:${m.revision}"))
                    .toList
            )
        }.taskValue
    )

Global / onChangedBuildSource := ReloadOnSourceChanges
Global / excludeLintKeys ++= Set(
    nativeImageVersion,
    nativeImageJvm
)
