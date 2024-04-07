val Http4sVersion = "0.23.25"
val CirceVersion = "0.14.6"
val MunitVersion = "0.7.29"
val LogbackVersion = "1.5.3"
val MunitCatsEffectVersion = "1.0.7"
val PureConfigVersion = "0.17.5"

lazy val printTransitiveDependencies = taskKey[Unit]("Print all transitive dependencies")

lazy val root = (project in file("."))
    .enablePlugins(NativeImagePlugin)
    .settings(
        organization := "org.alexn",
        name := "github-webhook-listener",
        version := "0.0.1-SNAPSHOT",
        scalaVersion := "3.3.1",
        libraryDependencies ++= Seq(
            "org.http4s" %% "http4s-ember-server" % Http4sVersion,
            "org.http4s" %% "http4s-ember-client" % Http4sVersion,
            "org.http4s" %% "http4s-circe" % Http4sVersion,
            "org.http4s" %% "http4s-dsl" % Http4sVersion,
            "com.github.pureconfig" %% "pureconfig-core" % PureConfigVersion,
            "org.scalameta" %% "munit" % MunitVersion % Test,
            "org.typelevel" %% "munit-cats-effect-3" % MunitCatsEffectVersion % Test,
            "ch.qos.logback" % "logback-classic" % LogbackVersion
        ),
        assembly / assemblyMergeStrategy := {
            case "module-info.class" => MergeStrategy.discard
            case x                   => (assembly / assemblyMergeStrategy).value.apply(x)
        },
        Compile / mainClass := Some("org.alexn.hook.Main"),
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
            "-H:-UnlockExperimentalVMOptions",
        ),
        Compile / resourceGenerators += Def.task {
            import NativeImageGenerateMetadataFiles._
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
    nativeImageJvm,
)
