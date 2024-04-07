addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.2.0")
addSbtPlugin("io.spray" % "sbt-revolver" % "0.10.0")
addSbtPlugin("org.scalameta" % "sbt-native-image" % "0.3.2")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.2")
addSbtPlugin("org.typelevel" % "sbt-tpolecat" % "0.5.0")

// Dependencies needed for using the graalvm-reachable-metadata repo
libraryDependencies ++= Seq(
    "io.circe" %% "circe-parser" % "0.14.1",
    "org.eclipse.jgit" % "org.eclipse.jgit" % "6.9.0.202403050737-r",
)
