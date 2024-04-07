addSbtPlugin("org.typelevel" % "sbt-tpolecat" % "0.5.0")
addSbtPlugin("io.spray" % "sbt-revolver" % "0.10.0")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.1.5")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.2")
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.9.16")
addSbtPlugin("org.scalameta" % "sbt-native-image" % "0.3.2")

libraryDependencies ++= Seq(
    "io.circe" %% "circe-parser" % "0.14.1",
    "org.eclipse.jgit" % "org.eclipse.jgit" % "6.9.0.202403050737-r",
)