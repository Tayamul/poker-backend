ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "poker",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "cask" % "0.9.5",
      "org.scalameta" %% "munit" % "1.0.2" % Test
    ),
    Compile / mainClass := Some("Api"),
    assembly / mainClass := Some("Api")
  )