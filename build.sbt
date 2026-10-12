ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "poker",
    libraryDependencies += "com.lihaoyi" %% "cask" % "0.9.5"
  )