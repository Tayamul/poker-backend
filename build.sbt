ThisBuild / scalaVersion := "3.8.3"

lazy val root = (project in file("."))
  .settings(
    name := "poker",
    libraryDependencies += "com.lihaoyi" %% "cask" % "0.9.5"
  )