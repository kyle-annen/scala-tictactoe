import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "org.clojars.kyleannen",
      scalaVersion := "2.12.2",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "tictactoe",
    libraryDependencies += scalaTest % Test,
    publishTo := Some("clojars" at "https://clojars.org/repo"),
    credentials += Credentials("clojars", "clojars.org", "kyleannen", "spacerace2021"),
    crossPaths := false
  )
