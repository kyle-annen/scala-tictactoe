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
    crossPaths := false,
    pomExtra :=
      <distributionManagement>
        <repository>
          <id>clojars</id>
          <name>Clojars repository</name>
          <url>https://clojars.org/repo</url>
        </repository>
      </distributionManagement>
      <licenses>
        <license>
          <name>MIT License</name>
          <url>http://www.opensource.org/licenses/mit-license.php</url>
        </license>
      </licenses>
        <url>http://github.com/kyle-annen/scala-tictactoe</url>
        <scm>
          <connection>
            scm:git:git://github.com/kyle-annen/scala-tictactoe.git
          </connection>
          <developerConnection>
            scm:git:ssh://github.com:kyle-annen/scala-ticatactoe.git
          </developerConnection>
          <url>
            http://github.com/kyle-annen/scala-tictactoe/tree/master
          </url>
        </scm>
  )
