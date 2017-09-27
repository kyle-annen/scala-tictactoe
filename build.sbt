import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "org.clojars.kyleannen",
      scalaVersion := "2.12.2",
      version      := "0.2"
    )),
    name := "tictactoe",
    libraryDependencies += scalaTest % Test,
    crossPaths := false,
    publishMavenStyle := true,
    pomExtra :=
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
      <distributionManagement>
        <repository>
          <id>clojars</id>
          <name>Clojars repository</name>
          <url>https://clojars.org/repo</url>
        </repository>
      </distributionManagement>
      <build>
        <sourceDirectory>src/main/scala</sourceDirectory>
        <testSourceDirectory>src/test/scala</testSourceDirectory>
        <plugins>
          <plugin>
            <groupId>net.alchim31.maven</groupId>
            <artifactId>scala-maven-plugin</artifactId>
            <version>3.3.1</version>
          </plugin>
        </plugins>
      </build>
  )
