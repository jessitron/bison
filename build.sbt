name := "jessitron-bison"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.2"

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots")

libraryDependencies += "org.scalaz.stream" %% "scalaz-stream" % "0.3-SNAPSHOT"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.0" % "test"

resolvers += "spray" at "http://repo.spray.io/"

libraryDependencies += "io.spray" %%  "spray-json" % "1.2.5"

libraryDependencies += "org.scribe" % "scribe" % "1.3.5"

