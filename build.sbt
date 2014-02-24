name := "jessitron-bison"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.2"

//resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots")

libraryDependencies += "org.scalaz.stream" %% "scalaz-stream" % "snapshot-0.4"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.1" % "test"

resolvers += "spray" at "http://repo.spray.io/"

libraryDependencies += "io.spray" %%  "spray-json" % "1.2.5"

libraryDependencies += "org.scribe" % "scribe" % "1.3.5"

net.virtualvoid.sbt.graph.Plugin.graphSettings

