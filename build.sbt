name := "jessitron-bison"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.3"

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots")

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

resolvers += "scribe" at "https://raw.github.com/fernandezpablo85/scribe-java/mvn-repo/"

libraryDependencies += "org.scalaz.stream" %% "scalaz-stream" % "0.4.1"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.1" % "test"

libraryDependencies += "org.scribe" % "scribe" % "1.3.6"

resolvers += "spray" at "http://repo.spray.io/"

libraryDependencies += "io.spray" %%  "spray-json" % "1.2.5"

net.virtualvoid.sbt.graph.Plugin.graphSettings

