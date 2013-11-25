name := "jessitron-crtd"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.2"

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots")

libraryDependencies += "org.scalaz.stream" %% "scalaz-stream" % "0.3-SNAPSHOT"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.0" % "test"

