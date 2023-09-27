name := """play-form"""

version := "4.0.0"

scalaVersion := "3.3.1"

organization := "com.ejisan"

publishTo := Some(Resolver.file("ejisan", file(Path.userHome.absolutePath+"/Development/repo.ejisan"))(Patterns(true, Resolver.mavenStyleBasePattern)))

lazy val root = (project in file(".")).enablePlugins(PlayScala)

// libraryDependencies += "joda-time" % "joda-time" % "2.12.5"
