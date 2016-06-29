name := """play-form"""

version := "3.0.0"

scalaVersion := "2.11.8"

crossScalaVersions := Seq("2.11.8")

organization := "com.ejisan"

publishTo := Some(Resolver.file("ejisan", file(Path.userHome.absolutePath+"/Development/repo.ejisan"))(Patterns(true, Resolver.mavenStyleBasePattern)))

lazy val root = (project in file(".")).enablePlugins(PlayScala)

libraryDependencies += "joda-time" % "joda-time" % "2.9.2"
