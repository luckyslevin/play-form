name := """play-form"""

version := "4.0.0"

scalaVersion := "3.3.1"

organization := "com.ejisan"

githubOwner := "luckyslevin"
githubRepository := "play-form"
githubTokenSource := TokenSource.GitConfig("github.token")

lazy val root = (project in file(".")).enablePlugins(PlayScala)
