import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.nihirash",
      scalaVersion := "2.12.6",
      version := "0.1.0-SNAPSHOT"
    )),
    name := "Embeddable Lisp-Like Scripting",
    libraryDependencies += scalaTest % Test
  )
