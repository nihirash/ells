import Dependencies._

name := "ells"
description := "Embeddable Lisp-Like Scripting"
organization := "io.github.nihirash"
organizationName := "Nihirash"
organizationHomepage := Some(url("https://github.com/nihirash"))

scalaVersion := "2.12.6"
version := "0.2.3-SNAPSHOT"
scalafmtOnCompile := true

libraryDependencies ++= Seq(
  scalaTest % Test,
  fastParse
)

pomIncludeRepository := { _ => false }

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credential")

publishMavenStyle := true