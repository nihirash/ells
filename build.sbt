import Dependencies._

name := "ells"
description := "Embeddable Lisp-Like Scripting"
organization := "io.github.nihirash"
organizationName := "Nihirash"
organizationHomepage := Some(url("https://github.com/nihirash"))

scalaVersion := "2.12.6"
version := "0.2.4"
scalafmtOnCompile := true

scmInfo := Some(
  ScmInfo(
    url("https://github.com/nihirash/ells"),
    "scm:git@github.com:nihirash/ells.git"
  )
)

developers := List(
  Developer(
    id = "nihirash",
    name = "Alexander Sharihin",
    email = "anihirash@gmail.com",
    url = url("https://github.com/nihirash")
  )
)

licenses := List("MIT" -> new URL("https://opensource.org/licenses/MIT"))
homepage := Some(url("https://github.com/nihirash/ells"))


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
