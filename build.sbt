organization := "com.phasmidsoftware"

name := "Number"

version := "1.0.9"

scalaVersion := "2.13.6"

scalacOptions ++= Seq( "-target:jvm-1.8", "-encoding", "UTF-8", "-unchecked", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused" )

val scalaTestVersion = "3.2.3"

lazy val root = (project in file(".")).dependsOn(matchers)

lazy val matchers = RootProject(uri("git://github.com/rchillyard/Matchers"))

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.2.0-M1",
    "org.apache.commons" % "commons-math3" % "3.6.1", // This is used for the trait Fuzzy (old stuff)
    "ch.qos.logback" % "logback-classic" % "1.2.3" % "test",
    "org.scalacheck" %% "scalacheck" % "1.14.1" % "test" // This is used for testing Rational
)

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"


