organization := "com.phasmidsoftware"

name := "Number"

version := "1.0.7"

scalaVersion := "2.13.4"

scalacOptions ++= Seq( "-target:jvm-1.8", "-encoding", "UTF-8", "-unchecked", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused" )

val scalaTestVersion = "3.2.3"

libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.2.0-M1",
  "org.apache.commons" % "commons-math3" % "3.6.1", // This is used for the trait Fuzzy (old stuff)
  "org.scalacheck" %% "scalacheck" % "1.14.1" % "test" // This is used for testing Rational
//  "org.typelevel" %% "cats-core" % "2.2.0"
)

