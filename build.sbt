organization := "com.phasmidsoftware"

name := "Number"

version := "1.1.1-SNAPSHOT"

scalaVersion := "2.13.14"

scalacOptions ++= Seq("-encoding", "UTF-8", "-unchecked", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused" )

val scalaTestVersion = "3.2.19"

libraryDependencies ++= Seq(
    "com.phasmidsoftware" %% "flog" % "1.0.10",
    "com.phasmidsoftware" %% "matchers" % "1.0.6",
    "org.apache.commons" % "commons-math3" % "3.6.1",
    "com.novocode" % "junit-interface" % "0.11" % "test",
    "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
    "ch.qos.logback" % "logback-classic" % "1.5.16" % "test",
    "org.scalacheck" %% "scalacheck" % "1.18.1" % "test" // This is used for testing Rational
)

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"


