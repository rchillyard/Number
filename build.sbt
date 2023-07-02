organization := "com.phasmidsoftware"

name := "Number"

version := "1.0.15"

scalaVersion := "2.13.6"

scalacOptions ++= Seq( "-target:jvm-1.8", "-encoding", "UTF-8", "-unchecked", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused" )

val scalaTestVersion = "3.2.15"

//lazy val root = (project in file(".")).dependsOn(matchers)

//lazy val matchers = RootProject(uri("git://github.com/rchillyard/Matchers#V1_0_5"))
//lazy val flog = RootProject(uri("git://github.com/rchillyard/Flog"))

libraryDependencies ++= Seq(
    "com.phasmidsoftware" %% "flog" % "1.0.8",
    "com.phasmidsoftware" %% "matchers" % "1.0.5",
    "org.apache.commons" % "commons-math3" % "3.6.1",
    "com.novocode" % "junit-interface" % "0.11" % "test",
    "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0",
    "ch.qos.logback" % "logback-classic" % "1.4.7" % "test",
    "org.scalacheck" %% "scalacheck" % "1.17.0" % "test" // This is used for testing Rational
)

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"


