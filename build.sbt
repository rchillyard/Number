organization := "com.phasmidsoftware"

name := "Number"

version := "1.2.12-SNAPSHOT"

scalaVersion := "2.13.16"

scalacOptions ++= Seq("-encoding", "UTF-8", "-unchecked", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused" )

//Test / unmanagedSourceDirectories += baseDirectory.value / "it/scala"

val catsVersion = "2.13.0"
val scalaTestVersion = "3.2.19"

libraryDependencies ++= Seq(

  "com.phasmidsoftware" %% "flog" % "1.0.10",
  "com.phasmidsoftware" %% "matchers" % "1.0.11",
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
  "org.typelevel" %% "spire" % "0.18.0",
  "org.typelevel" %% "cats-kernel" % catsVersion,
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "discipline-scalatest" % "2.3.0" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test", // NOTE vulnerability here
  "org.scalacheck" %% "scalacheck" % "1.19.0" % "test", // This is used for testing Rational
  "org.typelevel" %% "cats-laws" % catsVersion % "test",
  "org.typelevel" %% "algebra-laws" % catsVersion % "test",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "ch.qos.logback" % "logback-classic" % "1.5.20" % "runtime"
)

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

