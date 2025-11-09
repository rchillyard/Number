name := "number-core"

scalaVersion := "2.13.16"

lazy val catsVersion = "2.13.0"
lazy val scalaTestVersion = "3.2.19"
// NOTE: Issue #44: this library is not currently compatible with version 2.x.x of the parser-combinators library
lazy val scalaParserCombinatorsVersion = "2.4.0"
lazy val nScalaTimeVersion = "2.32.0"
lazy val apacheCommonsVersion = "3.6.1"

libraryDependencies ++= Seq(
  "com.phasmidsoftware" %% "flog" % "1.0.8",
  "com.phasmidsoftware" %% "matchers" % "1.0.11",
  "org.apache.commons" % "commons-math3" % apacheCommonsVersion,
  "org.scala-lang.modules" %% "scala-parser-combinators" % scalaParserCombinatorsVersion,
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

