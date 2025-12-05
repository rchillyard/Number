ThisBuild / organization := "com.phasmidsoftware"

name := "Number"

ThisBuild / version := "1.3.3"

val catsVersion = "2.13.0"
val scalaTestVersion = "3.2.19"
val scalaParserCombinatorsVersion = "2.4.0"
val nScalaTimeVersion = "2.32.0"
val apacheCommonsVersion = "3.6.1"
val flogVersion = "1.0.8"

ThisBuild / libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
  "ch.qos.logback" % "logback-classic" % "1.5.21" % "runtime"
)

lazy val root = (project in file("."))
    .aggregate(core, algebra, expression, parse, top)
    .settings(
      name := "number"
    )

lazy val core = (project in file("core"))
    .settings(
      scalaVersion := "2.13.16",
      libraryDependencies ++= Seq(
        "com.phasmidsoftware" %% "flog" % flogVersion,
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
        "ch.qos.logback" % "logback-classic" % "1.5.21" % "runtime"
      )
    )

lazy val algebra = (project in file("algebra"))
    .settings(
      scalaVersion := "3.7.3",
    ).dependsOn(core)

lazy val expression = (project in file("expression"))
    .settings(
      scalaVersion := "3.7.3",
    ).dependsOn(core, algebra)

lazy val parse = (project in file("parse"))
    .settings(
      scalaVersion := "3.7.3",
      libraryDependencies ++= Seq(
        "com.lihaoyi" %% "fastparse" % "3.1.1"
      )
    ).dependsOn(core, algebra, expression)

lazy val top = (project in file("top"))
    .settings(
      scalaVersion := "3.7.3",
    ).dependsOn(core, algebra, expression, parse)

scalacOptions ++= Seq("-encoding", "UTF-8", "-unchecked", "-deprecation")

Test / parallelExecution := false

// NOTE: if you reinstate these directories, you will need to manage the large crimes file (see code).
//Test / unmanagedSourceDirectories += baseDirectory.value / "src/it/scala"
//Test / unmanagedResourceDirectories += baseDirectory.value / "src/it/resources"

//resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

//scalacOptions ++= Seq("-encoding", "UTF-8", "-unchecked", "-deprecation", "-source:future-migration", "-rewrite", "-Vprint:typer")

//Compile / excludeFilter := {
//  val default = (Compile / excludeFilter).value
//  default || new SimpleFileFilter(_.getPath.contains("com/phasmidsoftware/number3/cats"))
//}
