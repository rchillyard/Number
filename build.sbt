// build.sbt for the Number project

ThisBuild / organization := "com.phasmidsoftware"

ThisBuild / version := "1.10.6"

val scalaVersionNumber = "3.9.0" // the current LTS
val catsVersion = "2.13.0"
val scalaTestVersion = "3.2.19"
val scalaParserCombinatorsVersion = "2.4.0"
val nScalaTimeVersion = "2.32.0"
val apacheCommonsVersion = "3.6.1"
val matchersVersion = "1.1.0"
val flogVersion = "1.1.0"
val logbackClassicVersion = "1.5.32"
val configVersion = "1.4.6"

ThisBuild / libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
  "com.typesafe" % "config" % configVersion,
  "ch.qos.logback" % "logback-classic" % logbackClassicVersion % "runtime"
)

// CONSIDER removing this (for performance reasons) if publishLocal runs fine without it.
Global / concurrentRestrictions += Tags.limitAll(1)

// ============================================================================
// COMPILER OPTIONS
// ============================================================================

// The compiler options that all the Phasmid libraries share.
ThisBuild / scalacOptions ++= Seq(
  "-encoding", "UTF-8",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Wunused:all",
  "-Wvalue-discard",
  "-Wnonunit-statement"
)
// Tests may use a value as a statement (for example, in a matcher), so each project's tests skip -Wnonunit-statement.
lazy val testScalacOptions = Test / scalacOptions ~= (_.filterNot(_ == "-Wnonunit-statement"))

// ============================================================================
// MODULE DEFINITIONS
// ============================================================================

// ----------------------------------------------------------------------------
// BUILD TOOLING — not a library module, not published
// Provides the Scala 3 TASTy-walking process launched by generateMermaidDiagram.
// ----------------------------------------------------------------------------
lazy val mermaid = (project in file("mermaid"))
  .settings(
    name             := "mermaid",
    scalaVersion     := scalaVersionNumber,
    publish / skip   := true,
    Test / test      := {},
    libraryDependencies += "ch.epfl.scala" %% "tasty-query" % "1.6.1",
    testScalacOptions
  )

lazy val root = (project in file("."))
  .enablePlugins(ScalaUnidocPlugin, MermaidDiagramGenerator, GhpagesPlugin)
  .aggregate(core, algebra, expression, parse, dimensions, top)
  // mermaid is NOT in aggregate: it is build tooling, not a library module
  .dependsOn(top)
  .settings(
    testScalacOptions,
    name := "number",
    scalaVersion := scalaVersionNumber,

    // Unidoc settings - create unified scaladoc for all modules
    ScalaUnidoc / unidoc / unidocProjectFilter :=
      inProjects(core, algebra, expression, parse, top),

    // Scaladoc options for the unified documentation
    ScalaUnidoc / unidoc / scalacOptions ++= Seq(
      "-groups",
      "-doc-title", "Number - Scala Mathematical Library",
      "-doc-version", version.value,
      "-sourcepath", (LocalRootProject / baseDirectory).value.getAbsolutePath,
      "-doc-root-content", (LocalRootProject / baseDirectory).value + "/rootdoc.txt"
    ),

    // The unified Scaladoc is published to https://rchillyard.github.io/Number/latest/api/ by `sbt ghpagesPushSite`
    ScalaUnidoc / siteSubdirName := "latest/api",
    addMappingsToSiteDir(ScalaUnidoc / packageDoc / mappings, ScalaUnidoc / siteSubdirName),
    git.remoteRepo := "git@github.com:rchillyard/Number.git"
  )
lazy val core = (project in file("core"))
  .settings(
    scalaVersion := scalaVersionNumber,
    testScalacOptions,

    libraryDependencies ++= Seq(
      "com.phasmidsoftware" %% "flog" % flogVersion,
      "com.phasmidsoftware" %% "matchers" % matchersVersion,
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
      "ch.qos.logback" % "logback-classic" % logbackClassicVersion % "runtime"
    )
  )

lazy val algebra = (project in file("algebra"))
  .settings(
    scalaVersion := scalaVersionNumber,
    testScalacOptions
  )
  .dependsOn(core)

lazy val expression = (project in file("expression"))
  .settings(
    scalaVersion := scalaVersionNumber,
    testScalacOptions
  )
  .dependsOn(core, algebra)

lazy val parse = (project in file("parse"))
  .settings(
    scalaVersion := scalaVersionNumber,
    testScalacOptions,

    libraryDependencies ++= Seq(
    )
  )
  .dependsOn(core, algebra, expression, dimensions)

lazy val top = (project in file("top"))
  .settings(
    scalaVersion := scalaVersionNumber,
    testScalacOptions
  )
  .dependsOn(core, algebra, expression, parse, dimensions)

lazy val dimensions = (project in file("dimensions"))
  .settings(
    scalaVersion := scalaVersionNumber,
    testScalacOptions
  )
  .dependsOn(algebra, core)

// ============================================================================
// GLOBAL SETTINGS
// ============================================================================

Test / parallelExecution := false


// ============================================================================
// USAGE NOTES
// ============================================================================
// The compiler options (above) are the ones that all the Phasmid libraries share.
// -Wvalue-discard and -Wnonunit-statement catch, among other things, a rogue + operator
// whose result is silently discarded.
//
// The -Wnonunit-statement option is filtered out for test code (testScalacOptions)
// because ScalaTest's assertions return Assertion, not Unit, and having multiple
// assertions in a test would trigger warnings.
// ============================================================================
