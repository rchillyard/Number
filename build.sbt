// build.sbt for the Number project

ThisBuild / organization := "com.phasmidsoftware"

ThisBuild / version := "1.11.0"

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
val jupiterVersion = "6.0.3"
// Must match the sbt-jupiter-interface plugin in project/plugins.sbt.
val jupiterInterfaceVersion = "0.19.0"

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
// -Werror: warnings fail the build.
ThisBuild / scalacOptions ++= Seq(
  "-encoding", "UTF-8",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Wunused:all",
  "-Wvalue-discard",
  "-Wnonunit-statement",
  "-Werror"
)
// Tests may use a value as a statement (for example, in a matcher), so each project's tests skip -Wnonunit-statement.
lazy val testScalacOptions = Test / scalacOptions ~= (_.filterNot(_ == "-Wnonunit-statement"))

// NOTE on test selection, kept because getting this wrong is invisible. Every test here runs in
// every environment: there is no tag exclusion and CI uses a plain `sbt test`. If a tag filter
// is ever wanted, it must be scoped to its framework with
// Tests.Argument(TestFrameworks.ScalaTest, ...) and NOT given on a command line as
// `testOnly -- -l SomeTag`. sbt hands everything after `--` to EVERY test framework on the
// classpath, and the JUnit interface answers an unrecognised selector by running none of the 140
// Java tests in core -- silently, with no run line, no skip count and exit code 0. That is what
// issue #217 was.

// ============================================================================
// AGGREGATED TEST TOTAL
// ============================================================================

// `sbt test` reports a summary per module and no overall total, because `test` is defined per
// project and returns Unit -- aggregation runs it once per module and each run logs its own
// counts through testResultLogger, so there is nothing to add up. On a six-module build that
// means six "All tests passed" blocks and arithmetic by hand.
//
// Test / executeTests is the task underneath `test` and does return a value, so collecting one
// per aggregated module and folding gives a single figure. See issue #223.
lazy val testAll = taskKey[Unit]("Run every aggregated module's tests and report one grand total")

testAll := {
  val log = streams.value.log
  val outputs = (Test / executeTests).all(ScopeFilter(inAggregates(LocalRootProject))).value
  val total = outputs.flatMap(_.events.values).foldLeft(SuiteResult.Empty)(_ + _)
  // The per-module summaries, which restore the detail that `test` would have logged through
  // testResultLogger -- executeTests does not run it.
  //
  // NOTE these do NOT account for every test. ScalaTest and ScalaCheck emit a summary; NEITHER
  // JUnit interface does -- measured on 2026-11-19 for com.novocode % junit-interface % 0.11 and
  // again for com.github.sbt.junit % jupiter-interface % 0.19.0 on the issue #218 branch, both
  // yielding only ScalaTest and ScalaCheck entries. So core's 140 Java tests appear in no line
  // below and only in the TOTAL, which is derived from the events rather than the summaries:
  // the summaries add up to 3941 against a total of 4081, before and after that migration.
  //
  // The TOTAL is therefore the only figure that would notice the Java tests silently ceasing to
  // run, which is what issue #217 was. Do not read the summaries as a complete breakdown.
  outputs.flatMap(_.summaries).filter(_.summaryText.nonEmpty).foreach(s => log.info(s"  ${s.name}: ${s.summaryText}"))
  log.info(
    s"TOTAL over ${outputs.size} modules: ${total.passedCount} passed, ${total.failureCount} failed, " +
      s"${total.errorCount} errors, ${total.skippedCount} skipped, ${total.ignoredCount} ignored, " +
      s"${total.canceledCount} canceled, ${total.pendingCount} pending")
  // executeTests does NOT fail the build -- that is `test`'s job -- so without this a red suite
  // exits 0.
  val bad = total.failureCount + total.errorCount
  if (bad > 0) sys.error(s"$bad test(s) did not pass")
}

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
      // The Java API tests in core/src/test/java. jupiter-interface is the sbt test *interface*:
      // without one, sbt compiles those tests and never runs them. See issue #218; it replaces
      // com.novocode % junit-interface % 0.11, which was the only JUnit 4 interface available,
      // is unmaintained since 2013, and was flagged here as a vulnerability.
      "org.junit.jupiter" % "junit-jupiter-api" % jupiterVersion % Test,
      "org.junit.jupiter" % "junit-jupiter-engine" % jupiterVersion % Test,
      "com.github.sbt.junit" % "jupiter-interface" % jupiterInterfaceVersion % Test,
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
