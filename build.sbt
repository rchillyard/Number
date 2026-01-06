ThisBuild / organization := "com.phasmidsoftware"

ThisBuild / version := "1.4.5"

val scalaVersionNumber = "3.7.3"
val catsVersion = "2.13.0"
val scalaTestVersion = "3.2.19"
val scalaParserCombinatorsVersion = "2.4.0"
val nScalaTimeVersion = "2.32.0"
val apacheCommonsVersion = "3.6.1"
val flogVersion = "1.0.11"

ThisBuild / libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
  "ch.qos.logback" % "logback-classic" % "1.5.23" % "runtime"
)

// ============================================================================
// COMPILER OPTIONS - Moderate settings focused on catching + operator issues
// ============================================================================

// Common options for all Scala versions
val commonScalacOptions = Seq(
  "-encoding", "UTF-8",
  "-unchecked",
  "-deprecation",
  "-feature"
)

// Scala 3-specific options - FOCUSED on catching string concatenation
val scala3Options = Seq(
  // CORE: These three are the minimum to catch the + operator issue
//  "-Xfatal-warnings",              // Turn warnings into errors
  "-Wvalue-discard", // Error on discarded non-Unit values
  "-Wnonunit-statement", // Error when non-Unit expressions used as statements

//  "-source:3.7-migration", "-rewrite", // Rewrites, here we do 3.7-migration.

  // HELPFUL: Good practices that don't add much burden
  "-explain", // Detailed error explanations
  "-Wunused:imports", // Catch unused imports
  "-Wunused:privates", // Catch unused private members
  "-Wunused:locals", // Catch unused local definitions
  "-Wconf:msg=Couldn't resolve a member:s" // Try to suppress linker warnings when generating docs
)

// Shared test settings for Scala 3 modules
// Removes -Wnonunit-statement for tests because ScalaTest assertions return Assertion, not Unit
val scala3TestSettings = Seq(
  Test / scalacOptions := scalacOptions.value.filterNot(_ == "-Wnonunit-statement")
)

// ============================================================================
// MODULE DEFINITIONS
// ============================================================================

lazy val root = (project in file("."))
    .aggregate(core, algebra, expression, parse, top)
    .dependsOn(top)
    .settings(
      name := "number",
      scalaVersion := scalaVersionNumber
    )
lazy val core = (project in file("core"))
    .settings(
      scalaVersion := scalaVersionNumber,
      scalacOptions ++= commonScalacOptions ++ scala3Options,

      libraryDependencies ++= Seq(
        "com.phasmidsoftware" %% "flog" % flogVersion,
        "com.phasmidsoftware" %% "matchers" % "1.0.13",
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
        "ch.qos.logback" % "logback-classic" % "1.5.23" % "runtime"
      )
    )

lazy val algebra = (project in file("algebra"))
    .settings(
      scalaVersion := scalaVersionNumber,
      scalacOptions ++= commonScalacOptions ++ scala3Options
    )
    .settings(scala3TestSettings)
    .dependsOn(core)

lazy val expression = (project in file("expression"))
    .settings(
      scalaVersion := scalaVersionNumber,
      scalacOptions ++= commonScalacOptions ++ scala3Options
    )
    .settings(scala3TestSettings)
    .dependsOn(core, algebra)

lazy val parse = (project in file("parse"))
    .settings(
      scalaVersion := scalaVersionNumber,
      scalacOptions ++= commonScalacOptions ++ scala3Options,

      libraryDependencies ++= Seq(
        "com.lihaoyi" %% "fastparse" % "3.1.1"
      )
    )
    .settings(scala3TestSettings)
    .dependsOn(core, algebra, expression)

lazy val top = (project in file("top"))
    .settings(
      scalaVersion := scalaVersionNumber,
      scalacOptions ++= commonScalacOptions ++ scala3Options,
      scalacOptions ++= Seq(
        "-Wconf:msg=any2stringadd:e"  // Make any2stringadd usage an ERROR
      )
    )
    .settings(scala3TestSettings)
    .dependsOn(core, algebra, expression, parse)

// ============================================================================
// GLOBAL SETTINGS
// ============================================================================

Test / parallelExecution := false

// ============================================================================
// USAGE NOTES
// ============================================================================
// This moderate configuration focuses on catching the rogue + operator issue
// without being overly strict about other things.
//
// The critical options for catching string concatenation are:
// 1. -Xfatal-warnings: Makes all warnings into compilation errors (currently disabled)
// 2. -Wvalue-discard: Catches when values are silently discarded
// 3. -Wnonunit-statement: Catches non-Unit expressions used as statements
//
// The -Wnonunit-statement option is filtered out for test code (scala3TestSettings)
// because ScalaTest's assertions return Assertion, not Unit, and having multiple
// assertions in a test would trigger warnings.
//
// To re-enable fatal warnings, uncomment the -Xfatal-warnings line in scala3Options.
// ============================================================================
