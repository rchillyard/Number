ThisBuild / organization := "com.phasmidsoftware"

name := "Number"

ThisBuild / version := "1.3.6"

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

// Scala 2.13-specific options
val scala2Options = Seq(
//  "-Xlint:_",                      // Enable all linting
//  "-Ywarn-value-discard",          // Warn when non-Unit values are discarded
//  "-Ywarn-unused:imports",         // Warn on unused imports
//  "-Xfatal-warnings"               // Turn warnings into errors
)

// Scala 3-specific options - FOCUSED on catching string concatenation
val scala3Options = Seq(
  // CORE: These three are the minimum to catch the + operator issue
//  "-Xfatal-warnings",              // Turn warnings into errors
  "-Wvalue-discard",               // Error on discarded non-Unit values
  "-Wnonunit-statement",           // Error when non-Unit expressions used as statements

  // HELPFUL: Good practices that don't add much burden
  "-explain",                      // Detailed error explanations
  "-Wunused:imports",              // Catch unused imports
  "-Wunused:privates",             // Catch unused private members
  "-Wunused:locals"                // Catch unused local definitions
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
    .settings(
      name := "number"
    )

lazy val core = (project in file("core"))
    .settings(
      scalaVersion := "3.7.3",
      scalacOptions ++= commonScalacOptions ++ scala3Options,

      libraryDependencies ++= Seq(
        "com.phasmidsoftware" %% "flog" % flogVersion,
        "com.phasmidsoftware" %% "matchers" % "1.0.12",
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
      scalacOptions ++= commonScalacOptions ++ scala3Options
    )
    .settings(scala3TestSettings)
    .dependsOn(core)

lazy val expression = (project in file("expression"))
    .settings(
      scalaVersion := "3.7.3",
      scalacOptions ++= commonScalacOptions ++ scala3Options
    )
    .settings(scala3TestSettings)
    .dependsOn(core, algebra)

lazy val parse = (project in file("parse"))
    .settings(
      scalaVersion := "3.7.3",
      scalacOptions ++= commonScalacOptions ++ scala3Options,

      libraryDependencies ++= Seq(
        "com.lihaoyi" %% "fastparse" % "3.1.1"
      )
    )
    .settings(scala3TestSettings)
    .dependsOn(core, algebra, expression)

lazy val top = (project in file("top"))
    .settings(
      scalaVersion := "3.7.3",
      scalacOptions ++= commonScalacOptions ++ scala3Options
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
