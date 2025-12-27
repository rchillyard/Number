ThisBuild / organization := "com.phasmidsoftware"

name := "Number"

ThisBuild / version := "1.3.6"

val catsVersion = "2.13.0"
val scalaTestVersion = "3.2.19"
val scalaParserCombinatorsVersion = "2.4.0"
val nScalaTimeVersion = "2.32.0"
val apacheCommonsVersion = "3.6.1"
val flogVersion = "1.0.8"

ThisBuild / libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
  "ch.qos.logback" % "logback-classic" % "1.5.23" % "runtime"
)

// ============================================================================
// COMPILER OPTIONS - Strict settings to prevent silent failures
// ============================================================================

// Common options for all Scala versions
val commonScalacOptions = Seq(
  "-encoding", "UTF-8",
  "-unchecked",
  "-deprecation",
  "-feature"
)

// Scala 2.13-specific strict options
val scala2StrictOptions = Seq(
  "-Xlint:_",                      // Enable all linting
  "-Ywarn-dead-code",              // Warn on dead code
  "-Ywarn-numeric-widen",          // Warn on numeric widening
  "-Ywarn-value-discard",          // Warn when non-Unit values are discarded
  "-Ywarn-unused:imports",         // Warn on unused imports
  "-Ywarn-unused:privates",        // Warn on unused privates
  "-Ywarn-unused:locals",          // Warn on unused locals
  "-Xfatal-warnings"               // Turn warnings into errors
)

// Scala 3-specific strict options to catch string concatenation issues
val scala3StrictOptions = Seq(
  "-Xfatal-warnings",              // Turn all warnings into errors (CRITICAL)
  "-Wvalue-discard",               // Error on discarded non-Unit values
  "-Wnonunit-statement",           // Error when non-Unit expressions are used as statements
  "-Wunused:all",                  // Warn about all unused things
  "-explain",                      // Provide detailed explanations for errors
  "-explain-types",                // Explain type errors in detail
  
  // The most important one for catching the + operator issue:
  // This catches when Any is inferred, which is what allows string concatenation
  "-Wconf:cat=other-match-analysis:error",  // Pattern match warnings as errors
  "-Wconf:cat=unused-imports:error",        // Unused imports as errors
  "-Wconf:cat=deprecation:error",           // Deprecation warnings as errors
  
  // Additional safety (optional - comment out if too strict)
  "-Ysafe-init",                   // Ensure safe initialization
  "-language:strictEquality"       // Require explicit Eql instances for == comparisons
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
      // Scala 2.13 module
      scalaVersion := "2.13.16",
      scalacOptions ++= commonScalacOptions ++ scala2StrictOptions,
      
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
      // Scala 3 module with strict options
      scalaVersion := "3.7.3",
      scalacOptions ++= commonScalacOptions ++ scala3StrictOptions,
      
      // NOTE: we need to use the 0.18.0 version of Spire for Scala 3.7.3
      // Currently commented out but keep this note
//      libraryDependencies ++= Seq(
//        "org.typelevel" %% "spire" % "0.18.0",
//      )
    ).dependsOn(core)

lazy val expression = (project in file("expression"))
    .settings(
      scalaVersion := "3.7.3",
      scalacOptions ++= commonScalacOptions ++ scala3StrictOptions
    ).dependsOn(core, algebra)

lazy val parse = (project in file("parse"))
    .settings(
      scalaVersion := "3.7.3",
      scalacOptions ++= commonScalacOptions ++ scala3StrictOptions,
      
      libraryDependencies ++= Seq(
        "com.lihaoyi" %% "fastparse" % "3.1.1"
      )
    ).dependsOn(core, algebra, expression)

lazy val top = (project in file("top"))
    .settings(
      scalaVersion := "3.7.3",
      scalacOptions ++= commonScalacOptions ++ scala3StrictOptions
    ).dependsOn(core, algebra, expression, parse)

// ============================================================================
// GLOBAL SETTINGS
// ============================================================================

Test / parallelExecution := false

// NOTE: if you reinstate these directories, you will need to manage the large crimes file (see code).
//Test / unmanagedSourceDirectories += baseDirectory.value / "src/it/scala"
//Test / unmanagedResourceDirectories += baseDirectory.value / "src/it/resources"

//resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

// ============================================================================
// NOTES ON COMPILER OPTIONS
// ============================================================================
// The Scala 3 strict options are specifically designed to:
// 1. Catch the rogue + operator issue (via -Xfatal-warnings and type inference checks)
// 2. Prevent silent string concatenation when types don't match
// 3. Catch unused code and potential bugs early
//
// If any option is too strict for your workflow, you can:
// - Remove it from scala3StrictOptions
// - Or comment it out individually in the module settings
//
// The most critical options for your concern are:
// - -Xfatal-warnings (turns all warnings into errors)
// - -Wvalue-discard (catches discarded values)
// - -Wnonunit-statement (catches non-Unit statements)
//
// Optional strictness (can be removed if too aggressive):
// - -Ysafe-init
// - -language:strictEquality
// ============================================================================

//Compile / excludeFilter := {
//  val default = (Compile / excludeFilter).value
//  default || new SimpleFileFilter(_.getPath.contains("com/phasmidsoftware/number3/cats"))
//}
