name := "number-expression"

scalaVersion := "3.7.3"

//scalacOptions ++= Seq("-encoding", "UTF-8", "-unchecked", "-deprecation", "-source:future-migration", "-rewrite", "-Vprint:typer")

//Compile / excludeFilter := {
//  val default = (Compile / excludeFilter).value
//  default || new SimpleFileFilter(_.getPath.contains("com/phasmidsoftware/number3/cats"))
//}

//Test / unmanagedSourceDirectories += baseDirectory.value / "it/scala"

lazy val catsVersion = "2.13.0"
lazy val scalaTestVersion = "3.2.19"
lazy val scalaParserCombinatorsVersion = "2.4.0"
lazy val nScalaTimeVersion = "2.32.0"
lazy val apacheCommonsVersion = "3.6.1"

libraryDependencies ++= Seq(
//  "com.phasmidsoftware" %% "flog" % "1.0.10",
//  "com.phasmidsoftware" %% "matchers" % "1.0.11",
//  "org.apache.commons" % "commons-math3" % apacheCommonsVersion,
//  "org.scala-lang.modules" %% "scala-parser-combinators" % scalaParserCombinatorsVersion,
//  "org.typelevel" %% "spire" % "0.18.0",
//  "org.typelevel" %% "cats-kernel" % catsVersion,
//  "org.typelevel" %% "cats-core" % catsVersion,
//  "org.typelevel" %% "discipline-scalatest" % "2.3.0" % "test",
//  "com.novocode" % "junit-interface" % "0.11" % "test", // NOTE vulnerability here
//  "org.scalacheck" %% "scalacheck" % "1.19.0" % "test", // This is used for testing Rational
//  "org.typelevel" %% "cats-laws" % catsVersion % "test",
//  "org.typelevel" %% "algebra-laws" % catsVersion % "test",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "ch.qos.logback" % "logback-classic" % "1.5.20" % "runtime"
)

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

