ThisBuild / organization := "com.phasmidsoftware"

name := "Number"

ThisBuild / version := "1.3.2"

lazy val root = (project in file("."))
    .aggregate(core, algebra, expression)
    .settings(
      name := "number"
    )

lazy val core = (project in file("core"))
    .settings(
      scalaVersion := "2.13.16",
    )

lazy val algebra = (project in file("algebra"))
    .settings(
      scalaVersion := "3.7.3",
    ).dependsOn(core)

lazy val expression = (project in file("expression"))
    .settings(
      scalaVersion := "3.7.3",
    ).dependsOn(core, algebra)

scalaVersion := "2.13.16"

scalacOptions ++= Seq("-encoding", "UTF-8", "-unchecked", "-deprecation")

Test / parallelExecution := false

// NOTE: if you reinstate these directories, you will need to manage the large crimes file (see code).
//Test / unmanagedSourceDirectories += baseDirectory.value / "src/it/scala"
//Test / unmanagedResourceDirectories += baseDirectory.value / "src/it/resources"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"
