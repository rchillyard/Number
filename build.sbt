ThisBuild / organization := "com.phasmidsoftware"

name := "Number"

ThisBuild / version := "1.3.1"

lazy val root = (project in file("."))
    .aggregate(core, algebra)
    .settings(
      name := "my-project"
    )

lazy val core = (project in file("core"))
    .settings(
      scalaVersion := "2.13.16",
      // other Scala 2 specific settings
    )

lazy val algebra = (project in file("algebra"))
    .settings(
      scalaVersion := "3.7.3",
      // other Scala 3 specific settings
    ).dependsOn(core)

scalaVersion := "2.13.16"

scalacOptions ++= Seq("-encoding", "UTF-8", "-unchecked", "-deprecation")

Test / parallelExecution := false

// NOTE: if you reinstate these directories, you will need to manage the large crimes file (see code).
//Test / unmanagedSourceDirectories += baseDirectory.value / "src/it/scala"
//Test / unmanagedResourceDirectories += baseDirectory.value / "src/it/resources"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"
