name := "number-top"

//scalaVersion := "3.7.3"

//scalacOptions ++= Seq("-encoding", "UTF-8", "-unchecked", "-deprecation", "-source:future-migration", "-rewrite", "-Vprint:typer")

//Compile / excludeFilter := {
//  val default = (Compile / excludeFilter).value
//  default || new SimpleFileFilter(_.getPath.contains("com/phasmidsoftware/number3/cats"))
//}

//Test / unmanagedSourceDirectories += baseDirectory.value / "it/scala"

lazy val scalaTestVersion = "3.2.19"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "ch.qos.logback" % "logback-classic" % "1.5.20" % "runtime"
)

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

