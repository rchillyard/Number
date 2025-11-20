name := "number-parse"

scalaVersion := "3.7.3"

lazy val scalaTestVersion = "3.2.19"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "3.1.1",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "ch.qos.logback" % "logback-classic" % "1.5.20" % "runtime"
)

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

