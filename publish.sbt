ThisBuild / organization := "com.phasmidsoftware"
ThisBuild / organizationName := "Phasmid Software"
ThisBuild / organizationHomepage := Some(url("https://phasmidsoftware.com/"))

// (deprecated):  ThisBuild / useGpg := true

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/rchillyard/Number"),
    "scm:git@github.com:rchillyard/Number.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id    = "rchillyard",
    name  = "Robin Hillyard",
    email = "rchillyard@phasmidsoftware.com",
    url   = url("https://phasmidsoftware.com")
  )
)

ThisBuild / description := "Fuzzy, Lazy Scala library for numerical computation"
ThisBuild / licenses := List("MIT" -> url("http://opensource.org/licenses/MIT"))
ThisBuild / homepage := Some(url("https://github.com/rchillyard/Number"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val nexus = "https://ossrh-staging-api.central.sonatype.com"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true

