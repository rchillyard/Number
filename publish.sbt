// Optional but useful
ThisBuild / description := "Fuzzy, Lazy Scala library for numerical computation"
ThisBuild / homepage := Some(url("https://github.com/rchillyard/Number"))
ThisBuild / organizationName := "Phasmid Software"
ThisBuild / organizationHomepage := Some(url("https://phasmidsoftware.com/"))

// Publishing configuration
ThisBuild / publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}
ThisBuild / publishMavenStyle := true

// Credentials
ThisBuild / credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credentials")

// Required POM metadata for Maven Central
ThisBuild / licenses := List("MIT" -> url("https://opensource.org/licenses/MIT"))
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

// Optional

// Publishing settings
// NOTE pvp, not early-semver. Under early-semver, sbt and Coursier treat x.(y+1) as binary
// compatible with x.y and evict silently, which gives a downstream NoSuchMethodError when a minor
// release breaks compatibility -- as Number's do. Under pvp the first TWO numbers form the major
// version, so a patch release is compatible and a minor release may break, with a release note.
// MiMa (see build.sbt) is the enforcement half of this.
ThisBuild / versionScheme := Some("pvp")
ThisBuild / publishConfiguration := publishConfiguration.value.withOverwrite(true)
ThisBuild / publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true)


// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }

// sbt's lint wrongly reports these ThisBuild publish settings as unused
Global / excludeLintKeys ++= Set(pomIncludeRepository, publishMavenStyle)
