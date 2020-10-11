val scala3Version = sys.props("plugin.scalaVersion")
val scala2Version = "2.13.3"

ThisBuild / organization := "test.dotty"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val `i9916b-lib` = (project in file ("lib"))
  .settings(scalaVersion := scala2Version)

lazy val `i9916b-test` = (project in file ("main"))
  .settings(
    scalaVersion := scala3Version,
    libraryDependencies += (organization.value %% "i9916b-lib" % version.value).withDottyCompat(scalaVersion.value)
  )
