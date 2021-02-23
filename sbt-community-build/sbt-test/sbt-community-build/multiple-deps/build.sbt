ThisBuild / scalaVersion := sys.props("plugin.scalaVersion")
ThisBuild / organization := "org.example"

lazy val a = project
  .settings(
    name := "a",
    version := "0.2.1-SNAPSHOT",
    libraryDependencies := Seq(),  // don't depend on scala-library
  )

lazy val b = project
  .settings(
    name := "b",
    version := "1.2.1-SNAPSHOT",
    libraryDependencies := Seq(),  // don't depend on scala-library
  )

lazy val c = project
  .settings(onlyThisTestResolverSettings)
  .settings(
    name := "c",
    libraryDependencies := Seq(
      organization.value %% "a" % "0.2.0-SNAPSHOT",
      organization.value %% "b" % "1.2.0-SNAPSHOT",
    ),
  )
