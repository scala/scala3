ThisBuild / scalaVersion := sys.props("plugin.scalaVersion")
ThisBuild / organization := "org.example"

lazy val aJVM = project
  .settings(
    name := "a",
    version := "0.5.1-SNAPSHOT",
    libraryDependencies := Seq(),  // don't depend on scala-library
  )

lazy val aJS = project
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "a",
    version := "0.5.1-SNAPSHOT",
    libraryDependencies := Seq(),  // don't depend on scala-library
  )

lazy val bJVM = project
  .settings(onlyThisTestResolverSettings)
  .settings(
    name := "b",
    libraryDependencies := Seq(organization.value %%% "a" % "0.5.0-SNAPSHOT"),
  )

lazy val bJS = project
  .enablePlugins(ScalaJSPlugin)
  .settings(onlyThisTestResolverSettings)
  .settings(
    name := "b",
    libraryDependencies := Seq(organization.value %%% "a" % "0.5.0-SNAPSHOT"),
  )
