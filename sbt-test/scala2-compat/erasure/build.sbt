ThisBuild / fork := true

lazy val scala2Lib = project.in(file("scala2Lib"))
  .settings(
    scalaVersion := sys.props("plugin.scala2Version")
  )

lazy val dottyApp = project.in(file("dottyApp"))
  .dependsOn(scala2Lib)
  .settings(
    scalaVersion := sys.props("plugin.scalaVersion")
  )
