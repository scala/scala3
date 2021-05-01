lazy val scala2Lib = project.in(file("scala2Lib"))
  .settings(
    scalaVersion := "2.13.5"
  )

lazy val dottyApp = project.in(file("dottyApp"))
  .dependsOn(scala2Lib)
  .settings(
    scalaVersion := sys.props("plugin.scalaVersion")
  )
