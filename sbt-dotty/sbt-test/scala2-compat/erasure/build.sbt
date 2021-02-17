lazy val scala2Lib = project.in(file("scala2Lib"))
  .settings(
    scalaVersion := "2.13.2"
  )

lazy val dottyApp = project.in(file("dottyApp"))
  .dependsOn(scala2Lib)
  .settings(
    scalaVersion := sys.props("plugin.scalaVersion"),
    // https://github.com/sbt/sbt/issues/5369
    projectDependencies := {
      projectDependencies.value.map(_.withDottyCompat(scalaVersion.value))
    }
  )

