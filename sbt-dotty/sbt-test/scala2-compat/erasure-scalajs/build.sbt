lazy val scala2Lib = project.in(file("scala2Lib"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    // TODO: switch to 2.13.5 once we've upgrade sbt-scalajs to 1.5.0
    scalaVersion := "2.13.4"
  )

lazy val dottyApp = project.in(file("dottyApp"))
  .dependsOn(scala2Lib)
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaVersion := sys.props("plugin.scalaVersion"),

    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= (_.withCheckIR(true)),
  )
