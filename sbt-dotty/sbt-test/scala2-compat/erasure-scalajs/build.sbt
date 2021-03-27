lazy val scala2Lib = project.in(file("scala2Lib"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaVersion := "2.13.5"
  )

lazy val dottyApp = project.in(file("dottyApp"))
  .dependsOn(scala2Lib)
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaVersion := sys.props("plugin.scalaVersion"),

    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= (_.withCheckIR(true)),
  )
