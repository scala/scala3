lazy val scala2Lib = project.in(file("scala2Lib"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaVersion := sys.props("plugin.scala2ForJSVersion")
  )

lazy val dottyApp = project.in(file("dottyApp"))
  .dependsOn(scala2Lib)
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaVersion := sys.props("plugin.scalaVersion"),

    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= (_.withCheckIR(true)),
  )
