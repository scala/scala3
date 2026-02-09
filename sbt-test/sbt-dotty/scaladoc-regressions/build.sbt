ThisBuild / scalaVersion := sys.props("plugin.scalaVersion")

lazy val i20476 = project
  .in(file("i20476"))
  .enablePlugins(ScalaJSPlugin)

lazy val i18231 = project
  .in(file("i18231"))
  .settings(scalacOptions += "-release:17")
