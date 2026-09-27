import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

enablePlugins(ScalaJSPlugin)

scalaVersion := sys.props("plugin.scalaVersion")

libraryDependencies += "org.scalameta" %% "munit" % "1.1.0"
