enablePlugins(ScalaJSPlugin)

scalaVersion := sys.props("plugin.scalaVersion")

libraryDependencies += "org.scalameta" %%% "munit" % "1.0.0-M10"
