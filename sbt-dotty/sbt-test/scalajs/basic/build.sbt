enablePlugins(ScalaJSPlugin)

scalaVersion := sys.props("plugin.scalaVersion")

// Test withDottyCompat for %%% dependencies
libraryDependencies += ("org.scala-js" %%% "scalajs-dom" % "1.1.0").cross(CrossVersion.for3Use2_13)

scalaJSUseMainModuleInitializer := true
