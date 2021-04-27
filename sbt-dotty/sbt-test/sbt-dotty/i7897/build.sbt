scalaVersion := sys.props("plugin.scalaVersion")

libraryDependencies += "org.scala-lang" %% "scala3-staging" % scalaVersion.value

fork := true
