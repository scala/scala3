scalaVersion := sys.props("plugin.scalaVersion")

libraryDependencies += "ch.epfl.lamp" %% "dotty-staging" % scalaVersion.value

fork := true
