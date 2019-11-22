scalaVersion := sys.props("plugin.scalaVersion")

libraryDependencies +=
   ("org.atnos" %% "eff" % "5.5.2").withDottyCompat(scalaVersion.value)
