scalaVersion := sys.props("plugin.scalaVersion")

libraryDependencies +=
   ("org.atnos" %% "eff" % "5.4.1").withDottyCompat(scalaVersion.value)
