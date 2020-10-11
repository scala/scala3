scalaVersion := sys.props("plugin.scalaVersion")

libraryDependencies +=
  ("com.twitter" %% "finagle-core" % "20.9.0").withDottyCompat(scalaVersion.value)
