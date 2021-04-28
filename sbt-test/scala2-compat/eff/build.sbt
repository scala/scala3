scalaVersion := sys.props("plugin.scalaVersion")

libraryDependencies +=
  ("org.atnos" %% "eff" % "5.5.2").cross(CrossVersion.for3Use2_13)
