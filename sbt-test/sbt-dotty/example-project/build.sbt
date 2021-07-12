scalaVersion := sys.props("plugin.scalaVersion")

libraryDependencies +=
  ("org.scala-lang.modules" %% "scala-xml" % "1.2.0").cross(CrossVersion.for3Use2_13)
