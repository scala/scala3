scalaVersion := sys.props("plugin.scalaVersion")

libraryDependencies +=
  ("com.lihaoyi" %% "fastparse" % "2.1.3").cross(CrossVersion.for3Use2_13)

scalacOptions += "-Xignore-scala2-macros"