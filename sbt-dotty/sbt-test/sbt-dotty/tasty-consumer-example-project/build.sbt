lazy val dottyVersion = sys.props("plugin.scalaVersion")

lazy val lib = project
  .in(file("lib"))
  .settings(
    scalaVersion := dottyVersion
  )

lazy val app = project
  .in(file("app"))
  .settings(
    scalaVersion := dottyVersion,
    libraryDependencies += "ch.epfl.lamp" %% "dotty-tasty-consumer" % scalaVersion.value,
  )
  .dependsOn(lib)
