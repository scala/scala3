lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-simple",
    version := "0.1.0",
    scalaVersion := sys.props("plugin.scalaVersion"),
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
