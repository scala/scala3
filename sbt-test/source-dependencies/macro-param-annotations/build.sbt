ThisBuild / scalaVersion := sys.props("plugin.scalaVersion")

lazy val a = project.in(file("a"))
lazy val root = project.in(file(".")).dependsOn(a)
