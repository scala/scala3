val scala3Version = sys.props("plugin.scalaVersion")
val scala2Version = "2.13.5"

lazy val lib = (project in file ("lib"))
  .settings(scalaVersion := scala2Version)

lazy val test = (project in file ("main"))
  .dependsOn(lib)
  .settings(scalaVersion := scala3Version)
