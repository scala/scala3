val scala3Version = sys.props("plugin.scalaVersion")
val scala2Version = sys.props("plugin.scala2Version")

lazy val lib = project.in(file("lib"))
  .settings(
    scalaVersion := scala2Version
  )

lazy val app = project.in(file("app"))
  .dependsOn(lib)
  .settings(
    scalaVersion := scala3Version
  )
