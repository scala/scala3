val scala3Version = sys.props("plugin.scalaVersion")
val scala2Version = sys.props("plugin.scala2Version")

lazy val lib1 = project.in(file("lib1"))
  .settings(
    scalaVersion := scala2Version
  )

lazy val lib2 = project.in(file("lib2"))
  .settings(
    scalaVersion := scala2Version
  )

lazy val app1fail = project.in(file("app1fail"))
  .dependsOn(lib1)
  .settings(
    scalaVersion := scala3Version
  )

lazy val app1ok = project.in(file("app1ok"))
  .dependsOn(lib1)
  .settings(
    scalaVersion := scala3Version
  )

lazy val app2fail = project.in(file("app2fail"))
  .dependsOn(lib2)
  .settings(
    scalaVersion := scala3Version
  )
