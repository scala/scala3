val scala3Version = sys.props("plugin.scalaVersion")
val scala2Version = "2.13.5"

lazy val `i9916a-lib` = (project in file ("lib"))
  .settings(scalaVersion := scala2Version)

lazy val `i9916a-test` = (project in file ("main"))
  .dependsOn(`i9916a-lib`)
  .settings(
    scalaVersion := scala3Version
  )
