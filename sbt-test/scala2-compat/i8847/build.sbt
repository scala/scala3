val scala3Version = sys.props("plugin.scalaVersion")
val scala2Version = "2.13.5"

lazy val `i8847-lib` = (project in file ("lib"))
  .settings(scalaVersion := scala2Version)

lazy val `i8847-test` = (project in file ("main"))
  .dependsOn(`i8847-lib`)
  .settings(
    scalaVersion := scala3Version
  )
