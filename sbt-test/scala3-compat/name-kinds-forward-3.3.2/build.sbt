lazy val lib = project.in(file("lib"))

lazy val app = project.in(file("app"))
  .dependsOn(lib)
  .settings(
    scalaVersion := "3.3.2"
  )