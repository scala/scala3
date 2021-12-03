lazy val lib = project.in(file("lib"))
  .settings(
    scalaVersion := "3.0.2"
  )

lazy val app = project.in(file("app"))
  .dependsOn(lib)
