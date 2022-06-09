lazy val lib = project.in(file("lib"))
  .settings(
    scalaVersion := "3.1.1"
  )

lazy val app = project.in(file("app"))
  .dependsOn(lib)
