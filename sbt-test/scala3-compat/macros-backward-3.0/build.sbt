lazy val checkOptions = Seq("-Xcheck-macros", "-Ycheck:all", "-Yno-double-bindings")

lazy val lib = project.in(file("lib"))
  .settings(
    scalaVersion := "3.0.2",
    scalacOptions ++= checkOptions,
  )

lazy val app = project.in(file("app"))
  .dependsOn(lib)
  .settings(
    scalacOptions ++= checkOptions,
  )
