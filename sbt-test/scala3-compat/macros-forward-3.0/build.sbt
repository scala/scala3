lazy val checkOptions = Seq("-Xcheck-macros", "-Ycheck:all", "-Yno-double-bindings")

lazy val lib = project.in(file("lib"))
  .settings(
    scalacOptions ++= Seq("-scala-output-version", "3.0") ++ checkOptions
  )

lazy val app = project.in(file("app"))
  .dependsOn(lib)
  .settings(
    scalaVersion := "3.0.2",
    scalacOptions ++= checkOptions,
    dependencyOverrides += scalaOrganization.value %% "scala3-library" % scalaVersion.value,
  )
