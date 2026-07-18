lazy val lib = project.in(file("lib"))
  .settings(
    scalaVersion := "3.3.7",
  )

// Should fail to compile
lazy val appNeg = project.in(file("app-neg"))
  .settings(
    scalaVersion := "3.8.1",
    (Compile / sources) ++= (app / Compile / sources).value
  )
  .dependsOn(lib)

// Should compile
lazy val app = project.in(file("app"))
  .settings(
    scalaVersion := sys.props("plugin.scalaVersion")
  )
  .dependsOn(lib)

