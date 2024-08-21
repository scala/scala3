ThisBuild / usePipelining := true

// m defines a macro depending on b.B, it also tries to use the macro in the same project,
// which will succeed even though B.class is not available when running the macro,
// because compilation can suspend until B is available.
lazy val m = project.in(file("m"))
  .settings(
    scalacOptions += "-Ycheck:all",
  )
