ThisBuild / usePipelining := true

lazy val a = project.in(file("a"))
  .settings(
    scalacOptions += "-Ycheck:all",
  )

// same as a, but does not use pipelining
lazy val a_alt = project.in(file("a_alt"))
  .settings(
    Compile / sources := (a / Compile / sources).value,
    Compile / exportPipelining := false,
  )


// m defines a macro depending on a, it also tries to use the macro in the same project,
// which will fail because A.class is not available when running the macro,
// because the dependency on a is pipelined.
lazy val m = project.in(file("m"))
  .dependsOn(a)
  .settings(
    scalacOptions += "-Ycheck:all",
  )

// same as m, but depends on a_alt, so it will compile
// because A.class will be available when running the macro.
lazy val m_alt = project.in(file("m_alt"))
  .dependsOn(a_alt)
  .settings(
    Compile / sources := (m / Compile / sources).value,
    scalacOptions += "-Ycheck:all",
  )
