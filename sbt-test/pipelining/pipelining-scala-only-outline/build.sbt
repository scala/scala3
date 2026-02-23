ThisBuild / usePipelining := true

lazy val a = project.in(file("a"))
  .settings(
    scalacOptions += "-Xexperimental-outline",
    scalacOptions += "-Xmax-parallelism:1",
    scalacOptions += "-Ycheck:all",
  )

lazy val b = project.in(file("b"))
  .dependsOn(a)
  .settings(
    scalacOptions += "-Xexperimental-outline",
    scalacOptions += "-Xmax-parallelism:1",
    scalacOptions += "-Ycheck:all",
  )
