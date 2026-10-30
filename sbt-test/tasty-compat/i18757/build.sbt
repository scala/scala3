lazy val a = project.in(file("a"))
  .settings(
    Compile / classDirectory := (ThisBuild / baseDirectory).value / "a-output"
  )

lazy val b = project.in(file("b"))
  .settings(
    scalacOptions += "-Xsemanticdb",
    Compile / unmanagedClasspath += (ThisBuild / baseDirectory).value / "a-output",
    Compile / classDirectory := (ThisBuild / baseDirectory).value / "b-output"
  )
