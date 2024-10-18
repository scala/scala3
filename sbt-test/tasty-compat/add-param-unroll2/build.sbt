lazy val commonSettings = Seq(
  scalacOptions += "-experimental",
)

lazy val printSettings = Seq(
  scalacOptions += "-Yprint-tasty",
  scalacOptions += "-Ydebug-error"
)

lazy val a_v1 = project.in(file("a_v1"))
  .settings(commonSettings)
  .settings(
    Compile / classDirectory := (ThisBuild / baseDirectory).value / "v2-input"
  )

lazy val a_v2 = project.in(file("a_v2"))
  .settings(commonSettings)
  .settings(printSettings)
  .settings(
    Compile / unmanagedClasspath += (ThisBuild / baseDirectory).value / "v2-input",
    Compile / classDirectory := (ThisBuild / baseDirectory).value / "v3-input"
  )

lazy val a_v3 = project.in(file("a_v3"))
  .settings(commonSettings)
  .settings(printSettings)
  .settings(
    Compile / unmanagedClasspath += (ThisBuild / baseDirectory).value / "v3-input",
    Compile / classDirectory := (ThisBuild / baseDirectory).value / "v3-output"
  )

lazy val a_v3_2 = project.in(file("a_v3_2"))
  .settings(commonSettings)
  .settings(printSettings)
  .settings(
    Compile / unmanagedClasspath += (ThisBuild / baseDirectory).value / "v3-input",
    Compile / classDirectory := (ThisBuild / baseDirectory).value / "v3_2-output"
  )
