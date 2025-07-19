lazy val commonSettings = Seq(
  scalacOptions += "-experimental",
)

lazy val printSettings = Seq(
  scalacOptions += "-Yprint-tasty",
)

lazy val a = project.in(file("a"))
  .settings(commonSettings)
  .settings(
    Compile / classDirectory := (ThisBuild / baseDirectory).value / "b-input"
  )

lazy val b = project.in(file("b"))
  .settings(commonSettings)
  .settings(
    Compile / unmanagedClasspath += (ThisBuild / baseDirectory).value / "b-input",
    Compile / classDirectory := (ThisBuild / baseDirectory).value / "c-input"
  )

lazy val `a-changes` = project.in(file("a-changes"))
  .settings(commonSettings)
  .settings(
    Compile / classDirectory := (ThisBuild / baseDirectory).value / "c-input"
  )

lazy val c = project.in(file("c"))
  .settings(commonSettings)
  .settings(printSettings)
  .settings(
    // scalacOptions ++= Seq("-from-tasty", "-Ycheck:readTasty", "-Xfatal-warnings", "-Vprint:readTasty", "-Xprint-types"),
    // Compile / sources := Seq(new java.io.File("c-input/B.tasty")),
    Compile / unmanagedClasspath += (ThisBuild / baseDirectory).value / "c-input",
    Compile / classDirectory := (ThisBuild / baseDirectory).value / "c-output"
  )
