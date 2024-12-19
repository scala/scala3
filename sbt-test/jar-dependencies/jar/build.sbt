ThisBuild / organization := "com.example"

lazy val a = project

lazy val b = project
  .settings(
    Compile / unmanagedJars += (a / Compile / packageBin).map(Attributed.blank).value
  )
