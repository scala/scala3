def toFileRef(f: File)(using conv: xsbti.FileConverter): xsbti.HashedVirtualFileRef =
  conv.toVirtualFile(f.toPath)

lazy val commonSettings = Seq(
  scalacOptions += "-preview",
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
    Compile / unmanagedClasspath += Attributed.blank {
      given xsbti.FileConverter = fileConverter.value
      toFileRef(((ThisBuild / baseDirectory).value / "b-input"))
    },
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
    // scalacOptions ++= Seq("-from-tasty", "-Ycheck:readTasty", "-Werror", "-Vprint:readTasty", "-Xprint-types"),
    // Compile / sources := Seq(new java.io.File("c-input/B.tasty")),
    Compile / unmanagedClasspath += Attributed.blank {
      given xsbti.FileConverter = fileConverter.value
      toFileRef(((ThisBuild / baseDirectory).value / "c-input"))
    },
    Compile / classDirectory := (ThisBuild / baseDirectory).value / "c-output"
  )
