def toFileRef(f: File)(using conv: xsbti.FileConverter): xsbti.HashedVirtualFileRef =
  conv.toVirtualFile(f.toPath)

lazy val a = project.in(file("a"))
  .settings(
    Compile / classDirectory := (ThisBuild / baseDirectory).value / "b-input"
  )

lazy val b = project.in(file("b"))
  .settings(
    Compile / unmanagedClasspath += Attributed.blank {
      given xsbti.FileConverter = fileConverter.value
      toFileRef(((ThisBuild / baseDirectory).value / "b-input"))
    },
    Compile / classDirectory := (ThisBuild / baseDirectory).value / "c-input"
  )

lazy val `a-changes` = project.in(file("a-changes"))
  .settings(
    Compile / classDirectory := (ThisBuild / baseDirectory).value / "c-input"
  )

lazy val c = project.in(file("."))
  .settings(
    scalacOptions ++= Seq("-from-tasty", "-Ycheck:readTasty", "-Werror"),
    Compile / sources := Seq(new java.io.File("c-input/B.tasty")),
    Compile / unmanagedClasspath += Attributed.blank {
      given xsbti.FileConverter = fileConverter.value
      toFileRef(((ThisBuild / baseDirectory).value / "c-input"))
    },
    Compile / classDirectory := (ThisBuild / baseDirectory).value / "c-output"
  )
