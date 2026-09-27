def toFileRef(f: File)(using conv: xsbti.FileConverter): xsbti.HashedVirtualFileRef =
  conv.toVirtualFile(f.toPath)

lazy val a = project.in(file("a"))
  .settings(
    Compile / classDirectory := (ThisBuild / baseDirectory).value / "a-output"
  )

lazy val b = project.in(file("b"))
  .settings(
    Compile / unmanagedClasspath += Attributed.blank {
      given xsbti.FileConverter = fileConverter.value
      toFileRef(((ThisBuild / baseDirectory).value / "b-input"))
    }
  )
