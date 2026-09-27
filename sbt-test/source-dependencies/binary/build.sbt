lazy val dep = project.in(file("dep"))
lazy val use = project.in(file("use")).
  settings(
    Compile / unmanagedJars := Def.uncached {
      val converter = fileConverter.value
      Attributed.blank((dep / Compile / packageBin).value) :: Nil
    }
  )
