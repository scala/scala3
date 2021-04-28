lazy val dep = project.in(file("dep"))
lazy val use = project.in(file("use")).
  settings(
    unmanagedJars in Compile := Attributed.blank(packageBin.in(dep, Compile).value) :: Nil
  )
