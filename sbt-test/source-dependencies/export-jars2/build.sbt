lazy val a = Project("a", file("a"))
  .settings(
    exportJars := true
  )

lazy val b = Project("b", file("b"))
  .dependsOn(a)
  .settings(
    exportJars := true
  )
