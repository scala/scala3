lazy val a = project.in(file("a"))
  .settings(
    scalaVersion := sys.props("plugin.scalaVersion")
  )
lazy val b = project.in(file("b"))
  .settings(Reporter.checkSettings)
  .settings(
    scalaVersion := sys.props("plugin.scalaVersion"),
    // Manually depend on `a` so that we can:
    // 1. Compile `a`
    // 2. Remove a source file in `a`
    // 3. Compile `b` without forcing a recompilation of `a`
    Compile / unmanagedJars := Def.uncached {
      val s = Attributed.blank((a / Compile / packageBin).value) :: Nil
      println("s: " + s)
      s
    }
  )
