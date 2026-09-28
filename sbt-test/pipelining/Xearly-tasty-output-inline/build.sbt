// NOTE: in this test, we are explictly fixing the classpath of project `b` to be `a-early.jar`
// to manually test pipelining without sbt/zinc managing the classpath.

// defines a inline method.
lazy val a = project.in(file("a"))
  .settings(
    scalacOptions ++= Seq("-Xearly-tasty-output", ((ThisBuild / baseDirectory).value / "a-early.jar").toString),
    scalacOptions += "-Ycheck:all",
  )

// uses the inline method, this is fine as there is no macro classloader involved
lazy val b = project.in(file("b"))
  .settings(
    Compile / unmanagedClasspath += Attributed.blank((ThisBuild / baseDirectory).value / "a-early.jar"),
    scalacOptions += "-Ycheck:all",
  )
