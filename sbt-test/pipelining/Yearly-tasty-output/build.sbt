// NOTE: in this test, we are explictly fixing the classpath of project `c` to be `a-early.jar:b-early-out`
// to manually test pipelining without sbt/zinc managing the classpath.

// early out is a jar
lazy val a = project.in(file("a"))
  .settings(
    scalacOptions ++= Seq("-Yearly-tasty-output", ((ThisBuild / baseDirectory).value / "a-early.jar").toString),
    scalacOptions += "-Ycheck:all",
  )

// early out is a directory
lazy val b = project.in(file("b"))
  .settings(
    scalacOptions ++= Seq("-Yearly-tasty-output", ((ThisBuild / baseDirectory).value / "b-early-out").toString),
    scalacOptions += "-Ycheck:all",
  )

// reads classpaths from early tasty outputs. No need for extra flags as the full tasty is available.
lazy val c = project.in(file("c"))
  .settings(
    Compile / unmanagedClasspath += Attributed.blank((ThisBuild / baseDirectory).value / "a-early.jar"),
    Compile / unmanagedClasspath += Attributed.blank((ThisBuild / baseDirectory).value / "b-early-out"),
    scalacOptions += "-Ycheck:all",
  )
