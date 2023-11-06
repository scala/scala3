lazy val a = project.in(file("a"))
  .settings(
    compileOrder := CompileOrder.Mixed, // ensure we send java sources to Scala compiler
    scalacOptions += "-Yjava-tasty", // enable pickling of java signatures
    scalacOptions ++= Seq("-Yjava-tasty-output", ((ThisBuild / baseDirectory).value / "a-enum-java-tasty.jar").toString),
    scalacOptions += "-Ycheck:all",
    classDirectory := ((ThisBuild / baseDirectory).value / "a-enum-classes"), // send classfiles to a different directory
  )


lazy val b = project.in(file("b"))
  .settings(
    Compile / unmanagedClasspath := Seq(Attributed.blank((ThisBuild / baseDirectory).value / "a-enum-java-tasty.jar")),
    scalacOptions += "-Ycheck:all",
  )
