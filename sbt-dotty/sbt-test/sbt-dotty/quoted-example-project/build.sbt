scalaVersion := sys.props("plugin.scalaVersion")

libraryDependencies ++= Seq(
      "ch.epfl.lamp" %% "dotty-compiler" % scalaVersion.value,
      "ch.epfl.lamp" %% "dotty-compiler" % scalaVersion.value % "test->runtime",
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )
