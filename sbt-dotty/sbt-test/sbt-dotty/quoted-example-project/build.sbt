scalaVersion := sys.props("plugin.scalaVersion")

libraryDependencies ++= Seq(
      "ch.epfl.lamp" %% "dotty-staging" % scalaVersion.value,
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )
