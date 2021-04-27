scalaVersion := sys.props("plugin.scalaVersion")

libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-staging" % scalaVersion.value,
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )
