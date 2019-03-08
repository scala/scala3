scalaVersion := "2.12.7"
scalacOptions += "-Yrangepos"
addCompilerPlugin("org.scalameta" % "semanticdb-scalac" % "4.0.0" cross CrossVersion.full)
