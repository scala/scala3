autoCompilerPlugins := true

addCompilerPlugin("org.scala-lang.plugins" % "continuations" % "2.9.2")

scalaVersion := "2.9.2"

scalacOptions += "-P:continuations:enable"