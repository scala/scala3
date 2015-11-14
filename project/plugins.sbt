// Add personal SBT plugins for IDEs, etc to `local-plugins.sbt`
//
// e.g. addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.1.0")

// Scala IDE project file generator
addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "4.0.0")

// See https://github.com/scalastyle/scalastyle/issues/156#issuecomment-137229733
addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.7.0" excludeAll(
  ExclusionRule(organization = "com.danieltrinh")))

libraryDependencies += "org.scalariform" %% "scalariform" % "0.1.7"
