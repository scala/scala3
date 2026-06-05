// Add personal SBT plugins for IDEs, etc to `local-plugins.sbt`
//
// e.g. addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.1.0")

// some plugins haven't moved to scala-xml 2.x yet
libraryDependencySchemes +=
  "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.22.0")

addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.3.1")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.1")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.5")

addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.11.7")

// MissingLinkPlugin is vendored in project/MissingLinkPlugin.scala (upstream sbt2 port not yet on Maven Central)
libraryDependencies += "com.spotify" % "missinglink-core" % "0.2.11"
libraryDependencies += "org.ow2.asm" % "asm-tree" % "9.9"
