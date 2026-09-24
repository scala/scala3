// Add personal SBT plugins for IDEs, etc to `local-plugins.sbt`
//
// e.g. addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.1.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.22.0")

addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.3.1")

// addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.7")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.2")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.6")

addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.11.7")

// addSbtPlugin("com.github.sbt" % "sbt-jdi-tools" % "1.2.0")

addSbtPlugin("ch.epfl.scala" % "sbt-missinglink" % "0.3.8")
libraryDependencies += "com.spotify" % "missinglink-core" % "0.2.11"
