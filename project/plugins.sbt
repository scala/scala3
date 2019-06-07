// Add personal SBT plugins for IDEs, etc to `local-plugins.sbt`
//
// e.g. addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.1.0")

// Scala IDE project file generator
addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "5.2.4")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.0.0-M7")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "2.1")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.0")

addSbtPlugin("org.xerial.sbt" % "sbt-pack" % "0.10.1")

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.3.2")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.9.0")
