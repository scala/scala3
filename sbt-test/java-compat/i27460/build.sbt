// must be run with JDK 21 or later
scalaVersion := sys.props("plugin.scalaVersion")

// sbt by default points scala.ext.dirs at an export of the full JDK runtime (~/.sbt/1.0/java9-rt-ext-*/rt.jar), which the
// Scala 3 compiler adds to its classpath. That provides the real jdk.internal.ValueBased and hides the bug.
Global / onLoad := { (s: State) => System.clearProperty("scala.ext.dirs"); s } compose (Global / onLoad).value

lazy val root = project
  .in(file("."))
  .settings(
    // -release older than the JDK running sbt (21): the JDK's ct.sym lacks jdk.internal.ValueBased
    scalacOptions ++= Seq("-release", "17")
  )
