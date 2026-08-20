enablePlugins(ScalaJSPlugin)

scalaVersion := sys.props("plugin.scalaVersion")

val checkMainClasses = taskKey[Unit]("Checks the main classes discovered by the compiler bridge")

checkMainClasses := {
  val actual = (Compile / discoveredMainClasses).value.toSet
  val expected = Set("runscala.MainScala")

  assert(actual == expected, s"Expected $expected main classes, got $actual")
}
