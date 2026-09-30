scalaVersion := sys.props("plugin.scalaVersion")

val checkMainClasses = taskKey[Unit]("Checks the main classes discovered by the compiler bridge")

checkMainClasses := {
  val actual = (Compile / discoveredMainClasses).value.toSet
  val alwaysExpected = Set(
    "runscala.MainScala",
    "runscala.foo"
  )
  val fromJava25 = Set(
    "runscala.InheritTrait",
    "runscala.NoArgs",
    "runscala.NoStatic",
    "runscala.Parameterless",
    "runscala.Protected",
    "runscala.ProtectedStatic",
    "runscala.SecondaryNoArgs",
    "runscala.StaticNoArgs"
  )
  val expected =
    if (scala.util.Properties.isJavaAtLeast("25")) alwaysExpected ++ fromJava25
    else alwaysExpected

  assert(actual == expected, s"Expected $expected main classes, got $actual")
}
