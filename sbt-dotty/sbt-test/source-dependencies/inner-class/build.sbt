import complete.DefaultParsers._

val checkIterations = inputKey[Unit]("Verifies the accumlated number of iterations of incremental compilation.")

checkIterations := {
  val analysis = (compile in Compile).value.asInstanceOf[sbt.internal.inc.Analysis]

  val expected: Int = (Space ~> NatBasic).parsed
  val actual: Int = analysis.compilations.allCompilations.size
  assert(expected == actual, s"Expected $expected compilations, got $actual")
}

