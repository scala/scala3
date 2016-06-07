import complete.DefaultParsers._

val checkIterations = inputKey[Unit]("Verifies the accumlated number of iterations of incremental compilation.")

checkIterations := {
        val expected: Int = (Space ~> NatBasic).parsed
        val actual: Int = (compile in Compile).value.compilations.allCompilations.size
        assert(expected == actual, s"Expected $expected compilations, got $actual")
}

