import complete.DefaultParsers._

InputKey[Unit]("check-number-of-compiler-iterations") := {
  val args = spaceDelimited("<arg>").parsed
  val a = (compile in Compile).value.asInstanceOf[sbt.internal.inc.Analysis]
  assert(args.size == 1)
  val expectedIterationsNumber = args(0).toInt
  assert(a.compilations.allCompilations.size == expectedIterationsNumber, "a.compilations.allCompilations.size = %d (expected %d)".format(a.compilations.allCompilations.size, expectedIterationsNumber))
}
