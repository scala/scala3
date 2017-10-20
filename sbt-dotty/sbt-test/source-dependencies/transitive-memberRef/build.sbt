logLevel := Level.Debug

// disable sbt's heuristic which recompiles everything in case
// some fraction (e.g. 50%) of files is scheduled to be recompiled
// in this test we want precise information about recompiled files
// which that heuristic would distort
incOptions := incOptions.value.withRecompileAllFraction(1.0)

/* Performs checks related to compilations:
 *  a) checks in which compilation given set of files was recompiled
 *  b) checks overall number of compilations performed
 */
TaskKey[Unit]("check-compilations") := {
  val analysis = (compile in Compile).value.asInstanceOf[sbt.internal.inc.Analysis]
  val srcDir = (scalaSource in Compile).value
  val allCompilations = analysis.compilations.allCompilations
  val recompiledClasses: Seq[Set[String]] = allCompilations map { c =>
    val recompiledClasses = analysis.apis.internal.collect {
      case (clazz, api) if api.compilationTimestamp() == c.getStartTime() => clazz
    }
    recompiledClasses.toSet
  }
  def recompiledClassesInIteration(iteration: Int, classNames: Set[String]): Unit = {
    assert(recompiledClasses(iteration) == classNames, "%s != %s".format(recompiledClasses(iteration), classNames))
  }
  // test.Y is compiled only at the beginning as changes to test.A do not affect it
  recompiledClassesInIteration(0, Set("test.X", "test.Y"))
  // test.A is changed and recompiled
  recompiledClassesInIteration(1, Set("test.A"))
  // change in test.A causes recompilation of test.B, test.C, test.D which depend on transitively
  // and by inheritance on test.A
  // test.X is also recompiled because it depends by member reference on test.B
  // Note that test.Y is not recompiled because it depends just on X through member reference dependency
  recompiledClassesInIteration(2, Set("test.B", "test.C", "test.D"))
  assert(allCompilations.size == 3)
}
