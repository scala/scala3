/* Performs checks related to compilations:
 *  a) checks in which compilation given set of files was recompiled
 *  b) checks overall number of compilations performed
 */
TaskKey[Unit]("check-compilations") := {
  val analysis = (compile in Compile).value.asInstanceOf[sbt.internal.inc.Analysis]
  val allCompilations = analysis.compilations.allCompilations
  val recompiledClasses: Seq[Set[String]] = allCompilations map { c =>
    val recompiledClasses = analysis.apis.internal.collect {
      case (clazz, api) if api.compilationTimestamp() == c.getStartTime() => clazz
    }
    recompiledClasses.toSet
  }
  def recompiledFilesInIteration(iteration: Int, classNames: Set[String]): Unit = {
    assert(recompiledClasses(iteration) == classNames, "%s != %s".format(recompiledClasses(iteration), classNames))
  }
  assert(allCompilations.size == 2)
  // B.scala is just compiled at the beginning
  recompiledFilesInIteration(0, Set("B"))
  // A.scala is changed and recompiled
  recompiledFilesInIteration(1, Set("A"))
}

logLevel := Level.Debug
