import xsbti.VirtualFileRef
import sbt.internal.inc.Analysis
import xsbti.compile.{PreviousResult, CompileAnalysis, MiniSetup}
import xsbti.compile.analysis.{ Compilation => XCompilation }

previousCompile in Compile := {
  val previous = (previousCompile in Compile).value
  if (!CompileState.isNew) {
    val res = PreviousResult.of(none[CompileAnalysis].asJava, none[MiniSetup].asJava)
    CompileState.isNew = true
    res
  } else previous
}

/* Performs checks related to compilations:
 *  a) checks in which compilation given set of files was recompiled
 *  b) checks overall number of compilations performed
 */
TaskKey[Unit]("checkCompilations") := {
  val analysis = (compile in Compile).value match { case a: Analysis => a }
  val srcDir = (scalaSource in Compile).value
  def findFile(className: String): VirtualFileRef = {
    analysis.relations.definesClass(className).head
  }
  val allCompilations = analysis.compilations.allCompilations
  val recompiledFiles: Seq[Set[VirtualFileRef]] = allCompilations map { c: XCompilation =>
    val recompiledFiles = analysis.apis.internal.collect {
      case (cn, api) if api.compilationTimestamp == c.getStartTime => findFile(cn)
    }
    recompiledFiles.toSet
  }
  def recompiledFilesInIteration(iteration: Int, fileNames: Set[String]) = {
    assert(recompiledFiles(iteration).map(_.name) == fileNames,
      s"""${recompiledFiles(iteration).map(_.name)} != $fileNames
         |
         |allCompilations = $allCompilations
         |""".stripMargin)
  }
  assert(allCompilations.size == 2, s"All compilations is ${allCompilations.size}")
  // B.scala is just compiled at the beginning
  recompiledFilesInIteration(0, Set("B.scala"))
  // A.scala is changed and recompiled
  recompiledFilesInIteration(1, Set("A.scala"))
}

logLevel := Level.Debug
