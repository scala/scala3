import sbt.internal.inc.Analysis
import xsbti.VirtualFileRef
import xsbti.api.AnalyzedClass
import xsbti.compile.{PreviousResult, CompileAnalysis, MiniSetup}
import xsbti.compile.analysis.{ Compilation => XCompilation }

logLevel := Level.Debug

// Reset compile status because scripted tests are run in batch mode
previousCompile in Compile := {
  val previous = (previousCompile in Compile).value
  if (!CompileState.isNew) {
    val res = PreviousResult.of(none[CompileAnalysis].asJava, none[MiniSetup].asJava)
    CompileState.isNew = true
    res
  } else previous
}

// disable sbt's heuristic which recompiles everything in case
// some fraction (e.g. 50%) of files is scheduled to be recompiled
// in this test we want precise information about recompiled files
// which that heuristic would distort
incOptions := incOptions.value.withRecompileAllFraction(1.0)

Global / allowMachinePath := false

/* Performs checks related to compilations:
 *  a) checks in which compilation given set of files was recompiled
 *  b) checks overall number of compilations performed
 */
TaskKey[Unit]("checkCompilations") := {
  val log = streams.value.log
  val c = fileConverter.value
  val vs = (Compile / sources).value.toVector map { x =>
    c.toVirtualFile(x.toPath)
  }
  // log.info(vs.mkString(","))

  val analysis = (compile in Compile).value match { case a: Analysis => a }
  val srcDir = (scalaSource in Compile).value
  def findFile(className: String): VirtualFileRef = {
    analysis.relations.definesClass(className).head
  }
  val allCompilations: Seq[XCompilation] = analysis.compilations.allCompilations
  log.info(s"allCompilations: $allCompilations")
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
  // Y.scala is compiled only at the beginning as changes to A.scala do not affect it
  recompiledFilesInIteration(0, Set("X.scala", "Y.scala"))
  // A.scala is changed and recompiled
  recompiledFilesInIteration(1, Set("A.scala"))
  // change in A.scala causes recompilation of B.scala, C.scala, D.scala which depend on transitively
  // and by inheritance on A.scala
  // X.scala is also recompiled because it depends by member reference on B.scala
  // Note that Y.scala is not recompiled because it depends just on X through member reference dependency
  recompiledFilesInIteration(2, Set("B.scala", "C.scala", "D.scala"))
  assert(allCompilations.size == 3)
}
