package dotty.tools
package dotc

import core._
import Contexts._, Periods._, Symbols._, Phases._, Decorators._
import io.PlainFile
import util.{SourceFile, NoSource, Stats, SimpleMap}
import reporting.Reporter
import transform.TreeChecker
import java.io.{BufferedWriter, OutputStreamWriter}
import scala.reflect.io.VirtualFile

class Run(comp: Compiler)(implicit ctx: Context) {

  var units: List[CompilationUnit] = _

  def getSource(fileName: String): SourceFile = {
    val f = new PlainFile(fileName)
    if (f.exists) new SourceFile(f)
    else {
      ctx.error(s"not found: $fileName")
      NoSource
    }
  }

  def compile(fileNames: List[String]): Unit = {
    val sources = fileNames map getSource
    compileSources(sources)
  }

  /** TODO: There's a fundamental design problem here: We assmble phases using `squash`
   *  when we first build the compiler. But we modify them with -Yskip, -Ystop
   *  on each run. That modification needs to either trasnform the tree structure,
   *  or we need to assmeble phases on each run, and take -Yskip, -Ystop into
   *  account. I think the latter would be preferable.
   */
  def compileSources(sources: List[SourceFile]) = Stats.monitorHeartBeat {
    if (sources forall (_.exists)) {
      units = sources map (new CompilationUnit(_))
      def stoppedBefore(phase: Phase) =
        ctx.settings.YstopBefore.value.containsPhase(phase) ||
        ctx.settings.YstopAfter.value.containsPhase(phase.prev)
      val phasesToRun = ctx.allPhases.init
        .takeWhile(!stoppedBefore(_))
        .filterNot(ctx.settings.Yskip.value.containsPhase(_)) // TODO: skip only subphase
      for (phase <- phasesToRun)
        if (!ctx.reporter.hasErrors) {
          phase.runOn(units)
          def foreachUnit(op: Context => Unit)(implicit ctx: Context): Unit =
            for (unit <- units) op(ctx.fresh.setPhase(phase.next).setCompilationUnit(unit))
          if (ctx.settings.Xprint.value.containsPhase(phase))
            foreachUnit(printTree)
          if (ctx.settings.Ycheck.value.containsPhase(phase) && !ctx.reporter.hasErrors) {
            assert(phase.isCheckable, s"phase $phase is not checkable")
            foreachUnit(TreeChecker.check(phasesToRun, _))
          }
        }
    }
  }

  private def printTree(ctx: Context) = {
    val unit = ctx.compilationUnit
    println(s"result of $unit after ${ctx.phase.prev}:")
    println(unit.tpdTree.show(ctx))
  }

  def compile(sourceCode: String): Unit = {
    val virtualFile = new VirtualFile(sourceCode) // use source code as name as it's used for equals
    val writer = new BufferedWriter(new OutputStreamWriter(virtualFile.output, "UTF-8")) // buffering is still advised by javadoc
    writer.write(sourceCode)
    writer.close()
    compileSources(List(new SourceFile(virtualFile)))
  }

  /** Print summary; return # of errors encountered */
  def printSummary(): Reporter = {
    ctx.runInfo.printMaxConstraint()
    val r = ctx.typerState.reporter
    r.printSummary
    r
  }
}