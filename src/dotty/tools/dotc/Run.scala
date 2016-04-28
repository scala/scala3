package dotty.tools
package dotc

import core._
import Contexts._, Periods._, Symbols._, Phases._, Decorators._
import dotty.tools.dotc.transform.TreeTransforms.TreeTransformer
import io.PlainFile
import util.{SourceFile, NoSource, Stats, SimpleMap}
import reporting.Reporter
import transform.TreeChecker
import rewrite.Rewrites
import java.io.{BufferedWriter, OutputStreamWriter}
import scala.reflect.io.VirtualFile
import scala.util.control.NonFatal

/** A compiler run. Exports various methods to compile source files */
class Run(comp: Compiler)(implicit ctx: Context) {

  assert(comp.phases.last.last.id <= Periods.MaxPossiblePhaseId)
  assert(ctx.runId <= Periods.MaxPossibleRunId)

  var units: List[CompilationUnit] = _

  def getSource(fileName: String): SourceFile = {
    val f = new PlainFile(fileName)
    if (f.exists) new SourceFile(f)
    else {
      ctx.error(s"not found: $fileName")
      NoSource
    }
  }

  def compile(fileNames: List[String]): Unit = try {
    val sources = fileNames map getSource
    compileSources(sources)
  } catch {
    case NonFatal(ex) =>
      ctx.echo(i"exception occurred while compiling $units%, %")
      throw ex
  }

  /** TODO: There's a fundamental design problem here: We assemble phases using `squash`
   *  when we first build the compiler. But we modify them with -Yskip, -Ystop
   *  on each run. That modification needs to either transform the tree structure,
   *  or we need to assemble phases on each run, and take -Yskip, -Ystop into
   *  account. I think the latter would be preferable.
   */
  def compileSources(sources: List[SourceFile]) =
    if (sources forall (_.exists)) {
      units = sources map (new CompilationUnit(_))
      compileUnits()
    }

  protected def compileUnits() = Stats.monitorHeartBeat {
    ctx.checkSingleThreaded()
    val phases = ctx.squashPhases(ctx.phasePlan,
      ctx.settings.Yskip.value, ctx.settings.YstopBefore.value, ctx.settings.YstopAfter.value, ctx.settings.Ycheck.value)
    ctx.usePhases(phases)
    for (phase <- ctx.allPhases)
      if (!ctx.reporter.hasErrors) {
        val start = System.currentTimeMillis
        units = phase.runOn(units)
        def foreachUnit(op: Context => Unit)(implicit ctx: Context): Unit =
          for (unit <- units) op(ctx.fresh.setPhase(phase.next).setCompilationUnit(unit))
        if (ctx.settings.Xprint.value.containsPhase(phase))
          foreachUnit(printTree)
        ctx.informTime(s"$phase ", start)
      }
    if (!ctx.reporter.hasErrors) Rewrites.writeBack()
  }

  private def printTree(ctx: Context) = {
    val unit = ctx.compilationUnit
    val prevPhase = ctx.phase.prev // can be a mini-phase
    val squashedPhase = ctx.squashed(prevPhase)

    ctx.echo(s"result of $unit after ${squashedPhase}:")
    ctx.echo(unit.tpdTree.show(ctx))
  }

  def compile(sourceCode: String): Unit = {
    val virtualFile = new VirtualFile(sourceCode) // use source code as name as it's used for equals
    val writer = new BufferedWriter(new OutputStreamWriter(virtualFile.output, "UTF-8")) // buffering is still advised by javadoc
    writer.write(sourceCode)
    writer.close()
    compileSources(List(new SourceFile(virtualFile)))
  }

  /** The context created for this run */
  def runContext = ctx

  /** Print summary; return # of errors encountered */
  def printSummary(): Reporter = {
    ctx.runInfo.printMaxConstraint()
    val r = ctx.reporter
    r.printSummary
    r
  }
}
