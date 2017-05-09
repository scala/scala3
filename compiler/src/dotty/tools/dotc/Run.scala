package dotty.tools
package dotc

import core._
import Contexts._
import Periods._
import Symbols._
import Phases._
import Decorators._
import dotty.tools.dotc.transform.TreeTransforms.TreeTransformer
import io.PlainFile
import scala.io.Codec
import util._
import reporting.Reporter
import transform.TreeChecker
import rewrite.Rewrites
import java.io.{BufferedWriter, OutputStreamWriter}
import printing.XprintMode

import scala.annotation.tailrec
import dotty.tools.io.VirtualFile
import scala.util.control.NonFatal

/** A compiler run. Exports various methods to compile source files */
class Run(comp: Compiler)(implicit ctx: Context) {

  assert(comp.phases.last.last.id <= Periods.MaxPossiblePhaseId)
  assert(ctx.runId <= Periods.MaxPossibleRunId)

  var units: List[CompilationUnit] = _

  def getSource(fileName: String): SourceFile = {
    val f = new PlainFile(fileName)
    if (f.isDirectory) {
      ctx.error(s"expected file, received directory '$fileName'")
      NoSource
    } else if (f.exists) {
      val encoding = ctx.settings.encoding.value
      new SourceFile(f, Codec(encoding))
    } else {
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

    // If testing pickler, make sure to stop after pickling phase:
    val stopAfter =
      if (ctx.settings.YtestPickler.value) List("pickler")
      else ctx.settings.YstopAfter.value

    val phases = ctx.squashPhases(ctx.phasePlan,
      ctx.settings.Yskip.value, ctx.settings.YstopBefore.value, stopAfter, ctx.settings.Ycheck.value)
    ctx.usePhases(phases)
    var lastPrintedTree: PrintedTree = NoPrintedTree
    for (phase <- ctx.allPhases)
      if (!ctx.reporter.hasErrors) {
        val start = System.currentTimeMillis
        units = phase.runOn(units)
        if (ctx.settings.Xprint.value.containsPhase(phase)) {
          for (unit <- units) {
            lastPrintedTree =
              printTree(lastPrintedTree)(ctx.fresh.setPhase(phase.next).setCompilationUnit(unit))
          }
        }
        ctx.informTime(s"$phase ", start)
      }
    if (!ctx.reporter.hasErrors) Rewrites.writeBack()
    for (unit <- units)
      Stats.record("retained typed trees at end", unit.tpdTree.treeSize)
    Stats.record("total trees at end", ast.Trees.ntrees)
  }

  private sealed trait PrintedTree
  private final case class SomePrintedTree(phase: String, tree: String) extends PrintedTree
  private object NoPrintedTree extends PrintedTree

  private def printTree(last: PrintedTree)(implicit ctx: Context): PrintedTree = {
    val unit = ctx.compilationUnit
    val prevPhase = ctx.phase.prev // can be a mini-phase
    val squashedPhase = ctx.squashed(prevPhase)
    val treeString = unit.tpdTree.show(ctx.withProperty(XprintMode, Some(())))

    ctx.echo(s"result of $unit after $squashedPhase:")

    last match {
      case SomePrintedTree(phase, lastTreeSting) if lastTreeSting != treeString =>
        val msg =
          if (!ctx.settings.XprintDiff.value && !ctx.settings.XprintDiffDel.value) treeString
          else DiffUtil.mkColoredCodeDiff(treeString, lastTreeSting, ctx.settings.XprintDiffDel.value)
        ctx.echo(msg)
        SomePrintedTree(squashedPhase.toString, treeString)

      case SomePrintedTree(phase, lastTreeSting) =>
        ctx.echo("  Unchanged since " + phase)
        last

      case NoPrintedTree =>
        ctx.echo(treeString)
        SomePrintedTree(squashedPhase.toString, treeString)
    }
  }

  def compile(sourceCode: String): Unit = {
    val virtualFile = new VirtualFile(sourceCode) // use source code as name as it's used for equals
    val writer = new BufferedWriter(new OutputStreamWriter(virtualFile.output, "UTF-8")) // buffering is still advised by javadoc
    writer.write(sourceCode)
    writer.close()
    compileSources(List(new SourceFile(virtualFile, Codec.UTF8)))
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
