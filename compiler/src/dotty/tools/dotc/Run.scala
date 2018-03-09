package dotty.tools
package dotc

import core._
import Contexts._
import Periods._
import Symbols._
import Phases._
import Types._
import Scopes._
import typer.{FrontEnd, ImportInfo, RefChecks, Typer}
import Decorators._
import io.{AbstractFile, PlainFile}

import scala.io.Codec
import util.{Set => _, _}
import reporting.Reporter
import transform.TreeChecker
import rewrite.Rewrites
import java.io.{BufferedWriter, OutputStreamWriter}

import dotty.tools.dotc.profile.Profiler
import printing.XprintMode
import parsing.Parsers.Parser
import typer.ImplicitRunInfo
import collection.mutable

import scala.annotation.tailrec
import dotty.tools.io.VirtualFile

import scala.util.control.NonFatal

/** A compiler run. Exports various methods to compile source files */
class Run(comp: Compiler, ictx: Context) extends ImplicitRunInfo with ConstraintRunInfo {

  /** Produces the following contexts, from outermost to innermost
   *
   *    bootStrap:   A context with next available runId and a scope consisting of
   *                 the RootPackage _root_
   *    start        A context with RootClass as owner and the necessary initializations
   *                 for type checking.
   *    imports      For each element of RootImports, an import context
   */
  protected[this] def rootContext(implicit ctx: Context): Context = {
    ctx.initialize()(ctx)
    ctx.base.setPhasePlan(comp.phases)
    val rootScope = new MutableScope
    val bootstrap = ctx.fresh
      .setPeriod(Period(comp.nextRunId, FirstPhaseId))
      .setScope(rootScope)
    rootScope.enter(ctx.definitions.RootPackage)(bootstrap)
    val start = bootstrap.fresh
      .setOwner(defn.RootClass)
      .setTyper(new Typer)
      .addMode(Mode.ImplicitsEnabled)
      .setTyperState(new TyperState(ctx.typerState))
      .setFreshNames(new FreshNameCreator.Default)
    ctx.initialize()(start) // re-initialize the base context with start
    def addImport(ctx: Context, refFn: () => TermRef) =
      ctx.fresh.setImportInfo(ImportInfo.rootImport(refFn)(ctx))
    (start.setRun(this) /: defn.RootImportFns)(addImport)
  }

  private[this] var compiling = false

  private[this] var myCtx = rootContext(ictx)

  /** The context created for this run */
  def runContext = myCtx

  protected[this] implicit def ctx: Context = myCtx
  assert(ctx.runId <= Periods.MaxPossibleRunId)

  private[this] var myUnits: List[CompilationUnit] = _
  private[this] var myUnitsCached: List[CompilationUnit] = _
  private[this] var myFiles: Set[AbstractFile] = _

  /** The compilation units currently being compiled, this may return different
    *  results over time.
    */
  def units: List[CompilationUnit] = myUnits

  private def units_=(us: List[CompilationUnit]): Unit =
    myUnits = us

  /** The files currently being compiled, this may return different results over time.
    *  These files do not have to be source files since it's possible to compile
    *  from TASTY.
    */
  def files: Set[AbstractFile] = {
    if (myUnits ne myUnitsCached) {
      myUnitsCached = myUnits
      myFiles = myUnits.map(_.source.file).toSet
    }
    myFiles
  }

  /** The source files of all late entered symbols, as a set */
  private[this] var lateFiles = mutable.Set[AbstractFile]()

  /** Actions that need to be performed at the end of the current compilation run */
  private[this] var finalizeActions = mutable.ListBuffer[() => Unit]()

  def getSource(fileName: String): SourceFile = {
    val f = new PlainFile(io.Path(fileName))
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

  def compileUnits(us: List[CompilationUnit]): Unit = {
    units = us
    compileUnits()
  }

  def compileUnits(us: List[CompilationUnit], ctx: Context): Unit = {
    units = us
    compileUnits()(ctx)
  }

  protected def compileUnits()(implicit ctx: Context) = Stats.maybeMonitored {
    ctx.base.checkSingleThreaded()
    compiling = true

    // If testing pickler, make sure to stop after pickling phase:
    val stopAfter =
      if (ctx.settings.YtestPickler.value) List("pickler")
      else ctx.settings.YstopAfter.value

    val phases = ctx.base.squashPhases(ctx.base.phasePlan,
      ctx.settings.Yskip.value, ctx.settings.YstopBefore.value, stopAfter, ctx.settings.Ycheck.value)
    ctx.base.usePhases(phases)

    def runPhases(implicit ctx: Context) = {
      var lastPrintedTree: PrintedTree = NoPrintedTree
      val profiler = ctx.profiler

      for (phase <- ctx.base.allPhases)
        if (phase.isRunnable)
          Stats.trackTime(s"$phase ms ") {
            val start = System.currentTimeMillis
            val profileBefore = profiler.beforePhase(phase)
            units = phase.runOn(units)
            profiler.afterPhase(phase, profileBefore)
            if (ctx.settings.Xprint.value.containsPhase(phase)) {
              for (unit <- units) {
                lastPrintedTree =
                  printTree(lastPrintedTree)(ctx.fresh.setPhase(phase.next).setCompilationUnit(unit))
              }
            }
            ctx.informTime(s"$phase ", start)
            Stats.record(s"total trees at end of $phase", ast.Trees.ntrees)
            for (unit <- units)
              Stats.record(s"retained typed trees at end of $phase", unit.tpdTree.treeSize)
          }

      profiler.finished()
    }

    val runCtx = ctx.fresh
    runCtx.setProfiler(Profiler())
    ctx.phases.foreach(_.initContext(runCtx))
    runPhases(runCtx)
    if (!ctx.reporter.hasErrors) Rewrites.writeBack()
    while (finalizeActions.nonEmpty) {
      val action = finalizeActions.remove(0)
      action()
    }
    compiling = false
  }

  /** Enter top-level definitions of classes and objects contain in Scala source file `file`.
   *  The newly added symbols replace any previously entered symbols.
   *  If `typeCheck = true`, also run typer on the compilation unit.
   */
  def lateCompile(file: AbstractFile, typeCheck: Boolean)(implicit ctx: Context): Unit =
    if (!files.contains(file) && !lateFiles.contains(file)) {
      lateFiles += file
      val unit = new CompilationUnit(getSource(file.path))
      def process()(implicit ctx: Context) = {
        unit.untpdTree = new Parser(unit.source).parse()
        ctx.typer.lateEnter(unit.untpdTree)
        def typeCheckUnit() = unit.tpdTree = ctx.typer.typedExpr(unit.untpdTree)
        if (typeCheck)
          if (compiling) finalizeActions += (() => typeCheckUnit()) else typeCheckUnit()
      }
      process()(runContext.fresh.setCompilationUnit(unit))
    }

  private sealed trait PrintedTree
  private /*final*/ case class SomePrintedTree(phase: String, tree: String) extends PrintedTree
  private object NoPrintedTree extends PrintedTree

  private def printTree(last: PrintedTree)(implicit ctx: Context): PrintedTree = {
    val unit = ctx.compilationUnit
    val prevPhase = ctx.phase.prev // can be a mini-phase
    val squashedPhase = ctx.base.squashed(prevPhase)
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

  /** Print summary; return # of errors encountered */
  def printSummary(): Reporter = {
    printMaxConstraint()
    val r = ctx.reporter
    r.printSummary
    r
  }

  override def reset() = {
    super[ImplicitRunInfo].reset()
    super[ConstraintRunInfo].reset()
    myCtx = null
    myUnits = null
    myUnitsCached = null
  }
}
