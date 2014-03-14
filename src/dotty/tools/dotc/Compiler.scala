package dotty.tools
package dotc

import core._
import Contexts._
import Periods._
import Symbols._
import typer.{FrontEnd, Typer, Mode, ImportInfo}
import reporting.ConsoleReporter
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.transform.{LazyValsCreateCompanionObjects, LazyValTranformContext}
import dotty.tools.dotc.transform.TreeTransforms.{TreeTransform, TreeTransformer}
import dotty.tools.dotc.transform.PostTyperTransformers.PostTyperTransformer

class Compiler {

  def postTyperTransform = new PostTyperTransformer {
    override def name: String = "PostTyperTransformations"
    override protected def transformations: Array[(TreeTransformer, Int) => TreeTransform] =
    Array(new LazyValsCreateCompanionObjects(_, _))
  }

  def lazyValsPhase = new TreeTransformer with DenotTransformer{

    val lazyValsContext = new LazyValTranformContext()
    override protected def transformations: Array[(TreeTransformer, Int) => TreeTransform] = {

      Array(lazyValsContext.transformer(_, _))
    }

    override def name: String = "LazyVals"
  }

  def phases: List[Phase] = List(new FrontEnd, postTyperTransform, lazyValsPhase, new transform.SamplePhase)

  var runId = 1
  def nextRunId = {
    runId += 1; runId
  }

  def rootContext(implicit ctx: Context): Context = {
    ctx.definitions.init(ctx)
    ctx.usePhases(phases)
    val start = ctx.fresh
      .withPeriod(Period(nextRunId, FirstPhaseId))
      .withOwner(defn.RootClass)
      .withTyper(new Typer)
      .withNewMode(Mode.ImplicitsEnabled)
      .withTyperState(new MutableTyperState(ctx.typerState, new ConsoleReporter()(ctx), isCommittable = true))
    ctx.definitions.init(start)
    def addImport(ctx: Context, sym: Symbol) =
      ctx.fresh.withImportInfo(ImportInfo.rootImport(sym)(ctx))
    (start.withRunInfo(new RunInfo(start)) /: defn.RootImports)(addImport)
  }

  def newRun(implicit ctx: Context): Run = {
    try new Run(this)(rootContext)
    finally {
      ctx.base.reset()
      ctx.runInfo.clear()
    }
  }
}