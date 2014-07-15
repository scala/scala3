package dotty.tools
package dotc

import core._
import Contexts._
import Periods._
import Symbols._
import Scopes._
import typer.{FrontEnd, Typer, Mode, ImportInfo}
import reporting.ConsoleReporter
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.transform._
import dotty.tools.dotc.transform.TreeTransforms.{TreeTransform, TreeTransformer}
import dotty.tools.dotc.transform.PostTyperTransformers.PostTyperTransformer
import dotty.tools.dotc.core.DenotTransformers.DenotTransformer
import dotty.tools.dotc.core.Denotations.SingleDenotation

class Compiler {

  def phases: List[List[Phase]] =
    List(
      List(new FrontEnd),
      List(new SuperAccessors),
      List(new LazyValsCreateCompanionObjects,
           new TailRec),    //force separataion between lazyVals and LVCreateCO
      List(new PatternMatcher,
           new LazyValTranformContext().transformer,
           new Splitter),
      List(new Nullarify,
           new TypeTestsCasts,
           new InterceptedMethods),
      List(new Erasure),
      List(new UncurryTreeTransform
        /* , new Constructors */)
    )

  var runId = 1
  def nextRunId = {
    runId += 1; runId
  }

  /** Produces the following contexts, from outermost to innermost
   *
   *    bootStrap:   A context with next available runId and a scope consisting of
   *                 the RootPackage _root_
   *    start        A context with RootClass as owner and the necessary initializations
   *                 for type checking.
   *    imports      For each element of RootImports, an import context
   */
  def rootContext(implicit ctx: Context): Context = {
    ctx.definitions.init(ctx)
    ctx.usePhases(phases)
    val rootScope = new MutableScope
    val bootstrap = ctx.fresh
      .setPeriod(Period(nextRunId, FirstPhaseId))
      .setScope(rootScope)
    rootScope.enter(ctx.definitions.RootPackage)(bootstrap)
    val start = bootstrap.fresh
      .setOwner(defn.RootClass)
      .setTyper(new Typer)
      .setMode(Mode.ImplicitsEnabled)
      .setTyperState(new MutableTyperState(ctx.typerState, new ConsoleReporter()(ctx), isCommittable = true))
    ctx.definitions.init(start) // set context of definitions to start
    def addImport(ctx: Context, sym: Symbol) =
      ctx.fresh.setImportInfo(ImportInfo.rootImport(sym)(ctx))
    (start.setRunInfo(new RunInfo(start)) /: defn.RootImports)(addImport)
  }

  def newRun(implicit ctx: Context): Run = {
    try new Run(this)(rootContext)
    finally {
      ctx.base.reset()
      ctx.runInfo.clear()
    }
  }
}