package dotty.tools
package dotc

import core._
import Contexts._
import Periods._
import Symbols._
import Scopes._
import typer.{FrontEnd, Typer, Mode, ImportInfo, RefChecks}
import reporting.ConsoleReporter
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.transform._
import dotty.tools.dotc.transform.TreeTransforms.{TreeTransform, TreeTransformer}
import dotty.tools.dotc.core.DenotTransformers.DenotTransformer
import dotty.tools.dotc.core.Denotations.SingleDenotation


import dotty.tools.backend.jvm.{LabelDefs, GenBCode}

class Compiler {

  /** Meta-ordering constraint:
   *
   *  DenotTransformers that change the signature of  their denotation's info must go
   *  after erasure. The reason is that denotations are permanently referred to by
   *  TermRefs which contain a signature. If the signature of a symbol would change,
   *  all refs to it would become outdated - they could not be dereferenced in the
   *  new phase.
   *
   *  After erasure, signature changing denot-transformers are OK because erasure
   *  will make sure that only term refs with fixed SymDenotations survive beyond it. This
   *  is possible because:
   *
   *   - splitter has run, so every ident or select refers to a unique symbol
   *   - after erasure, asSeenFrom is the identity, so every reference has a
   *     plain SymDenotation, as opposed to a UniqueRefDenotation.
   */
  def phases: List[List[Phase]] =
    List(
      List(new FrontEnd),
      List(new FirstTransform,
           new SyntheticMethods),
      List(new SuperAccessors),
      //List(new Pickler), // Pickler needs to come last in a group since it should not pickle trees generated later
      List(new RefChecks,
           new ElimRepeated,
           new ElimLocals,
           new ExtensionMethods),
      List(new TailRec), // TailRec needs to be in its own group for now.
                         // Otherwise it produces -Ycheck incorrect code for
                         // file core/Decorators.scala.
      List(new PatternMatcher,
           new ExplicitOuter,
           new Splitter),
      List(new LazyVals,
           new ElimByName,
           new SeqLiterals,
           new InterceptedMethods,
           new Literalize,
           new Getters,
           new ResolveSuper),
      List(new Erasure),
      List(new Mixin,
           new Memoize,
           new CapturedVars,
           new Constructors),
      List(new LambdaLift,
           new Flatten,
           new RestoreScopes),
      List(/*new PrivateToStatic,*/ new CollectEntryPoints, new LabelDefs, new ElimWildcardIdents),
      List(new GenBCode)
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
    ctx.setPhasePlan(phases)
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