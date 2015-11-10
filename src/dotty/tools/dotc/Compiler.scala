package dotty.tools
package dotc

import core._
import Contexts._
import Periods._
import Symbols._
import Types._
import Scopes._
import typer.{FrontEnd, Typer, Mode, ImportInfo, RefChecks}
import reporting.{Reporter, ConsoleReporter}
import Phases.Phase
import transform._
import transform.TreeTransforms.{TreeTransform, TreeTransformer}
import core.DenotTransformers.DenotTransformer
import core.Denotations.SingleDenotation

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
      List(new PostTyper),
      List(new Pickler),
      List(new FirstTransform,
           new CheckReentrant),
      List(new RefChecks,
           new ElimRepeated,
           new NormalizeFlags,
           new ExtensionMethods,
           new ExpandSAMs,
           new TailRec,
           new LiftTry,
           new ClassOf),
      List(new PatternMatcher,
           new ExplicitOuter,
           new ExplicitSelf,
           new CrossCastAnd,
           new Splitter),
      List(new VCInlineMethods,
           new SeqLiterals,
           new InterceptedMethods,
           new Getters,
           new ClassTags,
           new ElimByName,
           new AugmentScala2Traits,
           new ResolveSuper),
      List(new Erasure),
      List(new ElimErasedValueType,
           new VCElideAllocations,
           new Mixin,
           new LazyVals,
           new Memoize,
           new LinkScala2ImplClasses,
           new NonLocalReturns,
           new CapturedVars, // capturedVars has a transformUnit: no phases should introduce local mutable vars here
           new Constructors, // constructors changes decls in transformTemplate, no InfoTransformers should be added after it
           new FunctionalInterfaces,
           new GetClass), // getClass transformation should be applied to specialized methods
      List(new LambdaLift,   // in this mini-phase block scopes are incorrect. No phases that rely on scopes should be here
           new ElimStaticThis,
           new Flatten,
           new RestoreScopes),
      List(/*new PrivateToStatic,*/
           new ExpandPrivate,
           new CollectEntryPoints,
           new LabelDefs),
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
      .setTyperState(new MutableTyperState(ctx.typerState, rootReporter(ctx), isCommittable = true))
    ctx.definitions.init(start) // set context of definitions to start
    def addImport(ctx: Context, refFn: () => TermRef) =
      ctx.fresh.setImportInfo(ImportInfo.rootImport(refFn)(ctx))
    (start.setRunInfo(new RunInfo(start)) /: defn.RootImportFns)(addImport)
  }

  protected def rootReporter(implicit ctx: Context): Reporter = new ConsoleReporter()(ctx)

  def reset()(implicit ctx: Context): Unit = {
    ctx.base.reset()
    ctx.runInfo.clear()
  }

  def newRun(implicit ctx: Context): Run = {
    reset()
    new Run(this)(rootContext)
  }
}
