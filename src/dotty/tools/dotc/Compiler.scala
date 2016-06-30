package dotty.tools
package dotc

import core._
import Contexts._
import Periods._
import Symbols._
import Types._
import Scopes._
import typer.{FrontEnd, Typer, ImportInfo, RefChecks}
import reporting.{Reporter, ConsoleReporter}
import Phases.Phase
import transform._
import transform.TreeTransforms.{TreeTransform, TreeTransformer}
import core.DenotTransformers.DenotTransformer
import core.Denotations.SingleDenotation

import dotty.tools.backend.jvm.{LabelDefs, GenBCode}
import dotty.tools.backend.sjs.GenSJSIR

/** The central class of the dotc compiler. The job of a compiler is to create
 *  runs, which process given `phases` in a given `rootContext`.
 */
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
      List(new FrontEnd),           // Compiler frontend: scanner, parser, namer, typer
      List(new sbt.ExtractDependencies), // Sends information on classes' dependencies to sbt via callbacks
      List(new PostTyper),          // Additional checks and cleanups after type checking
      List(new sbt.ExtractAPI),     // Sends a representation of the API of classes to sbt via callbacks
      List(new Pickler),            // Generate TASTY info
      List(new FirstTransform,      // Some transformations to put trees into a canonical form
           new CheckReentrant),     // Internal use only: Check that compiled program has no data races involving global vars
      List(new RefChecks,           // Various checks mostly related to abstract members and overriding
           new CheckStatic,         // Check restrictions that apply to @static members
           new ElimRepeated,        // Rewrite vararg parameters and arguments
           new NormalizeFlags,      // Rewrite some definition flags
           new ExtensionMethods,    // Expand methods of value classes with extension methods
           new ExpandSAMs,          // Expand single abstract method closures to anonymous classes
           new TailRec,             // Rewrite tail recursion to loops
           new LiftTry,             // Put try expressions that might execute on non-empty stacks into their own methods
           new ClassOf),            // Expand `Predef.classOf` calls.
      List(new TryCatchPatterns,    // Compile cases in try/catch
           new PatternMatcher,      // Compile pattern matches
           new ExplicitOuter,       // Add accessors to outer classes from nested ones.
           new ExplicitSelf,        // Make references to non-trivial self types explicit as casts
           new CrossCastAnd,        // Normalize selections involving intersection types.
           new Splitter),           // Expand selections involving union types into conditionals
      List(new VCInlineMethods,     // Inlines calls to value class methods
           new IsInstanceOfEvaluator, // Issues warnings when unreachable statements are present in match/if expressions
           new SeqLiterals,         // Express vararg arguments as arrays
           new InterceptedMethods,  // Special handling of `==`, `|=`, `getClass` methods
           new Getters,             // Replace non-private vals and vars with getter defs (fields are added later)
           new ElimByName,          // Expand by-name parameters and arguments
           new AugmentScala2Traits, // Expand traits defined in Scala 2.11 to simulate old-style rewritings
           new ResolveSuper,        // Implement super accessors and add forwarders to trait methods
           new ArrayConstructors),  // Intercept creation of (non-generic) arrays and intrinsify.
      List(new Erasure),            // Rewrite types to JVM model, erasing all type parameters, abstract types and refinements.
      List(new ElimErasedValueType, // Expand erased value types to their underlying implmementation types
           new VCElideAllocations,  // Peep-hole optimization to eliminate unnecessary value class allocations
           new Mixin,               // Expand trait fields and trait initializers
           new LazyVals,            // Expand lazy vals
           new Memoize,             // Add private fields to getters and setters
           new LinkScala2ImplClasses, // Forward calls to the implementation classes of traits defined by Scala 2.11
           new NonLocalReturns,     // Expand non-local returns
           new CapturedVars,        // Represent vars captured by closures as heap objects
           new Constructors,        // Collect initialization code in primary constructors
                                       // Note: constructors changes decls in transformTemplate, no InfoTransformers should be added after it
           new FunctionalInterfaces,// Rewrites closures to implement @specialized types of Functions.
           new GetClass),           // Rewrites getClass calls on primitive types.
      List(new LambdaLift,          // Lifts out nested functions to class scope, storing free variables in environments
                                       // Note: in this mini-phase block scopes are incorrect. No phases that rely on scopes should be here
           new ElimStaticThis,      // Replace `this` references to static objects by global identifiers
           new Flatten,             // Lift all inner classes to package scope
           new RestoreScopes),      // Repair scopes rendered invalid by moving definitions in prior phases of the group
      List(new ExpandPrivate,       // Widen private definitions accessed from nested classes
           new CollectEntryPoints,  // Find classes with main methods
           new MoveStatics,         // Move static methods to companion classes
           new LabelDefs),          // Converts calls to labels to jumps
      List(new GenSJSIR),           // Generate .js code
      List(new GenBCode)            // Generate JVM bytecode
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
    ctx.initialize()(ctx)
    val actualPhases = if (ctx.settings.scalajs.value) {
      // Remove phases that Scala.js does not want
      phases.mapConserve(_.filter {
        case _: FunctionalInterfaces => false
        case _ => true
      }).filter(_.nonEmpty)
    } else {
      // Remove Scala.js-related phases
      phases.mapConserve(_.filter {
        case _: GenSJSIR => false
        case _ => true
      }).filter(_.nonEmpty)
    }
    ctx.setPhasePlan(actualPhases)
    val rootScope = new MutableScope
    val bootstrap = ctx.fresh
      .setPeriod(Period(nextRunId, FirstPhaseId))
      .setScope(rootScope)
    rootScope.enter(ctx.definitions.RootPackage)(bootstrap)
    val start = bootstrap.fresh
      .setOwner(defn.RootClass)
      .setTyper(new Typer)
      .setMode(Mode.ImplicitsEnabled)
      .setTyperState(new MutableTyperState(ctx.typerState, ctx.typerState.reporter, isCommittable = true))
    ctx.initialize()(start) // re-initialize the base context with start
    def addImport(ctx: Context, refFn: () => TermRef) =
      ctx.fresh.setImportInfo(ImportInfo.rootImport(refFn)(ctx))
    (start.setRunInfo(new RunInfo(start)) /: defn.RootImportFns)(addImport)
  }

  def reset()(implicit ctx: Context): Unit = {
    ctx.base.reset()
    ctx.runInfo.clear()
  }

  def newRun(implicit ctx: Context): Run = {
    reset()
    new Run(this)(rootContext)
  }
}
