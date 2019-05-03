package dotty.tools
package dotc

import core._
import Contexts._
import typer.{FrontEnd, RefChecks}
import Phases.Phase
import transform._
import dotty.tools.backend.jvm.{CollectSuperCalls, GenBCode}
import dotty.tools.backend.sjs
import dotty.tools.dotc.transform.localopt.StringInterpolatorOpt

/** The central class of the dotc compiler. The job of a compiler is to create
 *  runs, which process given `phases` in a given `rootContext`.
 */
class Compiler {

  /** Meta-ordering constraint:
   *
   *  DenotTransformers that change the signature of their denotation's info must go
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
    frontendPhases ::: picklerPhases ::: transformPhases ::: backendPhases

  /** Phases dealing with the frontend up to trees ready for TASTY pickling */
  protected def frontendPhases: List[List[Phase]] =
    List(new FrontEnd) ::           // Compiler frontend: scanner, parser, namer, typer
    List(new YCheckPositions) ::    // YCheck positions
    List(new Staging) ::            // Check PCP, heal quoted types and expand macros
    List(new sbt.ExtractDependencies) :: // Sends information on classes' dependencies to sbt via callbacks
    List(new PostTyper) ::          // Additional checks and cleanups after type checking
    List(new sbt.ExtractAPI) ::     // Sends a representation of the API of classes to sbt via callbacks
    List(new SetRootTree) ::        // Set the `rootTreeOrProvider` on class symbols
    Nil

  /** Phases dealing with TASTY tree pickling and unpickling */
  protected def picklerPhases: List[List[Phase]] =
    List(new Pickler) ::            // Generate TASTY info
    List(new ReifyQuotes) ::        // Turn quoted trees into explicit run-time data structures
    Nil

  /** Phases dealing with the transformation from pickled trees to backend trees */
  protected def transformPhases: List[List[Phase]] =
    List(new FirstTransform,         // Some transformations to put trees into a canonical form
         new CheckReentrant,         // Internal use only: Check that compiled program has no data races involving global vars
         new ElimPackagePrefixes,    // Eliminate references to package prefixes in Select nodes
         new CookComments) ::        // Cook the comments: expand variables, doc, etc.
    List(new CheckStatic,            // Check restrictions that apply to @static members
         new ElimRepeated,           // Rewrite vararg parameters and arguments
         new ExpandSAMs,             // Expand single abstract method closures to anonymous classes
         new ProtectedAccessors,     // Add accessors for protected members
         new ExtensionMethods,       // Expand methods of value classes with extension methods
         new ShortcutImplicits,      // Allow implicit functions without creating closures
         new ByNameClosures,         // Expand arguments to by-name parameters to closures
         new HoistSuperArgs,         // Hoist complex arguments of supercalls to enclosing scope
         new ClassOf,                // Expand `Predef.classOf` calls.
         new RefChecks) ::           // Various checks mostly related to abstract members and overriding
    List(new ElimOpaque,             // Turn opaque into normal aliases
         new TryCatchPatterns,       // Compile cases in try/catch
         new PatternMatcher,         // Compile pattern matches
         new ExplicitOuter,          // Add accessors to outer classes from nested ones.
         new ExplicitSelf,           // Make references to non-trivial self types explicit as casts
         new StringInterpolatorOpt,  // Optimizes raw and s string interpolators by rewriting them to string concatentations
         new CrossCastAnd) ::        // Normalize selections involving intersection types.
    List(new PruneErasedDefs,        // Drop erased definitions from scopes and simplify erased expressions
         new VCInlineMethods,        // Inlines calls to value class methods
         new SeqLiterals,            // Express vararg arguments as arrays
         new InterceptedMethods,     // Special handling of `==`, `|=`, `getClass` methods
         new Getters,                // Replace non-private vals and vars with getter defs (fields are added later)
         new ElimByName,             // Expand by-name parameter references
         new CollectNullableFields,  // Collect fields that can be nulled out after use in lazy initialization
         new ElimOuterSelect,        // Expand outer selections
         new AugmentScala2Traits,    // Augments Scala2 traits with additional members needed for mixin composition.
         new ResolveSuper,           // Implement super accessors
         new FunctionXXLForwarders,  // Add forwarders for FunctionXXL apply method
         new ArrayConstructors) ::   // Intercept creation of (non-generic) arrays and intrinsify.
    List(new Erasure) ::             // Rewrite types to JVM model, erasing all type parameters, abstract types and refinements.
    List(new ElimErasedValueType,    // Expand erased value types to their underlying implmementation types
         new VCElideAllocations,     // Peep-hole optimization to eliminate unnecessary value class allocations
         new TailRec,                // Rewrite tail recursion to loops
         new Mixin,                  // Expand trait fields and trait initializers
         new LazyVals,               // Expand lazy vals
         new Memoize,                // Add private fields to getters and setters
         new NonLocalReturns,        // Expand non-local returns
         new CapturedVars) ::        // Represent vars captured by closures as heap objects
    List(new Constructors,           // Collect initialization code in primary constructors
                                        // Note: constructors changes decls in transformTemplate, no InfoTransformers should be added after it
         new FunctionalInterfaces,   // Rewrites closures to implement @specialized types of Functions.
         new Instrumentation,        // Count closure allocations under -Yinstrument-closures
         new GetClass,               // Rewrites getClass calls on primitive types.
         new LiftTry) ::             // Put try expressions that might execute on non-empty stacks into their own methods their implementations
    List(new LinkScala2Impls,        // Redirect calls to trait methods defined by Scala 2.x, so that they now go to
         new LambdaLift,             // Lifts out nested functions to class scope, storing free variables in environments
                                        // Note: in this mini-phase block scopes are incorrect. No phases that rely on scopes should be here
         new ElimStaticThis) ::      // Replace `this` references to static objects by global identifiers
    List(new Flatten,                // Lift all inner classes to package scope
         new RenameLifted,           // Renames lifted classes to local numbering scheme
         new TransformWildcards,     // Replace wildcards with default values
         new MoveStatics,            // Move static methods from companion to the class itself
         new ExpandPrivate,          // Widen private definitions accessed from nested classes
         new RestoreScopes,          // Repair scopes rendered invalid by moving definitions in prior phases of the group
         new SelectStatic,           // get rid of selects that would be compiled into GetStatic
         new sjs.JUnitBootstrappers, // Generate JUnit-specific bootstrapper classes for Scala.js (not enabled by default)
         new CollectEntryPoints,     // Find classes with main methods
         new CollectSuperCalls) ::   // Find classes that are called with super
    Nil

  /** Generate the output of the compilation */
  protected def backendPhases: List[List[Phase]] =
    List(new sjs.GenSJSIR) ::        // Generate .sjsir files for Scala.js (not enabled by default)
    List(new GenBCode) ::            // Generate JVM bytecode
    Nil

  var runId: Int = 1
  def nextRunId: Int = {
    runId += 1; runId
  }

  def reset()(implicit ctx: Context): Unit = {
    ctx.base.reset()
    if (ctx.run != null) ctx.run.reset()
  }

  def newRun(implicit ctx: Context): Run = {
    reset()
    new Run(this, ctx)
  }
}
