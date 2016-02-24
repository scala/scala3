package dotty.tools
package dotc
package core

import interfaces.CompilerCallback
import Decorators._
import Periods._
import Names._
import Phases._
import Types._
import Symbols._
import Scopes._
import NameOps._
import Uniques._
import SymDenotations._
import Flags.ParamAccessor
import util.Positions._
import ast.Trees._
import ast.untpd
import util.{FreshNameCreator, SimpleMap, SourceFile, NoSource}
import typer._
import Implicits.ContextualImplicits
import config.Settings._
import config.Config
import reporting._
import collection.mutable
import collection.immutable.BitSet
import printing._
import config.{Settings, ScalaSettings, Platform, JavaPlatform}
import language.implicitConversions
import DenotTransformers.DenotTransformer

object Contexts {

  /** A context is passed basically everywhere in dotc.
   *  This is convenient but carries the risk of captured contexts in
   *  objects that turn into space leaks. To combat this risk, here are some
   *  conventions to follow:
   *
   *    - Never let an implicit context be an argument of a class whose instances
   *      live longer than the context.
   *    - Classes that need contexts for their initialization take an explicit parameter
   *      named `initctx`. They pass initctx to all positions where it is needed
   *      (and these positions should all be part of the intialization sequence of the class).
   *    - Classes that need contexts that survive initialization are instead passed
   *      a "condensed context", typically named `cctx` (or they create one). Condensed contexts
   *      just add some basic information to the context base without the
   *      risk of capturing complete trees.
   *    - To make sure these rules are kept, it would be good to do a sanity
   *      check using bytecode inspection with javap or scalap: Keep track
   *      of all class fields of type context; allow them only in whitelisted
   *      classes (which should be short-lived).
   */
  abstract class Context extends Periods
                            with Substituters
                            with TypeOps
                            with Phases
                            with Printers
                            with Symbols
                            with SymDenotations
                            with Reporting
                            with NamerContextOps
                            with Cloneable { thiscontext =>
    implicit def ctx: Context = this

    /** The context base at the root */
    val base: ContextBase

    /** All outer contexts, ending in `base.initialCtx` and then `NoContext` */
    def outersIterator = new Iterator[Context] {
      var current = thiscontext
      def hasNext = current != NoContext
      def next = { val c = current; current = current.outer; c }
    }

    /** The outer context */
    private[this] var _outer: Context = _
    protected def outer_=(outer: Context) = _outer = outer
    def outer: Context = _outer

    /** The compiler callback implementation, or null if no callback will be called. */
    private[this] var _compilerCallback: CompilerCallback = _
    protected def compilerCallback_=(callback: CompilerCallback) =
      _compilerCallback = callback
    def compilerCallback: CompilerCallback = _compilerCallback

    /** The current context */
    private[this] var _period: Period = _
    protected def period_=(period: Period) = {
      assert(period.firstPhaseId == period.lastPhaseId, period)
      _period = period
    }
    def period: Period = _period

    /** The scope nesting level */
    private[this] var _mode: Mode = _
    protected def mode_=(mode: Mode) = _mode = mode
    def mode: Mode = _mode

    /** The current type comparer */
    private[this] var _typerState: TyperState = _
    protected def typerState_=(typerState: TyperState) = _typerState = typerState
    def typerState: TyperState = _typerState

    /** The current plain printer */
    private[this] var _printerFn: Context => Printer = _
    protected def printerFn_=(printerFn: Context => Printer) = _printerFn = printerFn
    def printerFn: Context => Printer = _printerFn

    /** The current owner symbol */
    private[this] var _owner: Symbol = _
    protected def owner_=(owner: Symbol) = _owner = owner
    def owner: Symbol = _owner

    /** The current settings values */
    private[this] var _sstate: SettingsState = _
    protected def sstate_=(sstate: SettingsState) = _sstate = sstate
    def sstate: SettingsState = _sstate

    /** The current tree */
    private[this] var _compilationUnit: CompilationUnit = _
    protected def compilationUnit_=(compilationUnit: CompilationUnit) = _compilationUnit = compilationUnit
    def compilationUnit: CompilationUnit = _compilationUnit

    /** The current tree */
    private[this] var _tree: Tree[_ >: Untyped] = _
    protected def tree_=(tree: Tree[_ >: Untyped]) = _tree = tree
    def tree: Tree[_ >: Untyped] = _tree

    /** The current scope */
    private[this] var _scope: Scope = _
    protected def scope_=(scope: Scope) = _scope = scope
    def scope: Scope = _scope

    /** The current type assigner or typer */
    private[this] var _typeAssigner: TypeAssigner = _
    protected def typeAssigner_=(typeAssigner: TypeAssigner) = _typeAssigner = typeAssigner
    def typeAssigner: TypeAssigner = _typeAssigner
    def typer: Typer = _typeAssigner.asInstanceOf[Typer]

    /** The currently active import info */
    private[this] var _importInfo: ImportInfo = _
    protected def importInfo_=(importInfo: ImportInfo) = _importInfo = importInfo
    def importInfo: ImportInfo = _importInfo

    /** The current compiler-run specific Info */
    private[this] var _runInfo: RunInfo = _
    protected def runInfo_=(runInfo: RunInfo) = _runInfo = runInfo
    def runInfo: RunInfo = _runInfo

    /** An optional diagostics buffer than is used by some checking code
     *  to provide more information in the buffer if it exists.
     */
    private var _diagnostics: Option[StringBuilder] = _
    protected def diagnostics_=(diagnostics: Option[StringBuilder]) = _diagnostics = diagnostics
    def diagnostics: Option[StringBuilder] = _diagnostics

    /** The current bounds in force for type parameters appearing in a GADT */
    private var _gadt: GADTMap = _
    protected def gadt_=(gadt: GADTMap) = _gadt = gadt
    def gadt: GADTMap = _gadt

    /**The current fresh name creator */
    private[this] var _freshNames: FreshNameCreator = _
    protected def freshNames_=(freshNames: FreshNameCreator) = _freshNames = freshNames
    def freshNames: FreshNameCreator = _freshNames

    def freshName(prefix: String = ""): String = freshNames.newName(prefix)
    def freshName(prefix: Name): String = freshName(prefix.toString)

    /** A map in which more contextual properties can be stored */
    private var _moreProperties: Map[String, Any] = _
    protected def moreProperties_=(moreProperties: Map[String, Any]) = _moreProperties = moreProperties
    def moreProperties: Map[String, Any] = _moreProperties

    private var _typeComparer: TypeComparer = _
    protected def typeComparer_=(typeComparer: TypeComparer) = _typeComparer = typeComparer
    def typeComparer: TypeComparer = {
      if (_typeComparer.ctx ne this)
        _typeComparer = _typeComparer.copyIn(this)
      _typeComparer
    }

    /** Number of findMember calls on stack */
    private[core] var findMemberCount: Int = 0

    /** List of names which have a findMemberCall on stack,
     *  after Config.LogPendingFindMemberThreshold is reached.
     */
    private[core] var pendingMemberSearches: List[Name] = Nil

    /** The new implicit references that are introduced by this scope */
    private var implicitsCache: ContextualImplicits = null
    def implicits: ContextualImplicits = {
      if (implicitsCache == null )
        implicitsCache = {
          val implicitRefs: List[TermRef] =
            if (isClassDefContext)
              try owner.thisType.implicitMembers
              catch {
                case ex: CyclicReference => Nil
              }
            else if (isImportContext) importInfo.importedImplicits
            else if (isNonEmptyScopeContext) scope.implicitDecls
            else Nil
          val outerImplicits =
            if (isImportContext && importInfo.hiddenRoot.exists)
              outer.implicits exclude importInfo.hiddenRoot
            else
              outer.implicits
          if (implicitRefs.isEmpty) outerImplicits
          else new ContextualImplicits(implicitRefs, outerImplicits)(this)
        }
      implicitsCache
    }

    /** The history of implicit searches that are currently active */
    private var _searchHistory: SearchHistory = null
    protected def searchHistory_= (searchHistory: SearchHistory) = _searchHistory = searchHistory
    def searchHistory: SearchHistory = _searchHistory

    /** Those fields are used to cache phases created in withPhase.
      * phasedCtx is first phase with altered phase ever requested.
      * phasedCtxs is array that uses phaseId's as indexes,
      * contexts are created only on request and cached in this array
      */
    private var phasedCtx: Context = _
    private var phasedCtxs: Array[Context] = _

    /** This context at given phase.
     *  This method will always return a phase period equal to phaseId, thus will never return squashed phases
     */
    final def withPhase(phaseId: PhaseId): Context =
      if (this.phaseId == phaseId) this
      else if (phasedCtx.phaseId == phaseId) phasedCtx
      else if (phasedCtxs != null && phasedCtxs(phaseId) != null) phasedCtxs(phaseId)
      else {
        val ctx1 = fresh.setPhase(phaseId)
        if (phasedCtx eq this) phasedCtx = ctx1
        else {
          if (phasedCtxs == null) phasedCtxs = new Array[Context](base.phases.length)
          phasedCtxs(phaseId) = ctx1
        }
        ctx1
      }

    final def withPhase(phase: Phase): Context =
      withPhase(phase.id)

    final def withPhaseNoLater(phase: Phase) =
      if (ctx.phase.id > phase.id) withPhase(phase) else ctx

    /** If -Ydebug is on, the top of the stack trace where this context
     *  was created, otherwise `null`.
     */
    private var creationTrace: Array[StackTraceElement] = _

    private def setCreationTrace() =
      if (this.settings.YtraceContextCreation.value)
        creationTrace = (new Throwable).getStackTrace().take(20)

    /** Print all enclosing context's creation stacktraces */
    def printCreationTraces() = {
      println("=== context creation trace =======")
      for (ctx <- outersIterator) {
        println(s">>>>>>>>> $ctx")
        if (ctx.creationTrace != null) println(ctx.creationTrace.mkString("\n"))
      }
      println("=== end context creation trace ===")
    }

    /** The current reporter */
    def reporter: Reporter = typerState.reporter

    /** Is this a context for the members of a class definition? */
    def isClassDefContext: Boolean =
      owner.isClass && (owner ne outer.owner)

    /** Is this a context that introduces an import clause? */
    def isImportContext: Boolean =
      (this ne NoContext) && (this.importInfo ne outer.importInfo)

    /** Is this a context that introduces a non-empty scope? */
    def isNonEmptyScopeContext: Boolean =
      (this.scope ne outer.scope) && this.scope.nonEmpty

    /** Leave message in diagnostics buffer if it exists */
    def diagnose(str: => String) =
      for (sb <- diagnostics) {
        sb.setLength(0)
        sb.append(str)
      }

    /** The next outer context whose tree is a template or package definition */
    def enclTemplate: Context = {
      var c = this
      while (c != NoContext && !c.tree.isInstanceOf[Template[_]] && !c.tree.isInstanceOf[PackageDef[_]])
        c = c.outer
      c
    }

    /** The context for a supercall. This context is used for elaborating
     *  the parents of a class and their arguments.
     *  The context is computed from the current class context. It has
     *
     *  - as owner: The primary constructor of the class
     *  - as outer context: The context enclosing the class context
     *  - as scope: The parameter accessors in the class context
     *  - with additional mode: InSuperCall
     *
     *  The reasons for this peculiar choice of attributes are as follows:
     *
     *  - The constructor must be the owner, because that's where any local methods or closures
     *    should go.
     *  - The context may not see any class members (inherited or defined), and should
     *    instead see definitions defined in the outer context which might be shadowed by
     *    such class members. That's why the outer context must be the outer context of the class.
     *  - At the same time the context should see the parameter accessors of the current class,
     *    that's why they get added to the local scope. An alternative would have been to have the
     *    context see the constructor parameters instead, but then we'd need a final substitution step
     *    from constructor parameters to class parameter accessors.
     */
    def superCallContext: Context = {
      val locals = newScopeWith(owner.asClass.paramAccessors: _*)
      superOrThisCallContext(owner.primaryConstructor, locals)
    }

    /** The context for the arguments of a this(...) constructor call.
     *  The context is computed from the local auxiliary constructor context.
     *  It has
     *
     *   - as owner: The auxiliary constructor
     *   - as outer context: The context enclosing the enclosing class context
     *   - as scope: The parameters of the auxiliary constructor.
     */
    def thisCallArgContext: Context = {
      assert(owner.isClassConstructor)
      val constrCtx = outersIterator.dropWhile(_.outer.owner == owner).next
      superOrThisCallContext(owner, constrCtx.scope).setTyperState(typerState)
    }

    /** The super= or this-call context with given owner and locals. */
    private def superOrThisCallContext(owner: Symbol, locals: Scope): FreshContext = {
      var classCtx = outersIterator.dropWhile(!_.isClassDefContext).next
      classCtx.outer.fresh.setOwner(owner).setScope(locals).setMode(classCtx.mode | Mode.InSuperCall)
    }

    /** The context of expression `expr` seen as a member of a statement sequence */
    def exprContext(stat: Tree[_ >: Untyped], exprOwner: Symbol) =
      if (exprOwner == this.owner) this
      else if (untpd.isSuperConstrCall(stat) && this.owner.isClass) superCallContext
      else ctx.fresh.setOwner(exprOwner)

    /** The current source file; will be derived from current
     *  compilation unit.
     */
    def source: SourceFile =
      if (compilationUnit == null) NoSource else compilationUnit.source

    /** Does current phase use an erased types interpretation? */
    def erasedTypes: Boolean = phase.erasedTypes

    /** Is the debug option set? */
    def debug: Boolean = base.settings.debug.value

    /** Is the verbose option set? */
    def verbose: Boolean = base.settings.verbose.value

    /** A condensed context containing essential information of this but
     *  no outer contexts except the initial context.
    private var _condensed: CondensedContext = null
    def condensed: CondensedContext = {
      if (_condensed eq outer.condensed)
        _condensed = base.initialCtx.fresh
          .withPeriod(period)
          .withNewMode(mode)
          // typerState and its constraint is not preserved in condensed
          // reporter is always ThrowingReporter
          .withPrinterFn(printerFn)
          .withOwner(owner)
          .withSettings(sstate)
          // tree is not preserved in condensed
          .withRunInfo(runInfo)
          .withDiagnostics(diagnostics)
          .withMoreProperties(moreProperties)
      _condensed
    }
    */

    protected def init(outer: Context): this.type = {
      this.outer = outer
      this.implicitsCache = null
      this.phasedCtx = this
      this.phasedCtxs = null
      setCreationTrace()
      this
    }

    /** A fresh clone of this context. */
    def fresh: FreshContext = clone.asInstanceOf[FreshContext].init(this)

    final def withOwner(owner: Symbol): Context =
      if (owner ne this.owner) fresh.setOwner(owner) else this

    override def toString =
      "Context(\n" +
      (outersIterator map ( ctx => s"  owner = ${ctx.owner}, scope = ${ctx.scope}") mkString "\n")
  }

  /** A condensed context provides only a small memory footprint over
   *  a Context base, and therefore can be stored without problems in
   *  long-lived objects.
  abstract class CondensedContext extends Context {
    override def condensed = this
  }
  */

  /** A fresh context allows selective modification
   *  of its attributes using the with... methods.
   */
  abstract class FreshContext extends Context {
    def setPeriod(period: Period): this.type = { this.period = period; this }
    def setMode(mode: Mode): this.type = { this.mode = mode; this }
    def setCompilerCallback(callback: CompilerCallback): this.type = { this.compilerCallback = callback; this }
    def setTyperState(typerState: TyperState): this.type = { this.typerState = typerState; this }
    def setReporter(reporter: Reporter): this.type = setTyperState(typerState.withReporter(reporter))
    def setNewTyperState: this.type = setTyperState(typerState.fresh(isCommittable = true))
    def setExploreTyperState: this.type = setTyperState(typerState.fresh(isCommittable = false))
    def setPrinterFn(printer: Context => Printer): this.type = { this.printerFn = printer; this }
    def setOwner(owner: Symbol): this.type = { assert(owner != NoSymbol); this.owner = owner; this }
    def setSettings(sstate: SettingsState): this.type = { this.sstate = sstate; this }
    def setCompilationUnit(compilationUnit: CompilationUnit): this.type = { this.compilationUnit = compilationUnit; this }
    def setTree(tree: Tree[_ >: Untyped]): this.type = { this.tree = tree; this }
    def setScope(scope: Scope): this.type = { this.scope = scope; this }
    def setNewScope: this.type = { this.scope = newScope; this }
    def setTypeAssigner(typeAssigner: TypeAssigner): this.type = { this.typeAssigner = typeAssigner; this }
    def setTyper(typer: Typer): this.type = { this.scope = typer.scope; setTypeAssigner(typer) }
    def setImportInfo(importInfo: ImportInfo): this.type = { this.importInfo = importInfo; this }
    def setRunInfo(runInfo: RunInfo): this.type = { this.runInfo = runInfo; this }
    def setDiagnostics(diagnostics: Option[StringBuilder]): this.type = { this.diagnostics = diagnostics; this }
    def setTypeComparerFn(tcfn: Context => TypeComparer): this.type = { this.typeComparer = tcfn(this); this }
    def setSearchHistory(searchHistory: SearchHistory): this.type = { this.searchHistory = searchHistory; this }
    def setFreshNames(freshNames: FreshNameCreator): this.type = { this.freshNames = freshNames; this }
    def setMoreProperties(moreProperties: Map[String, Any]): this.type = { this.moreProperties = moreProperties; this }

    def setProperty(prop: (String, Any)): this.type = setMoreProperties(moreProperties + prop)

    def setPhase(pid: PhaseId): this.type = setPeriod(Period(runId, pid))
    def setPhase(phase: Phase): this.type = setPeriod(Period(runId, phase.start, phase.end))

    def setSetting[T](setting: Setting[T], value: T): this.type =
      setSettings(setting.updateIn(sstate, value))

    def setFreshGADTBounds: this.type = { this.gadt = new GADTMap(gadt.bounds); this }

    def setDebug = setSetting(base.settings.debug, true)
  }

  implicit class ModeChanges(val c: Context) extends AnyVal {
    final def withModeBits(mode: Mode): Context =
      if (mode != c.mode) c.fresh.setMode(mode) else c

    final def addMode(mode: Mode): Context = withModeBits(c.mode | mode)
    final def maskMode(mode: Mode): Context = withModeBits(c.mode & mode)
    final def retractMode(mode: Mode): Context = withModeBits(c.mode &~ mode)
  }

  implicit class FreshModeChanges(val c: FreshContext) extends AnyVal {
    final def addMode(mode: Mode): c.type = c.setMode(c.mode | mode)
    final def maskMode(mode: Mode): c.type = c.setMode(c.mode & mode)
    final def retractMode(mode: Mode): c.type = c.setMode(c.mode &~ mode)
  }

  /** A class defining the initial context with given context base
   *  and set of possible settings.
   */
  private class InitialContext(val base: ContextBase, settings: SettingGroup) extends FreshContext {
    outer = NoContext
    period = InitialPeriod
    mode = Mode.None
    typerState = new TyperState(new ConsoleReporter())
    printerFn = new RefinedPrinter(_)
    owner = NoSymbol
    sstate = settings.defaultState
    tree = untpd.EmptyTree
    typeAssigner = TypeAssigner
    runInfo = new RunInfo(this)
    diagnostics = None
    freshNames = new FreshNameCreator.Default
    moreProperties = Map.empty
    typeComparer = new TypeComparer(this)
    searchHistory = new SearchHistory(0, Map())
    gadt = new GADTMap(SimpleMap.Empty)
  }

  @sharable object NoContext extends Context {
    val base = null
    override val implicits: ContextualImplicits = new ContextualImplicits(Nil, null)(this)
  }

  /** A context base defines state and associated methods that exist once per
   *  compiler run.
   */
  class ContextBase extends ContextState
                       with Denotations.DenotationsBase
                       with Phases.PhasesBase {

    /** The applicable settings */
    val settings = new ScalaSettings

    /** The initial context */
    val initialCtx: Context = new InitialContext(this, settings)

    /** The symbol loaders */
    val loaders = new SymbolLoaders

    /** The platform */
    val platform: Platform = new JavaPlatform

    /** The loader that loads the members of _root_ */
    def rootLoader(root: TermSymbol)(implicit ctx: Context): SymbolLoader = platform.rootLoader(root)

    // Set up some phases to get started */
    usePhases(List(SomePhase))

    /** The standard definitions */
    val definitions = new Definitions

    def squashed(p: Phase): Phase = {
      allPhases.find(_.period.containsPhaseId(p.id)).getOrElse(NoPhase)
    }
  }

  /** The essential mutable state of a context base, collected into a common class */
  class ContextState {
    // Symbols state

    /** A counter for unique ids */
    private[core] var _nextId = 0

    def nextId = { _nextId += 1; _nextId }

    /** A map from a superclass id to the typeref of the class that has it */
    private[core] var classOfId = new Array[ClassSymbol](Config.InitialSuperIdsSize)

    /** A map from a the typeref of a class to its superclass id */
    private[core] val superIdOfClass = new mutable.AnyRefMap[ClassSymbol, Int]

    /** The last allocated superclass id */
    private[core] var lastSuperId = -1

    /** Allocate and return next free superclass id */
    private[core] def nextSuperId: Int = {
      lastSuperId += 1
      if (lastSuperId >= classOfId.length) {
        val tmp = new Array[ClassSymbol](classOfId.length * 2)
        classOfId.copyToArray(tmp)
        classOfId = tmp
      }
      lastSuperId
    }

    // Types state
    /** A table for hash consing unique types */
    private[core] val uniques = new util.HashSet[Type](Config.initialUniquesCapacity) {
      override def hash(x: Type): Int = x.hash
    }

    /** A table for hash consing unique refined types */
    private[dotc] val uniqueRefinedTypes = new RefinedUniques

    /** A table for hash consing unique named types */
    private[core] val uniqueNamedTypes = new NamedTypeUniques

    /** A table for hash consing unique type bounds */
    private[core] val uniqueTypeAliases = new TypeAliasUniques

    private def uniqueSets = Map(
        "uniques" -> uniques,
        "uniqueRefinedTypes" -> uniqueRefinedTypes,
        "uniqueNamedTypes" -> uniqueNamedTypes,
        "uniqueTypeAliases" -> uniqueTypeAliases)

    /** A map that associates label and size of all uniques sets */
    def uniquesSizes: Map[String, Int] = uniqueSets.mapValues(_.size)

    /** The number of recursive invocation of underlying on a NamedType
     *  during a controlled operation.
     */
    private[core] var underlyingRecursions: Int = 0

    /** The set of named types on which a currently active invocation
     *  of underlying during a controlled operation exists. */
    private[core] val pendingUnderlying = new mutable.HashSet[Type]

    /** A flag that some unsafe nonvariant instantiation was encountered
     *  in this run. Used as a shortcut to a avoid scans of types in
     *  Typer.typedSelect.
     */
    private[dotty] var unsafeNonvariant: RunId = NoRunId

    // Phases state

    private[core] var phasesPlan: List[List[Phase]] = _

    /** Phases by id */
    private[core] var phases: Array[Phase] = _

    /** Phases with consecutive Transforms grouped into a single phase, Empty array if squashing is disabled */
    private[core] var squashedPhases: Array[Phase] = Array.empty[Phase]

    /** Next denotation transformer id */
    private[core] var nextDenotTransformerId: Array[Int] = _

    private[core] var denotTransformers: Array[DenotTransformer] = _

    // Printers state
    /** Number of recursive invocations of a show method on current stack */
    private[dotc] var toTextRecursions = 0

    // Reporters state
    private[dotc] var indent = 0

    protected[dotc] val indentTab = "  "

    def reset() = {
      for ((_, set) <- uniqueSets) set.clear()
      for (i <- 0 until classOfId.length) classOfId(i) = null
      superIdOfClass.clear()
      lastSuperId = -1
    }

    // Test that access is single threaded

    /** The thread on which `checkSingleThreaded was invoked last */
    @sharable private var thread: Thread = null

    /** Check that we are on the same thread as before */
    def checkSingleThreaded() =
      if (thread == null) thread = Thread.currentThread()
      else assert(thread == Thread.currentThread(), "illegal multithreaded access to ContextBase")
  }

  object Context {

    /** Implicit conversion that injects all printer operations into a context */
    implicit def toPrinter(ctx: Context): Printer = ctx.printer

    /** implicit conversion that injects all ContextBase members into a context */
    implicit def toBase(ctx: Context): ContextBase = ctx.base

    // @sharable val theBase = new ContextBase // !!! DEBUG, so that we can use a minimal context for reporting even in code that normally cannot access a context
  }

  /** Info that changes on each compiler run */
  class RunInfo(initctx: Context) extends ImplicitRunInfo with ConstraintRunInfo {
    implicit val ctx: Context = initctx
  }

  class GADTMap(initBounds: SimpleMap[Symbol, TypeBounds]) {
    private var myBounds = initBounds
    def setBounds(sym: Symbol, b: TypeBounds): Unit =
      myBounds = myBounds.updated(sym, b)
    def bounds = myBounds
  }
}
