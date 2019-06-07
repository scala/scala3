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
import Uniques._
import ast.Trees._
import ast.untpd
import Flags.ImplicitOrImpliedOrGiven
import util.{FreshNameCreator, NoSource, SimpleIdentityMap, SourceFile}
import typer.{Implicits, ImportInfo, Inliner, NamerContextOps, SearchHistory, SearchRoot, TypeAssigner, Typer}
import Implicits.ContextualImplicits
import config.Settings._
import config.Config
import reporting._
import reporting.diagnostic.Message
import io.{AbstractFile, NoAbstractFile, PlainFile, Path}
import scala.io.Codec
import collection.mutable
import printing._
import config.{JavaPlatform, SJSPlatform, Platform, ScalaSettings}

import scala.annotation.internal.sharable

import DenotTransformers.DenotTransformer
import dotty.tools.dotc.profile.Profiler
import util.Property.Key
import util.Store
import xsbti.AnalysisCallback
import plugins._
import java.util.concurrent.atomic.AtomicInteger

object Contexts {

  private val (compilerCallbackLoc, store1) = Store.empty.newLocation[CompilerCallback]()
  private val (sbtCallbackLoc,      store2) = store1.newLocation[AnalysisCallback]()
  private val (printerFnLoc,        store3) = store2.newLocation[Context => Printer](new RefinedPrinter(_))
  private val (settingsStateLoc,    store4) = store3.newLocation[SettingsState]()
  private val (freshNamesLoc,       store5) = store4.newLocation[FreshNameCreator](new FreshNameCreator.Default)
  private val (compilationUnitLoc,  store6) = store5.newLocation[CompilationUnit]()
  private val (runLoc,              store7) = store6.newLocation[Run]()
  private val (profilerLoc,         store8) = store7.newLocation[Profiler]()
  private val initialStore = store8

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
  abstract class Context(val base: ContextBase)
    extends Periods
       with Substituters
       with TypeOps
       with Phases
       with Printers
       with Symbols
       with SymDenotations
       with Reporting
       with NamerContextOps
       with Plugins
       with Cloneable { thiscontext =>

    implicit def ctx: Context = this

    /** All outer contexts, ending in `base.initialCtx` and then `NoContext` */
    def outersIterator: Iterator[Context] = new Iterator[Context] {
      var current = thiscontext
      def hasNext = current != NoContext
      def next = { val c = current; current = current.outer; c }
    }

    /** The outer context */
    private[this] var _outer: Context = _
    protected def outer_=(outer: Context): Unit = _outer = outer
    final def outer: Context = _outer

    /** The current context */
    private[this] var _period: Period = _
    protected def period_=(period: Period): Unit = {
      assert(period.firstPhaseId == period.lastPhaseId, period)
      _period = period
    }
    final def period: Period = _period

    /** The scope nesting level */
    private[this] var _mode: Mode = _
    protected def mode_=(mode: Mode): Unit = _mode = mode
    final def mode: Mode = _mode

    /** The current owner symbol */
    private[this] var _owner: Symbol = _
    protected def owner_=(owner: Symbol): Unit = _owner = owner
    final def owner: Symbol = _owner

    /** The current tree */
    private[this] var _tree: Tree[_ >: Untyped]= _
    protected def tree_=(tree: Tree[_ >: Untyped]): Unit = _tree = tree
    final def tree: Tree[_ >: Untyped] = _tree

    /** The current scope */
    private[this] var _scope: Scope = _
    protected def scope_=(scope: Scope): Unit = _scope = scope
    final def scope: Scope = _scope

    /** The current type comparer */
    private[this] var _typerState: TyperState = _
    protected def typerState_=(typerState: TyperState): Unit = _typerState = typerState
    final def typerState: TyperState = _typerState

    /** The current type assigner or typer */
    private[this] var _typeAssigner: TypeAssigner = _
    protected def typeAssigner_=(typeAssigner: TypeAssigner): Unit = _typeAssigner = typeAssigner
    final def typeAssigner: TypeAssigner = _typeAssigner

    /** The currently active import info */
    private[this] var _importInfo: ImportInfo = _
    protected def importInfo_=(importInfo: ImportInfo): Unit = _importInfo = importInfo
    final def importInfo: ImportInfo = _importInfo

    /** The current bounds in force for type parameters appearing in a GADT */
    private[this] var _gadt: GadtConstraint = _
    protected def gadt_=(gadt: GadtConstraint): Unit = _gadt = gadt
    final def gadt: GadtConstraint = _gadt

    /** The history of implicit searches that are currently active */
    private[this] var _searchHistory: SearchHistory = null
    protected def searchHistory_= (searchHistory: SearchHistory): Unit = _searchHistory = searchHistory
    final def searchHistory: SearchHistory = _searchHistory

    /** The current type comparer. This ones updates itself automatically for
     *  each new context.
     */
    private[this] var _typeComparer: TypeComparer = _
    protected def typeComparer_=(typeComparer: TypeComparer): Unit = _typeComparer = typeComparer
    def typeComparer: TypeComparer = {
      if (_typeComparer.ctx ne this)
        _typeComparer = _typeComparer.copyIn(this)
      _typeComparer
    }

    /** The current source file */
    private[this] var _source: SourceFile = _
    protected def source_=(source: SourceFile): Unit = _source = source
    final def source: SourceFile = _source

    /** A map in which more contextual properties can be stored
     *  Typically used for attributes that are read and written only in special situations.
     */
    private[this] var _moreProperties: Map[Key[Any], Any] = _
    protected def moreProperties_=(moreProperties: Map[Key[Any], Any]): Unit = _moreProperties = moreProperties
    final def moreProperties: Map[Key[Any], Any] = _moreProperties

    def property[T](key: Key[T]): Option[T] =
      moreProperties.get(key).asInstanceOf[Option[T]]

    /** A store that can be used by sub-components.
     *  Typically used for attributes that are defined only once per compilation unit.
     *  Access to store entries is much faster than access to properties, and only
     *  slightly slower than a normal field access would be.
     */
    private var _store: Store = _
    protected def store_=(store: Store): Unit = _store = store
    final def store: Store = _store

    /** The compiler callback implementation, or null if no callback will be called. */
    def compilerCallback: CompilerCallback = store(compilerCallbackLoc)

    /** The sbt callback implementation if we are run from sbt, null otherwise */
    def sbtCallback: AnalysisCallback = store(sbtCallbackLoc)

    /** The current plain printer */
    def printerFn: Context => Printer = store(printerFnLoc)

    /** The current settings values */
    def settingsState: SettingsState = store(settingsStateLoc)

    /** The current fresh name creator */
    def freshNames: FreshNameCreator = store(freshNamesLoc)

    /** The current compilation unit */
    def compilationUnit: CompilationUnit = store(compilationUnitLoc)

    /** The current compiler-run */
    def run: Run = store(runLoc)

    /**  The current compiler-run profiler */
    def profiler: Profiler = store(profilerLoc)

    /** The new implicit references that are introduced by this scope */
    protected var implicitsCache: ContextualImplicits = null
    def implicits: ContextualImplicits = {
      if (implicitsCache == null )
        implicitsCache = {
          val implicitRefs: List[ImplicitRef] =
            if (isClassDefContext)
              try owner.thisType.implicitMembers(ImplicitOrImpliedOrGiven)
              catch {
                case ex: CyclicReference => Nil
              }
            else if (isImportContext) importInfo.importedImplicits
            else if (isNonEmptyScopeContext) scope.implicitDecls
            else Nil
          val outerImplicits =
            if (isImportContext && importInfo.unimported.exists)
              outer.implicits exclude importInfo.unimported
            else
              outer.implicits
          if (implicitRefs.isEmpty) outerImplicits
          else new ContextualImplicits(implicitRefs, outerImplicits)(this)
        }
      implicitsCache
    }

    /** Sourcefile corresponding to given abstract file, memoized */
    def getSource(file: AbstractFile, codec: => Codec = Codec(settings.encoding.value)) = {
      util.Stats.record("getSource")
      base.sources.getOrElseUpdate(file, new SourceFile(file, codec))
    }

    /** Sourcefile with given path name, memoized */
    def getSource(path: SourceFile.PathName): SourceFile = base.sourceNamed.get(path) match {
      case Some(source) =>
        source
      case None =>
        val f = new PlainFile(Path(path.toString))
        val src = getSource(f)
        base.sourceNamed(path) = src
        src
    }

    /** Sourcefile with given path, memoized */
    def getSource(path: String): SourceFile = getSource(path.toTermName)

    /** Those fields are used to cache phases created in withPhase.
      * phasedCtx is first phase with altered phase ever requested.
      * phasedCtxs is array that uses phaseId's as indexes,
      * contexts are created only on request and cached in this array
      */
    private[this] var phasedCtx: Context = this
    private[this] var phasedCtxs: Array[Context] = _

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

    final def withPhaseNoLater(phase: Phase): Context =
      if (phase.exists && ctx.phase.id > phase.id) withPhase(phase) else ctx

    final def withPhaseNoEarlier(phase: Phase): Context =
      if (phase.exists && ctx.phase.id < phase.id) withPhase(phase) else ctx

    // `creationTrace`-related code. To enable, uncomment the code below and the
    // call to `setCreationTrace()` in this file.
    /*
    /** If -Ydebug is on, the top of the stack trace where this context
     *  was created, otherwise `null`.
     */
    private[this] var creationTrace: Array[StackTraceElement] = _

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
    */

    /** The current reporter */
    def reporter: Reporter = typerState.reporter

    /** Run `op` as if it was run in a fresh explore typer state, but possibly
     *  optimized to re-use the current typer state.
     */
    final def test[T](op: Context => T): T = typerState.test(op)(this)

    /** Is this a context for the members of a class definition? */
    def isClassDefContext: Boolean =
      owner.isClass && (owner ne outer.owner)

    /** Is this a context that introduces an import clause? */
    def isImportContext: Boolean =
      (this ne NoContext) && (this.importInfo ne outer.importInfo)

    /** Is this a context that introduces a non-empty scope? */
    def isNonEmptyScopeContext: Boolean =
      (this.scope ne outer.scope) && !this.scope.isEmpty

    /** Is this a context for typechecking an inlined body? */
    def isInlineContext: Boolean =
      typer.isInstanceOf[Inliner#InlineTyper]

    /** The next outer context whose tree is a template or package definition
     *  Note: Currently unused
    def enclTemplate: Context = {
      var c = this
      while (c != NoContext && !c.tree.isInstanceOf[Template[_]] && !c.tree.isInstanceOf[PackageDef[_]])
        c = c.outer
      c
    }*/

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
      val locals = newScopeWith(owner.typeParams ++ owner.asClass.paramAccessors: _*)
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
      val constrCtx = outersIterator.dropWhile(_.outer.owner == owner).next()
      superOrThisCallContext(owner, constrCtx.scope)
        .setTyperState(typerState)
        .setGadt(gadt)
    }

    /** The super- or this-call context with given owner and locals. */
    private def superOrThisCallContext(owner: Symbol, locals: Scope): FreshContext = {
      var classCtx = outersIterator.dropWhile(!_.isClassDefContext).next()
      classCtx.outer.fresh.setOwner(owner)
        .setScope(locals)
        .setMode(classCtx.mode | Mode.InSuperCall)
    }

    /** The context of expression `expr` seen as a member of a statement sequence */
    def exprContext(stat: Tree[_ >: Untyped], exprOwner: Symbol): Context =
      if (exprOwner == this.owner) this
      else if (untpd.isSuperConstrCall(stat) && this.owner.isClass) superCallContext
      else ctx.fresh.setOwner(exprOwner)

    /** A new context that summarizes an import statement */
    def importContext(imp: Import[_], sym: Symbol): FreshContext = {
      val impNameOpt = imp.expr match {
        case ref: RefTree[_] => Some(ref.name.asTermName)
        case _               => None
      }
      ctx.fresh.setImportInfo(
        new ImportInfo(implicit ctx => sym, imp.selectors, impNameOpt, imp.importImplied))
    }

    /** Does current phase use an erased types interpretation? */
    def erasedTypes: Boolean = phase.erasedTypes

    /** Is the debug option set? */
    def debug: Boolean = base.settings.Ydebug.value

    /** Is the verbose option set? */
    def verbose: Boolean = base.settings.verbose.value

    /** Should use colors when printing? */
    def useColors: Boolean =
      base.settings.color.value == "always"

    protected def init(outer: Context, origin: Context): this.type = {
      util.Stats.record("Context.fresh")
      _outer = outer
      _period = origin.period
      _mode = origin.mode
      _owner = origin.owner
      _tree = origin.tree
      _scope = origin.scope
      _typerState = origin.typerState
      _typeAssigner = origin.typeAssigner
      _importInfo = origin.importInfo
      _gadt = origin.gadt
      _searchHistory = origin.searchHistory
      _typeComparer = origin.typeComparer
      _source = origin.source
      _moreProperties = origin.moreProperties
      _store = origin.store
      this
    }

    /** A fresh clone of this context embedded in this context. */
    def fresh: FreshContext = freshOver(this)

    /** A fresh clone of this context embedded in the specified `outer` context. */
    def freshOver(outer: Context): FreshContext = new FreshContext(base).init(outer, this)

    final def withOwner(owner: Symbol): Context =
      if (owner ne this.owner) fresh.setOwner(owner) else this

    private var sourceCtx: SimpleIdentityMap[SourceFile, Context] = null

    final def withSource(source: SourceFile): Context =
      if (source `eq` this.source) this
      else if ((source `eq` outer.source) &&
               outer.sourceCtx != null &&
               (outer.sourceCtx(this.source) `eq` this)) outer
      else {
        if (sourceCtx == null) sourceCtx = SimpleIdentityMap.Empty
        val prev = sourceCtx(source)
        if (prev != null) prev
        else {
          val newCtx = fresh.setSource(source)
          sourceCtx = sourceCtx.updated(source, newCtx)
          newCtx
        }
      }

    final def withProperty[T](key: Key[T], value: Option[T]): Context =
      if (property(key) == value) this
      else value match {
        case Some(v) => fresh.setProperty(key, v)
        case None => fresh.dropProperty(key)
      }

    override def toString: String = {
      def iinfo(implicit ctx: Context) = if (ctx.importInfo == null) "" else i"${ctx.importInfo.selectors}%, %"
      "Context(\n" +
      (outersIterator map ( ctx => s"  owner = ${ctx.owner}, scope = ${ctx.scope}, import = ${iinfo(ctx)}") mkString "\n")
    }

    def typerPhase: Phase                  = base.typerPhase
    def postTyperPhase: Phase              = base.postTyperPhase
    def sbtExtractDependenciesPhase: Phase = base.sbtExtractDependenciesPhase
    def picklerPhase: Phase                = base.picklerPhase
    def reifyQuotesPhase: Phase            = base.reifyQuotesPhase
    def refchecksPhase: Phase              = base.refchecksPhase
    def patmatPhase: Phase                 = base.patmatPhase
    def elimRepeatedPhase: Phase           = base.elimRepeatedPhase
    def extensionMethodsPhase: Phase       = base.extensionMethodsPhase
    def explicitOuterPhase: Phase          = base.explicitOuterPhase
    def gettersPhase: Phase                = base.gettersPhase
    def erasurePhase: Phase                = base.erasurePhase
    def elimErasedValueTypePhase: Phase    = base.elimErasedValueTypePhase
    def lambdaLiftPhase: Phase             = base.lambdaLiftPhase
    def flattenPhase: Phase                = base.flattenPhase
    def genBCodePhase: Phase               = base.genBCodePhase
    def phases: Array[Phase]               = base.phases

    def settings: ScalaSettings            = base.settings
    def definitions: Definitions           = base.definitions
    def platform: Platform                 = base.platform
    def pendingUnderlying: mutable.HashSet[Type]   = base.pendingUnderlying
    def uniqueNamedTypes: Uniques.NamedTypeUniques = base.uniqueNamedTypes
    def uniques: util.HashSet[Type]                = base.uniques
    def nextSymId: Int                     = base.nextSymId

    def initialize()(implicit ctx: Context): Unit = base.initialize()(ctx)
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
  class FreshContext(base: ContextBase) extends Context(base) {
    def setPeriod(period: Period): this.type = { this.period = period; this }
    def setMode(mode: Mode): this.type = { this.mode = mode; this }
    def setOwner(owner: Symbol): this.type = { assert(owner != NoSymbol); this.owner = owner; this }
    def setTree(tree: Tree[_ >: Untyped]): this.type = { this.tree = tree; this }
    def setScope(scope: Scope): this.type = { this.scope = scope; this }
    def setNewScope: this.type = { this.scope = newScope; this }
    def setTyperState(typerState: TyperState): this.type = { this.typerState = typerState; this }
    def setNewTyperState(): this.type = setTyperState(typerState.fresh().setCommittable(true))
    def setExploreTyperState(): this.type = setTyperState(typerState.fresh().setCommittable(false))
    def setReporter(reporter: Reporter): this.type = setTyperState(typerState.fresh().setReporter(reporter))
    def setTypeAssigner(typeAssigner: TypeAssigner): this.type = { this.typeAssigner = typeAssigner; this }
    def setTyper(typer: Typer): this.type = { this.scope = typer.scope; setTypeAssigner(typer) }
    def setImportInfo(importInfo: ImportInfo): this.type = { this.importInfo = importInfo; this }
    def setGadt(gadt: GadtConstraint): this.type = { this.gadt = gadt; this }
    def setFreshGADTBounds: this.type = setGadt(gadt.fresh)
    def setSearchHistory(searchHistory: SearchHistory): this.type = { this.searchHistory = searchHistory; this }
    def setSource(source: SourceFile): this.type = { this.source = source; this }
    def setTypeComparerFn(tcfn: Context => TypeComparer): this.type = { this.typeComparer = tcfn(this); this }
    private def setMoreProperties(moreProperties: Map[Key[Any], Any]): this.type = { this.moreProperties = moreProperties; this }
    private def setStore(store: Store): this.type = { this.store = store; this }
    def setImplicits(implicits: ContextualImplicits): this.type = { this.implicitsCache = implicits; this }

    def setCompilationUnit(compilationUnit: CompilationUnit): this.type = {
      setSource(compilationUnit.source)
      updateStore(compilationUnitLoc, compilationUnit)
    }

    def setCompilerCallback(callback: CompilerCallback): this.type = updateStore(compilerCallbackLoc, callback)
    def setSbtCallback(callback: AnalysisCallback): this.type = updateStore(sbtCallbackLoc, callback)
    def setPrinterFn(printer: Context => Printer): this.type = updateStore(printerFnLoc, printer)
    def setSettings(settingsState: SettingsState): this.type = updateStore(settingsStateLoc, settingsState)
    def setRun(run: Run): this.type = updateStore(runLoc, run)
    def setProfiler(profiler: Profiler): this.type = updateStore(profilerLoc, profiler)
    def setFreshNames(freshNames: FreshNameCreator): this.type = updateStore(freshNamesLoc, freshNames)

    def setProperty[T](key: Key[T], value: T): this.type =
      setMoreProperties(moreProperties.updated(key, value))

    def dropProperty(key: Key[_]): this.type =
      setMoreProperties(moreProperties - key)

    def addLocation[T](initial: T): Store.Location[T] = {
      val (loc, store1) = store.newLocation(initial)
      setStore(store1)
      loc
    }

    def addLocation[T](): Store.Location[T] = {
      val (loc, store1) = store.newLocation[T]()
      setStore(store1)
      loc
    }

    def updateStore[T](loc: Store.Location[T], value: T): this.type =
      setStore(store.updated(loc, value))

    def setPhase(pid: PhaseId): this.type = setPeriod(Period(runId, pid))
    def setPhase(phase: Phase): this.type = setPeriod(Period(runId, phase.start, phase.end))

    def setSetting[T](setting: Setting[T], value: T): this.type =
      setSettings(setting.updateIn(settingsState, value))

    def setDebug: this.type = setSetting(base.settings.Ydebug, true)
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
  private class InitialContext(base: ContextBase, settingsGroup: SettingGroup) extends FreshContext(base) {
    outer = NoContext
    period = InitialPeriod
    mode = Mode.None
    typerState = new TyperState(null)
    owner = NoSymbol
    tree = untpd.EmptyTree
    typeAssigner = TypeAssigner
    moreProperties = Map.empty
    source = NoSource
    store = initialStore.updated(settingsStateLoc, settingsGroup.defaultState)
    typeComparer = new TypeComparer(this)
    searchHistory = new SearchRoot
    gadt = EmptyGadtConstraint
  }

  @sharable object NoContext extends Context(null) {
    source = NoSource
    override val implicits: ContextualImplicits = new ContextualImplicits(Nil, null)(this)
  }

  /** A context base defines state and associated methods that exist once per
   *  compiler run.
   */
  class ContextBase extends ContextState
                       with Denotations.DenotationsBase
                       with Phases.PhasesBase {

    /** The applicable settings */
    val settings: ScalaSettings = new ScalaSettings

    /** The initial context */
    val initialCtx: Context = new InitialContext(this, settings)

    /** The platform, initialized by `initPlatform()`. */
    private[this] var _platform: Platform = _

    /** The platform */
    def platform: Platform = {
      if (_platform == null) {
        throw new IllegalStateException(
            "initialize() must be called before accessing platform")
      }
      _platform
    }

    protected def newPlatform(implicit ctx: Context): Platform =
      if (settings.scalajs.value) new SJSPlatform
      else new JavaPlatform

    /** The loader that loads the members of _root_ */
    def rootLoader(root: TermSymbol)(implicit ctx: Context): SymbolLoader = platform.rootLoader(root)

    // Set up some phases to get started */
    usePhases(List(SomePhase))

    /** The standard definitions */
    val definitions: Definitions = new Definitions

    /** Initializes the `ContextBase` with a starting context.
     *  This initializes the `platform` and the `definitions`.
     */
    def initialize()(implicit ctx: Context): Unit = {
      _platform = newPlatform
      definitions.init()
    }

    def squashed(p: Phase): Phase = {
      allPhases.find(_.period.containsPhaseId(p.id)).getOrElse(NoPhase)
    }
  }

  /** The essential mutable state of a context base, collected into a common class */
  class ContextState {
    // Symbols state

    /** Counter for unique symbol ids */
    private[this] var _nextSymId: Int = 0
    def nextSymId: Int = { _nextSymId += 1; _nextSymId }

    /** Sources that were loaded */
    val sources: mutable.HashMap[AbstractFile, SourceFile] = new mutable.HashMap[AbstractFile, SourceFile]
    val sourceNamed: mutable.HashMap[SourceFile.PathName, SourceFile] = new mutable.HashMap[SourceFile.PathName, SourceFile]

    // Types state
    /** A table for hash consing unique types */
    private[core] val uniques: util.HashSet[Type] = new util.HashSet[Type](Config.initialUniquesCapacity) {
      override def hash(x: Type): Int = x.hash
      override def isEqual(x: Type, y: Type) = x.eql(y)
    }

    /** A table for hash consing unique applied types */
    private[dotc] val uniqueAppliedTypes: AppliedUniques = new AppliedUniques

    /** A table for hash consing unique named types */
    private[core] val uniqueNamedTypes: NamedTypeUniques = new NamedTypeUniques

    private def uniqueSets = Map(
        "uniques" -> uniques,
        "uniqueAppliedTypes" -> uniqueAppliedTypes,
        "uniqueNamedTypes" -> uniqueNamedTypes)

    /** A map that associates label and size of all uniques sets */
    def uniquesSizes: Map[String, (Int, Int, Int)] =
      uniqueSets.mapValues(s => (s.size, s.accesses, s.misses))

    /** Number of findMember calls on stack */
    private[core] var findMemberCount: Int = 0

    /** List of names which have a findMemberCall on stack,
     *  after Config.LogPendingFindMemberThreshold is reached.
     */
    private[core] var pendingMemberSearches: List[Name] = Nil

    /** The number of recursive invocation of underlying on a NamedType
     *  during a controlled operation.
     */
    private[core] var underlyingRecursions: Int = 0

    /** The set of named types on which a currently active invocation
     *  of underlying during a controlled operation exists. */
    private[core] val pendingUnderlying: mutable.HashSet[Type] = new mutable.HashSet[Type]

    /** A map from ErrorType to associated message computation. We use this map
     *  instead of storing message computations directly in ErrorTypes in order
     *  to avoid space leaks - the message computation usually captures a context.
     */
    private[core] val errorTypeMsg: mutable.Map[Types.ErrorType, () => Message] = mutable.Map()

    // Phases state

    private[core] var phasesPlan: List[List[Phase]] = _

    /** Phases by id */
    private[dotc] var phases: Array[Phase] = _

    /** Phases with consecutive Transforms grouped into a single phase, Empty array if squashing is disabled */
    private[core] var squashedPhases: Array[Phase] = Array.empty[Phase]

    /** Next denotation transformer id */
    private[core] var nextDenotTransformerId: Array[Int] = _

    private[core] var denotTransformers: Array[DenotTransformer] = _

    // Printers state
    /** Number of recursive invocations of a show method on current stack */
    private[dotc] var toTextRecursions: Int = 0

    // Reporters state
    private[dotc] var indent: Int = 0

    protected[dotc] val indentTab: String = "  "

    def reset(): Unit = {
      for ((_, set) <- uniqueSets) set.clear()
      errorTypeMsg.clear()
      sources.clear()
      sourceNamed.clear()
    }

    // Test that access is single threaded

    /** The thread on which `checkSingleThreaded was invoked last */
    @sharable private[this] var thread: Thread = null

    /** Check that we are on the same thread as before */
    def checkSingleThreaded(): Unit =
      if (thread == null) thread = Thread.currentThread()
      else assert(thread == Thread.currentThread(), "illegal multithreaded access to ContextBase")
  }
}
