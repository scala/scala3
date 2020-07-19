package dottyBench.tools
package dotc
package core

import dotty.tools.dotc.interfaces.CompilerCallback
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
import Flags.GivenOrImplicit
import util.{NoSource, SimpleIdentityMap, SourceFile}
import typer.{Implicits, ImportInfo, Inliner, SearchHistory, SearchRoot, TypeAssigner, Typer, Nullables}
import Nullables.{NotNullInfo, given _}
import Implicits.ContextualImplicits
import config.Settings._
import config.Config
import reporting._
import io.{AbstractFile, NoAbstractFile, PlainFile, Path}
import scala.io.Codec
import collection.mutable
import printing._
import config.{JavaPlatform, SJSPlatform, Platform, ScalaSettings}

import scala.annotation.internal.sharable

import DenotTransformers.DenotTransformer
import util.Property.Key
import util.Store
import xsbti.AnalysisCallback
import plugins._
import java.util.concurrent.atomic.AtomicInteger
import java.nio.file.InvalidPathException

object Contexts {

  private val (compilerCallbackLoc, store1) = Store.empty.newLocation[CompilerCallback]()
  private val (sbtCallbackLoc,      store2) = store1.newLocation[AnalysisCallback]()
  private val (printerFnLoc,        store3) = store2.newLocation[Context => Printer](new RefinedPrinter(_))
  private val (settingsStateLoc,    store4) = store3.newLocation[SettingsState]()
  private val (compilationUnitLoc,  store5) = store4.newLocation[CompilationUnit]()
  private val (runLoc,              store6) = store5.newLocation[Run]()
  private val (notNullInfosLoc,     store7) = store6.newLocation[List[NotNullInfo]]()
  private val (importInfoLoc,       store8) = store7.newLocation[ImportInfo]()

  private val initialStore = store8

  /** The current context */
  inline def ctx(using ctx: Ctx): Ctx = ctx

  inline def currentContext(using c: Context): Context = c

  inline def combinedContext(using ctx: Ctx, cs: CState): Context =
    ctx.withPeriod(cs)

  /** Run `op` with given small context at current phase */
  inline def inCtx[T](c: Ctx)(inline op: Ctx ?=> T): T =
    op(using c)

  /** Run `op` with given full context with phase as indicated by the context */
  inline def inContext[T](c: Context)(inline op: (Context, CState) ?=> T): T =
    op(using c, c.cstate)

  /** Run `op` with given full context with phase as indicated by the context */
  inline def inCtxAndState[T](c: Ctx, cs: CState)(inline op: (Ctx, CState) ?=> T): T =
    op(using c, cs)

  /** Execute `op` at given period */
  inline def atPeriod[T](period: Period)(inline op: (Ctx, CState) ?=> T)(using Ctx, CState): T =
    op(using ctx.fresh.setPeriod(period), period)

  /** Execute `op` at given phase id */
  inline def atPhase[T](pid: PhaseId)(inline op: (Ctx, CState) ?=> T)(using Ctx, CState): T =
    op(using
      if pid == currentPhaseId then ctx else ctx.withPhase(pid),
      if pid == currentPhaseId then currentPeriod else Period(currentRunId, pid))

  /** Execute `op` at given phase */
  inline def atPhase[T](phase: Phase)(inline op: (Ctx, CState) ?=> T)(using Ctx, CState): T =
    atPhase(phase.id)(op)

  inline def atNextPhase[T](inline op: (Ctx, CState) ?=> T)(using Ctx, CState): T =
    atPhase(currentPhase.next)(op)

  inline def atPhaseNoLater[T](limit: Phase)(inline op: (Ctx, CState) ?=> T)(using Ctx, CState): T =
    atPhase(if !limit.exists || currentPhase <= limit then currentPhase else limit)(op)

  inline def atPhaseNoEarlier[T](limit: Phase)(inline op: (Ctx, CState) ?=> T)(using Ctx, CState): T =
    atPhase(if !limit.exists || limit <= currentPhase then currentPhase else limit)(op)

  inline def currentPeriod(using ctx: Ctx, cs: CState): Period =
    assertSamePeriod()
    cs

  def assertSamePeriod()(using ctx: Ctx, cs: CState) =
    assert(ctx.asInstanceOf[Context].period == cs, s"period discrepancy, in context = ${ctx.asInstanceOf[Context].period}, state = $cs")

  inline def currentPhase(using ctx: Ctx, cs: CState): Phase = ctx.base.phases(currentPeriod.firstPhaseId)

  inline def currentRunId(using Ctx, CState): Int = currentPeriod.runId

  inline def currentPhaseId(using Ctx, CState): Int = currentPeriod.phaseId

  def currentlyAfterTyper(using Ctx, CState): Boolean = ctx.base.isAfterTyper(currentPhase)

  /** Does current phase use an erased types interpretation? */
  def currentlyAfterErasure(using Ctx, CState): Boolean = currentPhase.erasedTypes

  inline def inMode[T](mode: Mode)(inline op: (Ctx, CState) ?=> T)(using ctx: Ctx, cs: CState): T =
    op(using if mode != ctx.mode then ctx.fresh.setMode(mode) else ctx)

  inline def withMode[T](mode: Mode)(inline op: (Ctx, CState) ?=> T)(using ctx: Ctx, cs: CState): T =
    inMode(ctx.mode | mode)(op)

  inline def withoutMode[T](mode: Mode)(inline op: (Ctx, CState) ?=> T)(using ctx: Ctx, cs: CState): T =
    inMode(ctx.mode &~ mode)(op)

  type CState = Period

  inline def currentState(using cs: CState): CState = cs

  abstract class Ctx:
    val base: ContextBase
    def outersIterator: Iterator[Ctx]
    def outer: Ctx
    def mode: Mode
    def owner: Symbol
    def tree: Tree[? >: Untyped]
    def scope: Scope
    def typerState: TyperState
    def typeAssigner: TypeAssigner
    def typer: Typer
    def importInfo: ImportInfo
    def gadt: GadtConstraint
    def searchHistory: SearchHistory
    def typeComparer: TypeComparer
    def source: SourceFile
    def moreProperties: Map[Key[Any], Any]
    def property[T](key: Key[T]): Option[T]
    def store: Store
    def compilerCallback: CompilerCallback
    def sbtCallback: AnalysisCallback
    def printer: Printer
    def settingsState: SettingsState
    def compilationUnit: CompilationUnit
    def run: Run
    def notNullInfos: List[NotNullInfo]
    def implicits: ContextualImplicits
    def reporter: Reporter
    def getSource(file: AbstractFile, codec: => Codec = Codec(settings.encoding.value(using this))): SourceFile
    def getSource(path: TermName): SourceFile
    def getSource(path: String): SourceFile
    def isClassDefContext: Boolean
    def isImportContext: Boolean
    def isNonEmptyScopeContext: Boolean
    def isInlineContext: Boolean
    def debug: Boolean
    def explicitNulls: Boolean

    def settings: ScalaSettings
    def definitions: Definitions
    def platform: Platform
    def pendingUnderlying: mutable.HashSet[Type]
    def uniqueNamedTypes: Uniques.NamedTypeUniques
    def uniques: util.HashSet[Type]

    def fresh: FreshCtx
    def freshOver(outer: Ctx): FreshCtx

    def withPeriod(pd: Period): Context
    def withPhase(phaseId: PhaseId): Context
    def withPhase(phase: Phase): Context
    def withOwner(owner: Symbol): Ctx
    def withSource(source: SourceFile): Ctx
    def withProperty[T](key: Key[T], value: Option[T]): Ctx
    def addMode(mode: Mode): Ctx
    def retractMode(mode: Mode): Ctx

    def toContext(using cs: CState) = withPeriod(cs)

    private[Contexts] def storedSourceCtx(source: SourceFile): Ctx

    def toContextUNSAFE = asInstanceOf[Context]
  end Ctx

  trait FreshCtx extends Ctx:
    def setPeriod(period: Period): this.type
    def setMode(mode: Mode): this.type
    def addMode(mode: Mode): this.type
    def retractMode(mode: Mode): this.type
    def setOwner(owner: Symbol): this.type
    def setTree(tree: Tree[? >: Untyped]): this.type
    def setScope(scope: Scope): this.type
    def setNewScope: this.type
    def setTyperState(typerState: TyperState): this.type
    def setNewTyperState(): this.type
    def setExploreTyperState(): this.type
    def setReporter(reporter: Reporter): this.type
    def setTypeAssigner(typeAssigner: TypeAssigner): this.type
    def setTyper(typer: Typer): this.type
    def setGadt(gadt: GadtConstraint): this.type
    def setFreshGADTBounds: this.type
    def setSearchHistory(searchHistory: SearchHistory): this.type
    def setSource(source: SourceFile): this.type
    def setTypeComparerFn(tcfn: Context => TypeComparer): this.type
    def setImplicits(implicits: ContextualImplicits): this.type
    def setCompilationUnit(compilationUnit: CompilationUnit): this.type
    def setCompilerCallback(callback: CompilerCallback): this.type
    def setSbtCallback(callback: AnalysisCallback): this.type
    def setPrinterFn(printer: Context => Printer): this.type
    def setSettings(settingsState: SettingsState): this.type
    def setRun(run: Run): this.type
    def setNotNullInfos(notNullInfos: List[NotNullInfo]): this.type
    def setImportInfo(importInfo: ImportInfo): this.type
    def setProperty[T](key: Key[T], value: T): this.type
    def dropProperty(key: Key[?]): this.type
    def addLocation[T](initial: T): Store.Location[T]
    def addLocation[T](): Store.Location[T]
    def updateStore[T](loc: Store.Location[T], value: T): this.type
    def setPhase(pid: PhaseId): this.type
    def setPhase(phase: Phase): this.type
    def setSetting[T](setting: Setting[T], value: T): this.type
    def setDebug: this.type
  end FreshCtx

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
  abstract class Context(val base: ContextBase) extends Ctx { thiscontext =>

    protected given Context = this
    protected given CState = cstate

    /** All outer contexts, ending in `base.initialCtx` and then `NoContext` */
    def outersIterator: Iterator[Ctx] = new Iterator[Ctx] {
      var current: Ctx = thiscontext
      def hasNext = current != NoContext
      def next = { val c = current; current = current.outer; c }
    }

    /** The outer context */
    private var _outer: Ctx = _
    protected def outer_=(outer: Ctx): Unit = _outer = outer
    final def outer: Ctx = _outer

    /** The current context */
    private var _period: Period = _
    protected def period_=(period: Period): Unit = {
      assert(period.firstPhaseId == period.lastPhaseId, period)
      _period = period
    }
    final def period: Period = _period

    /** The scope nesting level */
    private var _mode: Mode = _
    protected def mode_=(mode: Mode): Unit = _mode = mode
    final def mode: Mode = _mode

    /** The current owner symbol */
    private var _owner: Symbol = _
    protected def owner_=(owner: Symbol): Unit = _owner = owner
    final def owner: Symbol = _owner

    /** The current tree */
    private var _tree: Tree[? >: Untyped]= _
    protected def tree_=(tree: Tree[? >: Untyped]): Unit = _tree = tree
    final def tree: Tree[? >: Untyped] = _tree

    /** The current scope */
    private var _scope: Scope = _
    protected def scope_=(scope: Scope): Unit = _scope = scope
    final def scope: Scope = _scope

    /** The current type comparer */
    private var _typerState: TyperState = _
    protected def typerState_=(typerState: TyperState): Unit = _typerState = typerState
    final def typerState: TyperState = _typerState

    /** The current type assigner or typer */
    private var _typeAssigner: TypeAssigner = _
    protected def typeAssigner_=(typeAssigner: TypeAssigner): Unit = _typeAssigner = typeAssigner
    final def typeAssigner: TypeAssigner = _typeAssigner

    /** The current bounds in force for type parameters appearing in a GADT */
    private var _gadt: GadtConstraint = _
    protected def gadt_=(gadt: GadtConstraint): Unit = _gadt = gadt
    final def gadt: GadtConstraint = _gadt

    /** The history of implicit searches that are currently active */
    private var _searchHistory: SearchHistory = null
    protected def searchHistory_= (searchHistory: SearchHistory): Unit = _searchHistory = searchHistory
    final def searchHistory: SearchHistory = _searchHistory

    /** The current type comparer. This ones updates itself automatically for
     *  each new context.
     */
    private var _typeComparer: TypeComparer = _
    protected def typeComparer_=(typeComparer: TypeComparer): Unit = _typeComparer = typeComparer
    def typeComparer: TypeComparer = {
      if (_typeComparer.comparerCtx ne this)
        _typeComparer = _typeComparer.copyIn(this)
      _typeComparer
    }

    /** The current source file */
    private var _source: SourceFile = _
    protected def source_=(source: SourceFile): Unit = _source = source
    final def source: SourceFile = _source

    /** A map in which more contextual properties can be stored
     *  Typically used for attributes that are read and written only in special situations.
     */
    private var _moreProperties: Map[Key[Any], Any] = _
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
    protected def printerFn: Context => Printer = store(printerFnLoc)

    /** A function creating a printer */
    def printer: Printer =
      val pr = printerFn(this)
      if this.settings.YplainPrinter.value then pr.plain else pr

    /** The current settings values */
    def settingsState: SettingsState = store(settingsStateLoc)

    /** The current compilation unit */
    def compilationUnit: CompilationUnit = store(compilationUnitLoc)

    /** The current compiler-run */
    def run: Run = store(runLoc)

    /** The paths currently known to be not null */
    def notNullInfos = store(notNullInfosLoc)

    /** The currently active import info */
    def importInfo = store(importInfoLoc)

    /** The new implicit references that are introduced by this scope */
    protected var implicitsCache: ContextualImplicits = null
    def implicits: ContextualImplicits = {
      if (implicitsCache == null)
        implicitsCache = {
          val implicitRefs: List[ImplicitRef] =
            if (isClassDefContext)
              try owner.thisType.implicitMembers
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
    def getSource(file: AbstractFile, codec: => Codec) = {
      util.Stats.record("getSource")
      base.sources.getOrElseUpdate(file, new SourceFile(file, codec))
    }

    /** Sourcefile with given path name, memoized */
    def getSource(path: TermName): SourceFile = base.sourceNamed.get(path) match {
      case Some(source) =>
        source
      case None => try {
        val f = new PlainFile(Path(path.toString))
        val src = getSource(f)
        base.sourceNamed(path) = src
        src
      } catch {
        case ex: InvalidPathException =>
          report.error(s"invalid file path: ${ex.getMessage}")
          NoSource
      }
    }

    /** Sourcefile with given path, memoized */
    def getSource(path: String): SourceFile = getSource(path.toTermName)

    /** Those fields are used to cache phases created in withPhase.
      * phasedCtx is first phase with altered phase ever requested.
      * phasedCtxs is array that uses phaseId's as indexes,
      * contexts are created only on request and cached in this array
      */
    private var phasedCtx: Context = this
    private var phasedCtxs: Array[Context] = null

    def withPeriod(pd: Period): Context =
      if period == pd then this else fresh.setPeriod(pd)

    /** This context at given phase.
     *  This method will always return a phase period equal to phaseId, thus will never return squashed phases
     */
    final def withPhase(phaseId: PhaseId): Context =
      if (this.period.phaseId == phaseId) this
      else if (phasedCtx.period.phaseId == phaseId) phasedCtx
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
      if (phase.exists && period.phaseId > phase.id) withPhase(phase) else this

    final def withPhaseNoEarlier(phase: Phase): Context =
      if (phase.exists && period.phaseId < phase.id) withPhase(phase) else this

    // `creationTrace`-related code. To enable, uncomment the code below and the
    // call to `setCreationTrace()` in this file.
    /*
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
    */

    /** The current reporter */
    def reporter: Reporter = typerState.reporter

    /** Is this a context for the members of a class definition? */
    def isClassDefContext: Boolean =
      owner.isClass && (owner ne outer.owner)

    /** Is this a context that introduces an import clause? */
    def isImportContext: Boolean =
      (this ne NoContext)
      && (outer ne NoContext)
      && (this.importInfo ne outer.importInfo)

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
      while (c != NoContext && !c.tree.isInstanceOf[Template[?]] && !c.tree.isInstanceOf[PackageDef[?]])
        c = c.outer
      c
    }*/

    /** Is the debug option set? */
    def debug: Boolean = base.settings.Ydebug.value

    /** Is the explicit nulls option set? */
    def explicitNulls: Boolean = base.settings.YexplicitNulls.value

    /** Initialize all context fields, except typerState, which has to be set separately
     *  @param  outer   The outer context
     *  @param  origin  The context from which fields are copied
     */
    private[Contexts] def init(outer: Ctx, origin: Context): this.type =
      init(outer, origin, origin.cstate)

    private[Contexts] def init(outer: Ctx, origin: Ctx, cstate: CState): this.type = {
      _outer = outer
      _period = cstate
      _mode = origin.mode
      _owner = origin.owner
      _tree = origin.tree
      _scope = origin.scope
      _typeAssigner = origin.typeAssigner
      _gadt = origin.gadt
      _searchHistory = origin.searchHistory
      _typeComparer = origin.typeComparer
      _source = origin.source
      _moreProperties = origin.moreProperties
      _store = origin.store
      this
    }

    def reuseIn(outer: Context): this.type =
      implicitsCache = null
      phasedCtxs = null
      sourceCtx = null
      init(outer, outer)

    /** A fresh clone of this context embedded in this context. */
    def fresh: FreshContext = freshOver(this)

    /** A fresh clone of this context embedded in the specified `outer` context. */
    def freshOver(outer: Ctx): FreshContext =
      util.Stats.record("Context.fresh")
      FreshContext(base).init(outer, this).setTyperState(this.typerState)

    final def withOwner(owner: Symbol): Context =
      if (owner ne this.owner) fresh.setOwner(owner) else this

    private var sourceCtx: SimpleIdentityMap[SourceFile, Ctx] = null

    private[Contexts] def storedSourceCtx(source: SourceFile): Ctx =
      if sourceCtx == null then null
      else sourceCtx(source)

    final def withSource(source: SourceFile): Ctx =
      if source eq this.source then this
      else if (source eq outer.source)
              && (outer.storedSourceCtx(this.source) eq this) then outer
      else {
        if (sourceCtx == null) sourceCtx = SimpleIdentityMap.Empty
        val prev = sourceCtx(source)
        if (prev != null) prev
        else {
          val newCtx = fresh.setSource(source)
          if (newCtx.compilationUnit == null)
            // `source` might correspond to a file not necessarily
            // in the current project (e.g. when inlining library code),
            // so set `mustExist` to false.
            newCtx.setCompilationUnit(CompilationUnit(source, mustExist = false))
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

    def withModeBits(mode: Mode): Context =
      if (mode != this.mode) fresh.setMode(mode) else this

    def addMode(mode: Mode): Context = withModeBits(this.mode | mode)
    def retractMode(mode: Mode): Context = withModeBits(this.mode &~ mode)

    def typer: Typer = this.typeAssigner match {
      case typer: Typer => typer
      case _ => new Typer
    }

    def cstate: CState = period

    override def toString: String = {
      def iinfo(using Ctx, CState) = if (ctx.importInfo == null) "" else i"${ctx.importInfo.selectors}%, %"
      "Context(\n" +
      (outersIterator.map(ctx => s"  owner = ${ctx.owner}, scope = ${ctx.scope}, import = ${iinfo(using ctx)}").mkString("\n"))
    }

    def settings: ScalaSettings            = base.settings
    def definitions: Definitions           = base.definitions
    def platform: Platform                 = base.platform
    def pendingUnderlying: mutable.HashSet[Type]   = base.pendingUnderlying
    def uniqueNamedTypes: Uniques.NamedTypeUniques = base.uniqueNamedTypes
    def uniques: util.HashSet[Type]                = base.uniques

    def initialize()(using Context): Unit = base.initialize()
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
  class FreshContext(base: ContextBase) extends Context(base), FreshCtx {
    def setPeriod(period: Period): this.type =
      util.Stats.record("Context.setPeriod")
      this.period = period
      this
    def setMode(mode: Mode): this.type =
      util.Stats.record("Context.setMode")
      this.mode = mode
      this
    override final def addMode(mode: Mode): this.type = setMode(this.mode | mode)
    override final def retractMode(mode: Mode): this.type = setMode(this.mode &~ mode)
    def setOwner(owner: Symbol): this.type =
      util.Stats.record("Context.setOwner")
      assert(owner != NoSymbol)
      this.owner = owner
      this
    def setTree(tree: Tree[? >: Untyped]): this.type =
      util.Stats.record("Context.setTree")
      this.tree = tree
      this
    def setScope(scope: Scope): this.type = { this.scope = scope; this }
    def setNewScope: this.type =
      util.Stats.record("Context.setScope")
      this.scope = newScope
      this
    def setTyperState(typerState: TyperState): this.type = { this.typerState = typerState; this }
    def setNewTyperState(): this.type = setTyperState(typerState.fresh().setCommittable(true))
    def setExploreTyperState(): this.type = setTyperState(typerState.fresh().setCommittable(false))
    def setReporter(reporter: Reporter): this.type = setTyperState(typerState.fresh().setReporter(reporter))
    def setTypeAssigner(typeAssigner: TypeAssigner): this.type =
      util.Stats.record("Context.setTypeAssigner")
      this.typeAssigner = typeAssigner
      this
    def setTyper(typer: Typer): this.type = { this.scope = typer.scope; setTypeAssigner(typer) }
    def setGadt(gadt: GadtConstraint): this.type =
      util.Stats.record("Context.setGadt")
      this.gadt = gadt
      this
    def setFreshGADTBounds: this.type = setGadt(gadt.fresh)
    def setSearchHistory(searchHistory: SearchHistory): this.type =
      util.Stats.record("Context.setSearchHistory")
      this.searchHistory = searchHistory
      this
    def setSource(source: SourceFile): this.type =
      util.Stats.record("Context.setSource")
      this.source = source
      this
    def setTypeComparerFn(tcfn: Context => TypeComparer): this.type = { this.typeComparer = tcfn(this); this }
    private def setMoreProperties(moreProperties: Map[Key[Any], Any]): this.type =
      util.Stats.record("Context.setMoreProperties")
      this.moreProperties = moreProperties
      this
    private def setStore(store: Store): this.type =
      util.Stats.record("Context.setStore")
      this.store = store
      this
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
    def setNotNullInfos(notNullInfos: List[NotNullInfo]): this.type = updateStore(notNullInfosLoc, notNullInfos)
    def setImportInfo(importInfo: ImportInfo): this.type = updateStore(importInfoLoc, importInfo)

    def setProperty[T](key: Key[T], value: T): this.type =
      setMoreProperties(moreProperties.updated(key, value))

    def dropProperty(key: Key[?]): this.type =
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

    def setPhase(pid: PhaseId): this.type = setPeriod(Period(period.runId, pid))
    def setPhase(phase: Phase): this.type = setPeriod(Period(period.runId, phase.start, phase.end))

    def setSetting[T](setting: Setting[T], value: T): this.type =
      setSettings(setting.updateIn(settingsState, value))

    def setDebug: this.type = setSetting(base.settings.Ydebug, true)
  }

  given ops as AnyRef:
    extension (c: Ctx):
      def addNotNullInfo(info: NotNullInfo)(using CState) =
        c.withNotNullInfos(c.notNullInfos.extendWith(info))

      def addNotNullRefs(refs: Set[TermRef])(using CState) =
        c.addNotNullInfo(NotNullInfo(refs, Set()))

      def withNotNullInfos(infos: List[NotNullInfo])(using CState): Ctx =
        if c.notNullInfos eq infos then c else c.fresh.setNotNullInfos(infos)
  end ops

  /** Test `op` in a fresh context with a typerstate that is not committable.
   *  The passed context may not survive the operation.
   */
   def explore[T](op: (Ctx, CState) ?=> T)(using Ctx, CState): T =
    util.Stats.record("Context.test")
    val base = ctx.base
    import base._
    val nestedCtx =
      if testsInUse < testContexts.size then
        testContexts(testsInUse).reuseIn(ctx.toContextUNSAFE)
      else
        val ts = TyperState()
          .setReporter(TestingReporter())
          .setCommittable(false)
        val c = FreshContext(ctx.base).init(ctx, ctx, currentPeriod).setTyperState(ts)
        testContexts += c
        c
    testsInUse += 1
    val nestedTS = nestedCtx.typerState
    nestedTS.init(ctx.typerState, ctx.typerState.constraint)
    val result =
      try op(using nestedCtx)
      finally
        nestedTS.reporter.asInstanceOf[TestingReporter].reset()
        testsInUse -= 1
    result
  end explore

  /** A class defining the initial context with given context base
   *  and set of possible settings.
   */
  private class InitialContext(base: ContextBase, settingsGroup: SettingGroup) extends FreshContext(base) {
    outer = NoContext
    period = InitialPeriod
    mode = Mode.None
    typerState = TyperState.initialState()
    owner = NoSymbol
    tree = untpd.EmptyTree
    typeAssigner = TypeAssigner
    moreProperties = Map(MessageLimiter -> DefaultMessageLimiter())
    source = NoSource
    store = initialStore
      .updated(settingsStateLoc, settingsGroup.defaultState)
      .updated(notNullInfosLoc, Nil)
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
                       with Phases.PhasesBase
                       with Plugins {

    /** The applicable settings */
    val settings: ScalaSettings = new ScalaSettings

    /** The initial context */
    val initialCtx: Context = new InitialContext(this, settings)

    /** The platform, initialized by `initPlatform()`. */
    private var _platform: Platform = _

    /** The platform */
    def platform: Platform = {
      if (_platform == null)
        throw new IllegalStateException(
            "initialize() must be called before accessing platform")
      _platform
    }

    protected def newPlatform(using Ctx, CState): Platform =
      if (settings.scalajs.value) new SJSPlatform
      else new JavaPlatform

    /** The loader that loads the members of _root_ */
    def rootLoader(root: TermSymbol)(using Ctx, CState): SymbolLoader = platform.rootLoader(root)

    // Set up some phases to get started */
    usePhases(List(SomePhase))

    /** The standard definitions */
    val definitions: Definitions = new Definitions

    /** Initializes the `ContextBase` with a starting context.
     *  This initializes the `platform` and the `definitions`.
     */
    def initialize()(using ctx: Context): Unit = {
      given CState = ctx.cstate
      _platform = newPlatform
      definitions.init()
    }

    def squashed(p: Phase): Phase =
      allPhases.find(_.period.containsPhaseId(p.id)).getOrElse(NoPhase)
  }

  /** The essential mutable state of a context base, collected into a common class */
  class ContextState {
    // Symbols state

    /** Counter for unique symbol ids */
    private var _nextSymId: Int = 0
    def nextSymId: Int = { _nextSymId += 1; _nextSymId }

    /** Sources that were loaded */
    val sources: mutable.HashMap[AbstractFile, SourceFile] = new mutable.HashMap[AbstractFile, SourceFile]
    val sourceNamed: mutable.HashMap[TermName, SourceFile] = new mutable.HashMap[TermName, SourceFile]

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
      uniqueSets.transform((_, s) => (s.size, s.accesses, s.misses))

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

    /** A map from ErrorType to associated message. We use this map
     *  instead of storing messages directly in ErrorTypes in order
     *  to avoid space leaks - the message usually captures a context.
     */
    private[core] val errorTypeMsg: mutable.Map[Types.ErrorType, Message] = mutable.Map()

    // Phases state

    private[core] var phasesPlan: List[List[Phase]] = _

    /** Phases by id */
    private[dotc] var phases: Array[Phase] = _

    /** Phases with consecutive Transforms grouped into a single phase, Empty array if squashing is disabled */
    private[core] var fusedPhases: Array[Phase] = Array.empty[Phase]

    /** Next denotation transformer id */
    private[core] var nextDenotTransformerId: Array[Int] = _

    private[core] var denotTransformers: Array[DenotTransformer] = _

    // Reporters state
    private[dotc] var indent: Int = 0

    protected[dotc] val indentTab: String = "  "

    private[dotc] val testContexts = new mutable.ArrayBuffer[FreshContext]
    private[dotc] var testsInUse: Int = 0

    def reset(): Unit = {
      for ((_, set) <- uniqueSets) set.clear()
      errorTypeMsg.clear()
      sources.clear()
      sourceNamed.clear()
    }

    // Test that access is single threaded

    /** The thread on which `checkSingleThreaded was invoked last */
    @sharable private var thread: Thread = null

    /** Check that we are on the same thread as before */
    def checkSingleThreaded(): Unit =
      if (thread == null) thread = Thread.currentThread()
      else assert(thread == Thread.currentThread(), "illegal multithreaded access to ContextBase")
  }
}
