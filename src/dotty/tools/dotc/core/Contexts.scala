package dotty.tools
package dotc
package core

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
import util.Positions._
import ast.Trees._
import ast.untpd
import util.{FreshNameCreator, SimpleMap, SourceFile, NoSource}
import typer._
import Implicits.ContextualImplicits
import config.Settings._
import reporting._
import collection.mutable
import collection.immutable.BitSet
import printing._
import config.{Settings, ScalaSettings, Platform, JavaPlatform}
import language.implicitConversions

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
   *      a "condensed context", typically named `cctx` (or they create one). Consensed contexts
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

    /** The current context */
    private[this] var _period: Period = _
    protected def period_=(period: Period) = _period = period
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

    /** The current typer */
    private[this] var _typer: Typer = _
    protected def typer_=(typer: Typer) = _typer = typer
    def typer: Typer = _typer

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

    /** The new implicit references that are introduced by this scope */
    private var implicitsCache: ContextualImplicits = null
    def implicits: ContextualImplicits = {
      if (implicitsCache == null )
        implicitsCache = {
          val implicitRefs: List[TermRef] =
            if (isClassDefContext) owner.thisType.implicitMembers
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

  /** If -Ydebug is on, the top of the stack trace where this context
     *  was created, otherwise `null`.
     */
    private var creationTrace: Array[StackTraceElement] = _

    private def setCreationTrace() =
      if (this.settings.debug.value)
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
     */
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

    /** A fresh clone of this context. */
    def fresh: FreshContext = {
      val newctx: Context = super.clone.asInstanceOf[FreshContext]
      newctx.outer = this
      newctx.implicitsCache = null
      newctx.setCreationTrace()
        // Dotty deviation: Scala2x allows access to private members implicitCache and setCreationTrace
        // even from a subclass prefix. Dotty (and Java) do not. I think that's a bug in Scala2x.
      newctx.asInstanceOf[FreshContext]
    }

    final def withMode(mode: Mode): Context =
      if (mode != this.mode) fresh.withNewMode(mode) else this

    final def addMode(mode: Mode): Context = withMode(this.mode | mode)
    final def maskMode(mode: Mode): Context = withMode(this.mode & mode)
    final def retractMode(mode: Mode): Context = withMode(this.mode &~ mode)

    override def toString =
      "Context(\n" +
      (outersIterator map ( ctx => s"  owner = ${ctx.owner}, scope = ${ctx.scope}") mkString "\n")
  }

  /** A condensed context provides only a small memory footprint over
   *  a Context base, and therefore can be stored without problems in
   *  long-lived objects.
   */
  abstract class CondensedContext extends Context {
    override def condensed = this
  }

  /** A fresh context allows selective modification
   *  of its attributes using the with... methods.
   */
  abstract class FreshContext extends CondensedContext {
    def withPeriod(period: Period): this.type = { this.period = period; this }
    def withNewMode(mode: Mode): this.type = { this.mode = mode; this }
    def withTyperState(typerState: TyperState): this.type = { this.typerState = typerState; this }
    def withNewTyperState: this.type = withTyperState(typerState.fresh(isCommittable = true))
    def withExploreTyperState: this.type = withTyperState(typerState.fresh(isCommittable = false))
    def withPrinterFn(printer: Context => Printer): this.type = { this.printerFn = printer; this }
    def withOwner(owner: Symbol): this.type = { assert(owner != NoSymbol); this.owner = owner; this }
    def withSettings(sstate: SettingsState): this.type = { this.sstate = sstate; this }
    def withCompilationUnit(compilationUnit: CompilationUnit): this.type = { this.compilationUnit = compilationUnit; this }
    def withTree(tree: Tree[_ >: Untyped]): this.type = { this.tree = tree; this }
    def withScope(scope: Scope): this.type = { this.scope = scope; this }
    def withNewScope: this.type = { this.scope = newScope; this }
    def withTyper(typer: Typer): this.type = { this.typer = typer; this.scope = typer.scope; this }
    def withImportInfo(importInfo: ImportInfo): this.type = { this.importInfo = importInfo; this }
    def withRunInfo(runInfo: RunInfo): this.type = { this.runInfo = runInfo; this }
    def withDiagnostics(diagnostics: Option[StringBuilder]): this.type = { this.diagnostics = diagnostics; this }
    def withTypeComparerFn(tcfn: Context => TypeComparer): this.type = { this.typeComparer = tcfn(this); this }
    def withSearchHistory(searchHistory: SearchHistory): this.type = { this.searchHistory = searchHistory; this }
    def withMoreProperties(moreProperties: Map[String, Any]): this.type = { this.moreProperties = moreProperties; this }

    def withProperty(prop: (String, Any)): this.type = withMoreProperties(moreProperties + prop)

    def withPhase(pid: PhaseId): this.type = withPeriod(Period(runId, pid))

    def withSetting[T](setting: Setting[T], value: T): this.type =
      withSettings(setting.updateIn(sstate, value))

    def withDebug = withSetting(base.settings.debug, true)
  }

  /** A class defining the initial context with given context base
   *  and set of possible settings.
   */
  private class InitialContext(val base: ContextBase, settings: SettingGroup) extends FreshContext {
    outer = NoContext
    period = InitialPeriod
    mode = Mode.None
    typerState = new TyperState(new ThrowingReporter(new ConsoleReporter()(this)))
    printerFn = new RefinedPrinter(_)
    owner = NoSymbol
    sstate = settings.defaultState
    tree = untpd.EmptyTree
    runInfo = new RunInfo(this)
    diagnostics = None
    moreProperties = Map.empty
    typeComparer = new TypeComparer(this)
    searchHistory = new SearchHistory(0, Map())
  }

  object NoContext extends Context {
    lazy val base = unsupported("base")
    override val implicits: ContextualImplicits = new ContextualImplicits(Nil, null)(this)
  }

  /** A context base defines state and associated methods that exist once per
   *  compiler run.
   */
  class ContextBase extends ContextState
                       with Transformers.TransformerBase
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

    /** The standard fresh name creator */
    val freshNames = new FreshNameCreator.Default

    def freshName(prefix: String = ""): String = freshNames.newName(prefix)
    def freshName(prefix: Name): String = freshName(prefix.toString)

    /** The loader that loads the members of _root_ */
    def rootLoader(root: TermSymbol)(implicit ctx: Context): SymbolLoader = platform.rootLoader(root)

    // Set up some phases to get started */
    usePhases(SomePhase :: Nil)

    /** The standard definitions */
    val definitions = new Definitions()(initialCtx)
  }

  /** The essential mutable state of a context base, collected into a common class */
  class ContextState {

    // Symbols state

    /** A counter for unique ids */
    private[core] var _nextId = 0

    def nextId = { _nextId += 1; _nextId }

    /** A map from a superclass id to the typeref of the class that has it */
    private[core] var classOfId = new Array[TypeRef](InitialSuperIdsSize)

    /** A map from a the typeref of a class to its superclass id */
    private[core] val superIdOfClass = new mutable.AnyRefMap[TypeRef, Int]

    /** The last allocated superclass id */
    private[core] var lastSuperId = -1

    /** Allocate and return next free superclass id */
    private[core] def nextSuperId: Int = {
      lastSuperId += 1;
      if (lastSuperId >= classOfId.length) {
        val tmp = new Array[TypeRef](classOfId.length * 2)
        classOfId.copyToArray(tmp)
        classOfId = tmp
      }
      lastSuperId
    }

    // SymDenotations state
    /** A table where unique superclass bits are kept.
     *  These are bitsets that contain the superclass ids of all base classes of a class.
     *  Used to speed up isSubClass tests.
     */
    private[core] val uniqueBits = new util.HashSet[BitSet]("superbits", 1024)

    // Types state
    /** A table for hash consing unique types */
    private[core] val uniques = new util.HashSet[Type]("uniques", initialUniquesCapacity) {
      override def hash(x: Type): Int = x.hash
    }

    /** A table for hash consing unique refined types */
    private[dotc] val uniqueRefinedTypes = new RefinedUniques

    /** A table for hash consing unique named types */
    private[core] val uniqueNamedTypes = new NamedTypeUniques

    /** A table for hash consing unique type bounds */
    private[core] val uniqueTypeBounds = new TypeBoundsUniques

    private def uniqueMaps = List(uniques, uniqueRefinedTypes, uniqueNamedTypes, uniqueTypeBounds)

    /** A map that associates label and size of all uniques sets */
    def uniquesSize: Map[String, Int] =
      uniqueMaps.map(m => m.label -> m.size).toMap

    /** The number of recursive invocation of underlying on a NamedType
     *  during a controlled operation.
     */
    private[core] var underlyingRecursions: Int = 0

    /** The set of named types on which a currently active invocation
     *  of underlying during a controlled operation exists. */
    private[core] val pendingUnderlying = new mutable.HashSet[Type]

    // Phases state
    /** Phases by id */
    private[core] var phases: Array[Phase] = _

    // Printers state
    /** Number of recursive invocations of a show method on cuyrrent stack */
    private[dotc] var toTextRecursions = 0

    // Reporters state
    private[dotc] var indent = 0

    protected[dotc] val indentTab = "  "

    /** Should warnings and errors containing non-sensical strings be suppressed? */
    private[dotc] var suppressNonSensicalErrors = true
  }

  object Context {

    /** Implicit conversion that injects all printer operations into a context */
    implicit def toPrinter(ctx: Context) = ctx.printer

    /** implicit conversion that injects all ContextBase members into a context */
    implicit def toBase(ctx: Context): ContextBase = ctx.base

    val theBase = new ContextBase // !!! DEBUG, so that we can use a minimal context for reporting even in code that normallly cannot access a context
  }

  /** Info that changes on each compiler run */
  class RunInfo(initctx: Context) extends ImplicitRunInfo {
    implicit val ctx = initctx
  }

  /** Initial size of superId table */
  private final val InitialSuperIdsSize = 4096

  /** Initial capacity of uniques HashMap */
  private[core] final val initialUniquesCapacity = 40000

  /** How many recursive calls to NamedType#underlying are performed before
   *  logging starts.
   */
  private[core] final val LogPendingUnderlyingThreshold = 50

  /** How many recursive calls to isSubType are performed before
   *  logging starts.
   */
  private[core] final val LogPendingSubTypesThreshold = 50
}
