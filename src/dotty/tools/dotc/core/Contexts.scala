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
import TypeComparers._, NameOps._, SymDenotations._, util.Positions._
import ast.Trees._, ast.untpd
import util.FreshNameCreator
import typer._
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

    /** The current set of type constraints */
    private[this] var _constraints: Constraints = _
    protected def constraints_=(constraints: Constraints) = _constraints = constraints
    def constraints: Constraints = _constraints

    /** The scope nesting level */
    private[this] var _scopeNestingLevel: Int = 0
    def scopeNestingLevel: Int = {
      if (this._scopeNestingLevel == outer.scopeNestingLevel && this.scope != outer.scope)
        this._scopeNestingLevel = outer.scopeNestingLevel + 1
      this._scopeNestingLevel
    }

    /** The current type comparer */
    private[this] var _typeComparer: TypeComparer = _
    protected def typeComparer_=(typeComparer: TypeComparer) = _typeComparer = typeComparer

    def typeComparer: TypeComparer = {
      if ((_typeComparer == null) ||
          (_typeComparer eq outer.typeComparer) && (constraints ne outer.constraints))
        _typeComparer = new TypeComparer()
      _typeComparer
    }

    /** The current position */
    private[this] var _position: Position = _
    protected def position_=(position: Position) = _position = position
    def position: Position = _position

    /** The current plain printer */
    private[this] var _plainPrinter: Context => Printer = _
    protected def plainPrinter_=(plainPrinter: Context => Printer) = _plainPrinter = plainPrinter
    def plainPrinter: Context => Printer = _plainPrinter

    /** The current refined printer */
    private[this] var _refinedPrinter: Context => Printer = _
    protected def refinedPrinter_=(refinedPrinter: Context => Printer) = _refinedPrinter = refinedPrinter
    def refinedPrinter: Context => Printer = _refinedPrinter

    /** The current owner symbol */
    private[this] var _owner: Symbol = _
    protected def owner_=(owner: Symbol) = _owner = owner
    def owner: Symbol = _owner

    /** The current settings values */
    private[this] var _sstate: SettingsState = _
    protected def sstate_=(sstate: SettingsState) = _sstate = sstate
    def sstate: SettingsState = _sstate

    /** The current tree */
    private[this] var _tree: Tree[_ >: Untyped] = _
    protected def tree_=(tree: Tree[_ >: Untyped]) = _tree = tree
    def tree: Tree[_ >: Untyped] = _tree

    /** The current scope */
    private[this] var _scope: Scope = _
    protected def scope_=(scope: Scope) = _scope = scope
    def scope: Scope = _scope

    /** The currently visible imports */
    private[this] var _imports: List[ImportInfo] = _
    protected def imports_=(imports: List[ImportInfo]) = _imports = imports
    def imports: List[ImportInfo] = _imports

    /** The current reporter */
    private[this] var _reporter: Reporter = _
    protected def reporter_=(reporter: Reporter) = _reporter = reporter
    def reporter: Reporter = _reporter

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


    /** If -Ydebug is on, the top of the stack trace where this context
     *  was created, otherwise `null`.
     */
    private var creationTrace: Array[StackTraceElement] = _

    setCreationTrace()

    private def setCreationTrace() =
      if (true || this.settings.debug.value)
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
    def source = util.NoSource // for now

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
          // constraints is not preserved in condensed
          .withPlainPrinter(plainPrinter)
          .withRefinedPrinter(refinedPrinter)
          .withOwner(owner)
          .withSettings(sstate)
          // tree is not preserved in condensed
          .withReporter(reporter)
          .withDiagnostics(diagnostics)
          .withMoreProperties(moreProperties)
      _condensed
    }

    /** A fresh clone of this context. */
    def fresh: FreshContext = {
      val newctx = super.clone.asInstanceOf[FreshContext]
      newctx.outer = this
      newctx.setCreationTrace()
      newctx
    }
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
    def withConstraints(constraints: Constraints): this.type = { this.constraints = constraints; this }
    def withPosition(position: Position): this.type = { this.position = position; this }
    def withPlainPrinter(printer: Context => Printer): this.type = { this.plainPrinter = printer; this }
    def withRefinedPrinter(printer: Context => Printer): this.type = { this.refinedPrinter = printer; this }
    def withOwner(owner: Symbol): this.type = { this.owner = owner; this }
    def withNewScope: this.type = { this.scope = newScope; this }
    def withSettings(sstate: SettingsState): this.type = { this.sstate = sstate; this }
    def withTree(tree: Tree[_ >: Untyped]): this.type = { this.tree = tree; this }
    def withScope(scope: Scope): this.type = { this.scope = scope; this }
    def withImport(importInfo: ImportInfo): this.type = { this.imports = importInfo :: imports; this }
    def withReporter(reporter: Reporter): this.type = { this.reporter = reporter; this }
    def withDiagnostics(diagnostics: Option[StringBuilder]): this.type = { this.diagnostics = diagnostics; this }
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
    constraints = Map()
    position = NoPosition
    plainPrinter = new PlainPrinter(_)
    refinedPrinter = new RefinedPrinter(_)
    owner = NoSymbol
    sstate = settings.defaultState
    tree = untpd.EmptyTree
    reporter = new ConsoleReporter()(this)
    diagnostics = None
    moreProperties = Map.empty
  }

  object NoContext extends Context {
    override def typeComparer = null
    lazy val base = unsupported("base")
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

    NoPhase // initialize some phases
    SomePhase // TODO: Is there a cleaner way to do this?

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
    private[core] val superIdOfClass = new mutable.HashMap[TypeRef, Int]

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

    /** The number of recursive invocation of underlying on a NamedType
     *  during a controlled operation.
     */
    private[core] var underlyingRecursions: Int = 0

    /** The set of named types on which a currently active invocation
     *  of underlying during a controlled operation exists. */
    private[core] val pendingUnderlying = new mutable.HashSet[Type]

    // Phases state
    /** Phases by id */
    private[core] var phases = new Array[Phase](MaxPossiblePhaseId + 1)

    /** The number of defined phases. This includes NoPhase, so nphases >= 1 */
    private[core] var nphases = 0

    // Printers state
    /** Number of recursive invocations of a show method on cuyrrent stack */
    private[dotc] var toTextRecursions = 0

    // Reporters state
    private[dotc] var indent = 0

    protected[dotc] val indentTab = "  "
  }

  object Context {

    /** Implicit conversion that injects all printer operations into a context */
    implicit def toPrinter(ctx: Context) = ctx.printer

    /** implicit conversion that injects all ContextBase members into a context */
    implicit def toBase(ctx: Context): ContextBase = ctx.base

    val theBase = new ContextBase // !!! DEBUG, so that we can use a minimal context for reporting even in code that normallly cannot access a context
  }

  /** Initial size of superId table */
  private final val InitialSuperIdsSize = 4096

  /** Initial capacity of uniques HashMap */
  private[core] final val initialUniquesCapacity = 50000

  /** How many recursive calls to NamedType#underlying are performed before
   *  logging starts.
   */
  private[core] final val LogPendingUnderlyingThreshold = 50

  /** How many recursive calls to isSubType are performed before
   *  logging starts.
   */
  private[core] final val LogPendingSubTypesThreshold = 50
}
