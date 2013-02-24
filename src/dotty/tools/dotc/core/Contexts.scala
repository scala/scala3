package dotty.tools.dotc
package core

import Decorators._
import Periods._
import Names._
import Phases._
import Types._
import Symbols._
import TypeComparers._, Printers._, NameOps._, SymDenotations._, Positions._
import config.Settings._
import config.ScalaSettings
import collection.mutable
import collection.immutable.BitSet
import config.{Settings, Platform, JavaPlatform}

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
   *    - Classes that need contexts that survive initialization are passed
   *      a "condensed context", typically named `cctx` instead. Consensed contexts
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
                            with Printers
                            with Symbols
                            with SymDenotations
                            with Cloneable {
    implicit val ctx: Context = this

    val base: ContextBase

    private[this] var _underlying: Context = _
    protected def underlying_=(underlying: Context) = _underlying = underlying
    def underlying: Context = _underlying

    private[this] var _period: Period = _
    protected def period_=(period: Period) = _period = period
    def period: Period = _period

    private[this] var _constraints: Constraints = _
    protected def constraints_=(constraints: Constraints) = _constraints = constraints
    def constraints: Constraints = _constraints

    private[this] var _typeComparer: TypeComparer = _
    protected def typeComparer_=(typeComparer: TypeComparer) = _typeComparer = typeComparer

    def typeComparer: TypeComparer = {
      if ((_typeComparer eq underlying.typeComparer) &&
          (constraints ne underlying.constraints))
        _typeComparer = new TypeComparer(this)
      _typeComparer
    }

    private[this] var _position: Position = _
    protected def position_=(position: Position) = _position = position
    def position: Position = _position

    private[this] var _plainPrinter: Context => Printer = _
    protected def plainPrinter_=(plainPrinter: Context => Printer) = _plainPrinter = plainPrinter
    def plainPrinter: Context => Printer = _plainPrinter

    private[this] var _refinedPrinter: Context => Printer = _
    protected def refinedPrinter_=(refinedPrinter: Context => Printer) = _refinedPrinter = refinedPrinter
    def refinedPrinter: Context => Printer = _refinedPrinter

    def printer = if (base.settings.debug.value) plainPrinter else refinedPrinter

    private[this] var _owner: Symbol = _
    protected def owner_=(owner: Symbol) = _owner = owner
    def owner: Symbol = _owner

    private[this] var _sstate: SettingsState = _
    protected def sstate_=(sstate: SettingsState) = _sstate = sstate
    def sstate: SettingsState = _sstate

    def phase: Phase = ??? // phase(period.phaseId)
    def enclClass: Context = ???
    def erasedTypes: Boolean = ???
    def debug: Boolean = ???
    def error(msg: String): Unit = ???
    def warning(msg: String): Unit = ???
    def log(msg: String): Unit = ???
    def debuglog(msg: String): Unit = ???
    def inform(msg: String) = ???
    def informTime(msg: String, start: Long): Unit = ???
    def beforeTyper[T](op: => T): T = ???

    private var _condensed: CondensedContext = null
    def condensed: CondensedContext = {
      if (_condensed == null)
        _condensed = base.initialCtx.fresh
          .withPeriod(period)
          .withPlainPrinter(plainPrinter)
          .withRefinedPrinter(refinedPrinter)
          .withSettings(sstate)
      _condensed
    }

    def fresh: FreshContext = {
      val newctx = super.clone.asInstanceOf[FreshContext]
      newctx.underlying = this
      newctx._condensed = null
      newctx
    }
  }

  abstract class CondensedContext extends Context

  abstract class FreshContext extends CondensedContext {
    def withPeriod(period: Period): this.type = { this.period = period; this }
    def withPhase(pid: PhaseId): this.type = withPeriod(Period(runId, pid))
    def withConstraints(constraints: Constraints): this.type = { this.constraints = constraints; this }
    def withPlainPrinter(printer: Context => Printer): this.type = { this.plainPrinter = printer; this }
    def withRefinedPrinter(printer: Context => Printer): this.type = { this.refinedPrinter = printer; this }
    def withOwner(owner: Symbol): this.type = { this.owner = owner; this }
    def withSettings(sstate: SettingsState): this.type = { this.sstate = sstate; this }
    def withDiagnostics(diagnostics: Option[StringBuilder]): this.type = { this.diagnostics = diagnostics; this }
  }

  private class InitialContext(val base: ContextBase) extends FreshContext {
    underlying = NoContext
    period = Nowhere
    constraints = Map()
    plainPrinter = new PlainPrinter(_)
    refinedPrinter = new RefinedPrinter(_)
    owner = NoSymbol
  }

  object NoContext extends Context {
    lazy val base = unsupported("base")
  }

  class ContextBase extends ContextState
                       with Transformers.TransformerBase
                       with Printers.PrinterBase
                       with Denotations.DenotationsBase {

    val settings = new ScalaSettings

    val initialCtx: Context = new InitialContext(this)
        .withSettings(settings.defaultState)

    val loaders = new SymbolLoaders

    val platform: Platform = new JavaPlatform

    def rootLoader(implicit ctx: Context): SymbolLoader = platform.rootLoader

    val definitions = new Definitions()(initialCtx)

  }

  /** Mutable state of a context base, collected into a common class */
  class ContextState {

    // Symbols state

    /** A counter for unique ids */
    private[core] var _nextId = 0

    def nextId = { _nextId += 1; _nextId }

    /** A map from a superclass id to the type-ref of the class that has it */
    private[core] var classOfId = new Array[TypeRef](InitialSuperIdsSize)

    /** A map from a the type-ref of a superclass to its superclass id */
    private[core] val superIdOfClass = new mutable.HashMap[TypeRef, Int]

    /** The last allocate superclass id */
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
    private[core] val uniqueBits = new util.HashSet[BitSet]("superbits", 1024)

    // Types state
    private[core] val uniques = new util.HashSet[Type]("uniques", initialUniquesCapacity) {
      override def hash(x: Type): Int = x.hash
    }

    // TypeOps state
    private[core] var volatileRecursions: Int = 0
    private[core] val pendingVolatiles = new mutable.HashSet[Type]
  }

  object Context {
    implicit def toPrinter(ctx: Context) = ctx.printer(ctx)
  }

  implicit def ctxToBase(ctx: Context): ContextBase = ctx.base

  /** Initial size of superId table */
  private final val InitialSuperIdsSize = 4096

  /** Initial capacity of uniques HashMap */
  private[core] final val initialUniquesCapacity = 50000

  /** How many recursive calls to isVolatile are performed before
   *  logging starts.
   */
  private[core] final val LogVolatileThreshold = 50
}
