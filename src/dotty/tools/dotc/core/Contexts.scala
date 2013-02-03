package dotty.tools.dotc
package core

import Decorators._
import Periods._
import Names._
import Phases._
import Types._
import Symbols._
import TypeComparers._, Printers._, NameOps._, SymDenotations._
import config.Settings._
import config.ScalaSettings
import collection.mutable
import collection.immutable.BitSet
import config.{Settings, Platform, JavaPlatform}

object Contexts {

  abstract class Context extends Periods
                            with Substituters
                            with TypeOps
                            with Printers
                            with Symbols
                            with Cloneable {
    implicit val ctx: Context = this

    def base: ContextBase

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

    private[this] var _printer: Context => Printer = _
    protected def printer_=(printer: Context => Printer) = _printer = printer
    def printer: Context => Printer = _printer

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
    def warning(msg: String) = ???
    def inform(msg: String) = ???

    def fresh: FreshContext = {
      val newctx = super.clone.asInstanceOf[FreshContext]
      newctx.underlying = this
      newctx
    }

    def condensed: CondensedContext = base.initialCtx.fresh
      .withPeriod(period)
      .withPrinter(printer)
      .withSettings(sstate)
  }

  abstract class CondensedContext extends Context

  abstract class FreshContext extends CondensedContext {
    def withPeriod(period: Period): this.type = { this.period = period; this }
    def withPhase(pid: PhaseId): this.type = withPeriod(Period(runId, pid))
    def withConstraints(constraints: Constraints): this.type = { this.constraints = constraints; this }
    def withPrinter(printer: Context => Printer): this.type = { this.printer = printer; this }
    def withOwner(owner: Symbol): this.type = { this.owner = owner; this }
    def withSettings(sstate: SettingsState): this.type = { this.sstate = sstate; this }
    def withDiagnostics(diagnostics: Option[StringBuilder]): this.type = { this.diagnostics = diagnostics; this }
  }

  private class InitialContext(val base: ContextBase) extends FreshContext {
    underlying = NoContext
    period = Nowhere
    constraints = Map()
    printer = new StdPrinter()(_)
    owner = NoSymbol
  }

  object NoContext extends Context {
    val base = unsupported("base")
  }

  class ContextBase extends ContextState with Transformers.TransformerBase
                       with Printers.PrinterBase {

    val settings = new ScalaSettings

    val initialCtx: Context = new InitialContext(this).fresh
        .withSettings(settings.defaultState)

    val loaders = new SymbolLoaders

    val platform: Platform = new JavaPlatform(this)

    val rootLoader: ClassCompleter = platform.rootLoader

    val definitions = new Definitions()(initialCtx)

  }

  /** Mutable state of a context base, collected into a common class */
  class ContextState {

    // Symbols state
    /** A map from a superclass id to the class that has it */
    private[core] var classOfId = new Array[ClassSymbol](InitialSuperIdsSize)

    /** A map from a superclass to its superclass id */
    private[core] val superIdOfClass = new mutable.HashMap[ClassSymbol, Int]

    /** The last allocate superclass id */
    private[core] var lastSuperId = -1

    /** Allocate and return next free superclass id */
    private[core] def nextSuperId: Int = {
      lastSuperId += 1;
      if (lastSuperId >= classOfId.length) {
        val tmp = new Array[ClassSymbol](classOfId.length * 2)
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
