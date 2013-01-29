package dotty.tools.dotc
package core

import Decorators._
import Periods._
import Names._
import Phases._
import Types._
import Symbols._
import TypeComparers._, Printers._
import collection.mutable
import collection.immutable.BitSet

object Contexts {

  abstract class Context extends Periods with Substituters with TypeOps with Cloneable {
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

    private[this] var _printer: Printer = _
    protected def printer_=(printer: Printer) = _printer = printer
    def printer: Printer = _printer

    private[this] var _owner: Symbol = _
    protected def owner_=(owner: Symbol) = _owner = owner
    def owner: Symbol = _owner

    private[this] var _diagnostics: Option[StringBuilder] = _
    protected def diagnostics_=(diagnostics: Option[StringBuilder]) = _diagnostics = diagnostics
    def diagnostics: Option[StringBuilder] = _diagnostics

    def diagnose(str: => String) =
      for (sb <- diagnostics) {
        sb.setLength(0)
        sb.append(str)
      }


    def phase: Phase = ??? // phase(period.phaseId)
    def enclClass: Context = ???
    def erasedTypes: Boolean = ???
//    def settings: Settings = ???
    def warning(msg: String) = ???

    def fresh: FreshContext = {
      val newctx = super.clone.asInstanceOf[FreshContext]
      newctx.underlying = this
      newctx
    }

  }

  abstract class FreshContext extends Context {
    def withPeriod(period: Period): this.type = { this.period = period; this }
    def withPhase(pid: PhaseId): this.type = withPeriod(Period(runId, pid))
    def withConstraints(constraints: Constraints): this.type = { this.constraints = constraints; this }
    def withPrinter(printer: Printer): this.type = { this.printer = printer; this }
    def withOwner(owner: Symbol): this.type = { this.owner = owner; this }
    def withDiagnostics(diagnostics: Option[StringBuilder]): this.type = { this.diagnostics = diagnostics; this }
  }

  private class InitialContext(val base: ContextBase) extends FreshContext {
    underlying = NoContext
    period = Nowhere
    constraints = Map()
    printer = new StdPrinter
    owner = NoSymbol
  }

  object NoContext extends Context {
    val base = unsupported("base")
  }

  class ContextBase extends Transformers.TransformerBase
                       with Printers.PrinterBase {

    val initialCtx: Context = new InitialContext(this)

    val names: NameTable = new NameTable

    lazy val definitions = new Definitions()(initialCtx)

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
