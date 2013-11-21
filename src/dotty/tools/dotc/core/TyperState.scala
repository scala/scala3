package dotty.tools
package dotc
package core

import Types._
import Flags._
import Contexts._
import util.SimpleMap
import reporting._
import printing.{Showable, Printer}
import printing.Texts._
import collection.mutable
import annotation.elidable

class TyperState(val reporter: Reporter) extends DotClass with Showable {

  /** The current constraint set */
  def constraint: Constraint = new Constraint(SimpleMap.Empty)

  def uninstVars = constraint.uninstVars

  /** A map that records for instantiated type vars their instance type.
   *  Used only in a temporary way for contexts that may be retracted
   *  without also retracting the type var as a whole.
   */
  def instType(tvar: TypeVar): Type = constraint.at(tvar.origin) match {
    case _: TypeBounds => NoType
    case tp => tp
  }

  def constraint_=(c: Constraint): Unit = {}

  def fresh(isCommittable: Boolean): TyperState = this

  def commit()(implicit ctx: Context): Unit = unsupported("commit")
  def isCommittable: Boolean = false

  @elidable(elidable.FINER)
  def checkConsistent(implicit ctx: Context) = ()

  @elidable(elidable.FINER)
  def enableChecking(b: Boolean): Boolean = true

  def withCheckingDisabled[T](op: => T)(implicit ctx: Context): T = op

  override def toText(printer: Printer): Text = "ImmutableTyperState"
}

class MutableTyperState(previous: TyperState, reporter: Reporter, override val isCommittable: Boolean)
extends TyperState(reporter) {

  private var myConstraint: Constraint = previous.constraint
  private var checkingEnabled: Boolean = true

  override def constraint = myConstraint
  override def constraint_=(c: Constraint) = {
    myConstraint = c
    checkConsistent()
  }

  override def fresh(isCommittable: Boolean): TyperState =
    new MutableTyperState(this, new StoreReporter, isCommittable)

  /** Commit typer state so that its information is copied into current typer state
   *  In addition (1) the owning state of undetermined or temporarily instantiated
   *  type variables changes from this typer state to the current one. (2) Variables
   *  that were temporarily instantiated in the current typer state are permanently
   *  instantiated instead.
   */
  override def commit()(implicit ctx: Context) = {
    checkConsistent
    val targetState = ctx.typerState
    val prev = targetState.enableChecking(false)
    targetState.constraint = constraint
    targetState.enableChecking(prev)

    val toCollect = new mutable.ListBuffer[PolyType]
    constraint foreachTypeVar { tvar =>
      if (tvar.owningState eq this)
        tvar.owningState = targetState
      if (!tvar.inst.exists) {
        val inst = instType(tvar)
        if (inst.exists && (tvar.owningState eq targetState)) {
          tvar.inst = inst
          val poly = tvar.origin.binder
          if (targetState.constraint.isRemovable(poly)) toCollect += poly
        }
      }
    }
    for (poly <- toCollect)
      targetState.constraint = targetState.constraint.remove(poly)

    targetState.checkConsistent // !!! DEBUG

    reporter.flush()
  }

  @elidable(elidable.FINER)
  def checkConsistent(show: Showable => String = MutableTyperState.toStr): Unit = if (checkingEnabled) {
    def err(msg: String, what: Showable) = s"$msg: ${show(what)}\n${show(this)}"
    for (tvar <- uninstVars)
      assert(constraint contains tvar.origin, err("unconstrained type var", tvar.origin))
    if (isCommittable) {
      val undetParams = uninstVars map (_.origin)
      for (param <- constraint.domainParams)
        assert(undetParams contains param, err("junk constraint on", param))
    }
  }

  @elidable(elidable.FINER)
  override def checkConsistent(implicit ctx: Context): Unit = checkConsistent(_.show)

  @elidable(elidable.FINER)
  override def enableChecking(b: Boolean) = {
    val prev = checkingEnabled
    checkingEnabled = b
    prev
  }

  override def withCheckingDisabled[T](op: => T)(implicit ctx: Context): T = {
    val prev = enableChecking(false)
    var thrown = false
    try op
    catch {
      case ex: Throwable =>
        thrown = true
        throw ex
    }
    finally {
      enableChecking(prev)
      if (!thrown) checkConsistent
    }
  }

  override def toText(printer: Printer): Text = {
    val header: Text = "Typer state:"
    val uninstVarsText =
      " uninstVars: " ~
      Text(uninstVars map (_.toText(printer)), ", ") ~ "."
    val constrainedText =
      " constrained types: " ~ constraint.constrainedTypesText(printer) ~ "."
    val constraintText =
      " constraint: " ~ constraint.constraintText(3, printer)
    Text.lines(List(header, uninstVarsText, constrainedText, constraintText))
  }
}

object MutableTyperState {
  private def toStr(x: Any) = x.toString
}
