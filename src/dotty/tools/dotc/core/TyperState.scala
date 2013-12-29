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

class TyperState(val reporter: Reporter) extends DotClass with Showable {

  /** The current constraint set */
  def constraint: Constraint = new Constraint(SimpleMap.Empty)
  def constraint_=(c: Constraint): Unit = {}

  /** The uninstantiated variables */
  def uninstVars = constraint.uninstVars

  /** Gives for each instantiated type var that does not yet have its `inst` field
   *  set, the instance value stored in the constraint. Storing instances in constraints
   *  is done only in a temporary way for contexts that may be retracted
   *  without also retracting the type var as a whole.
   */
  def instType(tvar: TypeVar): Type = constraint.at(tvar.origin) match {
    case _: TypeBounds => NoType
    case tp => tp
  }

  /** A fresh typer state with the same constraint as this one.
   *  @param isCommittable  The constraint can be committed to an exclosing context.
   */
  def fresh(isCommittable: Boolean): TyperState = this

  /** Commit state so that it gets propagated to enclosing context */
  def commit()(implicit ctx: Context): Unit = unsupported("commit")

  /** Is it allowed to commit this state? */
  def isCommittable: Boolean = false

  /** Can this state be transitively committed until the top-level? */
  def isGlobalCommittable: Boolean = false

  override def toText(printer: Printer): Text = "ImmutableTyperState"
}

class MutableTyperState(previous: TyperState, reporter: Reporter, override val isCommittable: Boolean)
extends TyperState(reporter) {

  private var myConstraint: Constraint = previous.constraint

  override def constraint = myConstraint
  override def constraint_=(c: Constraint) = myConstraint = c

  override def fresh(isCommittable: Boolean): TyperState =
    new MutableTyperState(this, new StoreReporter, isCommittable)

  override val isGlobalCommittable =
    isCommittable &&
    (!previous.isInstanceOf[MutableTyperState] || previous.isGlobalCommittable)

  /** Commit typer state so that its information is copied into current typer state
   *  In addition (1) the owning state of undetermined or temporarily instantiated
   *  type variables changes from this typer state to the current one. (2) Variables
   *  that were temporarily instantiated in the current typer state are permanently
   *  instantiated instead.
   */
  override def commit()(implicit ctx: Context) = {
    val targetState = ctx.typerState
    assert(isCommittable)
    targetState.constraint = constraint

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

    reporter.flush()
  }

  override def toText(printer: Printer): Text = constraint.toText(printer)
}
