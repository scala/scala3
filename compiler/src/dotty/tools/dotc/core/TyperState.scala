package dotty.tools
package dotc
package core

import Types._
import Flags._
import Contexts._
import util.{SimpleMap, DotClass}
import reporting._
import printing.{Showable, Printer}
import printing.Texts._
import config.Config
import collection.mutable
import java.lang.ref.WeakReference

class TyperState(r: Reporter) extends DotClass with Showable {

  /** The current reporter */
  def reporter = r

  /** The current constraint set */
  def constraint: Constraint =
    new OrderingConstraint(SimpleMap.Empty, SimpleMap.Empty, SimpleMap.Empty)
  def constraint_=(c: Constraint)(implicit ctx: Context): Unit = {}

  /** The uninstantiated variables */
  def uninstVars = constraint.uninstVars

  /** The ephemeral flag is set as a side effect if an operation accesses
   *  the underlying type of a type variable. The reason we need this flag is
   *  that any such operation is not referentially transparent; it might logically change
   *  its value at the moment the type variable is instantiated. Caching code needs to
   *  check the ephemeral flag; If the flag is set during an operation, the result
   *  of that operation should not be cached.
   */
  def ephemeral: Boolean = false
  def ephemeral_=(x: Boolean): Unit = ()

  /** Gives for each instantiated type var that does not yet have its `inst` field
   *  set, the instance value stored in the constraint. Storing instances in constraints
   *  is done only in a temporary way for contexts that may be retracted
   *  without also retracting the type var as a whole.
   */
  def instType(tvar: TypeVar)(implicit ctx: Context): Type = constraint.entry(tvar.origin) match {
    case _: TypeBounds => NoType
    case tp: TypeParamRef =>
      var tvar1 = constraint.typeVarOfParam(tp)
      if (tvar1.exists) tvar1 else tp
    case tp => tp
  }

  /** A fresh typer state with the same constraint as this one.
   *  @param isCommittable  The constraint can be committed to an enclosing context.
   */
  def fresh(isCommittable: Boolean): TyperState = this

  /** A fresh type state with the same constraint as this one and the given reporter */
  def withReporter(reporter: Reporter) = new TyperState(reporter)

  /** Commit state so that it gets propagated to enclosing context */
  def commit()(implicit ctx: Context): Unit = unsupported("commit")

  /** The closest ancestor of this typer state (including possibly this typer state itself)
   *  which is not yet committed, or which does not have a parent.
   */
  def uncommittedAncestor: TyperState = this

  /** Make type variable instances permanent by assigning to `inst` field if
   *  type variable instantiation cannot be retracted anymore. Then, remove
   *  no-longer needed constraint entries.
   */
  def gc()(implicit ctx: Context): Unit = ()

  /** Is it allowed to commit this state? */
  def isCommittable: Boolean = false
  def isCommittable_=(b: Boolean) = ()

  /** Can this state be transitively committed until the top-level? */
  def isGlobalCommittable: Boolean = false

  /** Test using `op`, restoring typerState to previous state afterwards */
  def test(op: => Boolean): Boolean = op

  override def toText(printer: Printer): Text = "ImmutableTyperState"

  /** A string showing the hashes of all nested mutable typerstates */
  def hashesStr: String = ""
}

class MutableTyperState(previous: TyperState, r: Reporter, override var isCommittable: Boolean)
extends TyperState(r) {

  private var myReporter = r

  override def reporter = myReporter

  private val previousConstraint = previous.constraint
  private var myConstraint: Constraint = previousConstraint

  override def constraint = myConstraint
  override def constraint_=(c: Constraint)(implicit ctx: Context) = {
    if (Config.debugCheckConstraintsClosed && isGlobalCommittable) c.checkClosed()
    myConstraint = c
  }

  private var myEphemeral: Boolean = previous.ephemeral

  override def ephemeral = myEphemeral
  override def ephemeral_=(x: Boolean): Unit = { myEphemeral = x }

  override def fresh(isCommittable: Boolean): TyperState =
    new MutableTyperState(this, new StoreReporter(reporter), isCommittable)

  override def withReporter(reporter: Reporter) =
    new MutableTyperState(this, reporter, isCommittable)

  override val isGlobalCommittable =
    isCommittable &&
    (!previous.isInstanceOf[MutableTyperState] || previous.isGlobalCommittable)

  private var isCommitted = false

  override def uncommittedAncestor: TyperState =
    if (isCommitted) previous.uncommittedAncestor else this

  override def test(op: => Boolean): Boolean = {
    val savedReporter = myReporter
    val savedConstraint = myConstraint
    val savedCommitted = isCommitted
    val savedCommittable = isCommittable
    isCommittable = false
    try op
    finally {
      myReporter = savedReporter
      myConstraint = savedConstraint
      isCommitted = savedCommitted
      isCommittable = savedCommittable
    }
  }

  /** Commit typer state so that its information is copied into current typer state
   *  In addition (1) the owning state of undetermined or temporarily instantiated
   *  type variables changes from this typer state to the current one. (2) Variables
   *  that were temporarily instantiated in the current typer state are permanently
   *  instantiated instead.
   *
   *  A note on merging: An interesting test case is isApplicableSafe.scala. It turns out that this
   *  requires a context merge using the new `&' operator. Sequence of actions:
   *  1) Typecheck argument in typerstate 1.
   *  2) Cache argument.
   *  3) Evolve same typer state (to typecheck other arguments, say)
   *     leading to a different constraint.
   *  4) Take typechecked argument in same state.
   *
   * It turns out that the merge is needed not just for
   * isApplicableSafe but also for (e.g. erased-lubs.scala) as well as
   * many parts of dotty itself.
   */
  override def commit()(implicit ctx: Context) = {
    val targetState = ctx.typerState
    assert(isCommittable)
    targetState.constraint =
      if (targetState.constraint eq previousConstraint) constraint
      else targetState.constraint & constraint
    constraint foreachTypeVar { tvar =>
      if (tvar.owningState.get eq this) tvar.owningState = new WeakReference(targetState)
    }
    targetState.ephemeral |= ephemeral
    targetState.gc()
    reporter.flush()
    isCommitted = true
  }

  override def gc()(implicit ctx: Context): Unit = {
    val toCollect = new mutable.ListBuffer[TypeLambda]
    constraint foreachTypeVar { tvar =>
      if (!tvar.inst.exists) {
        val inst = instType(tvar)
        if (inst.exists && (tvar.owningState.get eq this)) {
          tvar.inst = inst
          val lam = tvar.origin.binder
          if (constraint.isRemovable(lam)) toCollect += lam
        }
      }
    }
    for (poly <- toCollect)
      constraint = constraint.remove(poly)
  }

  override def toText(printer: Printer): Text = constraint.toText(printer)

  override def hashesStr: String = hashCode.toString + " -> " + previous.hashesStr

}
