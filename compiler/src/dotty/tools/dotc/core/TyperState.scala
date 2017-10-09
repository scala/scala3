package dotty.tools
package dotc
package core

import Types._
import Flags._
import Contexts._
import util.{SimpleIdentityMap, DotClass}
import reporting._
import printing.{Showable, Printer}
import printing.Texts._
import config.Config
import collection.mutable
import java.lang.ref.WeakReference

class TyperState(previous: TyperState /* | Null */) extends DotClass with Showable {

  private var myReporter =
    if (previous == null) new ConsoleReporter() else previous.reporter

  def reporter: Reporter = myReporter

  /** A fresh type state with the same constraint as this one and the given reporter */
  def setReporter(reporter: Reporter): this.type = { myReporter = reporter; this }

  private var myConstraint: Constraint =
    if (previous == null) new OrderingConstraint(SimpleIdentityMap.Empty, SimpleIdentityMap.Empty, SimpleIdentityMap.Empty)
    else previous.constraint

  def constraint = myConstraint
  def constraint_=(c: Constraint)(implicit ctx: Context) = {
    if (Config.debugCheckConstraintsClosed && isGlobalCommittable) c.checkClosed()
    myConstraint = c
  }

  private val previousConstraint =
    if (previous == null) constraint else previous.constraint

  private var myEphemeral: Boolean =
    if (previous == null) false else previous.ephemeral

  /** The ephemeral flag is set as a side effect if an operation accesses
   *  the underlying type of a type variable. The reason we need this flag is
   *  that any such operation is not referentially transparent; it might logically change
   *  its value at the moment the type variable is instantiated. Caching code needs to
   *  check the ephemeral flag; If the flag is set during an operation, the result
   *  of that operation should not be cached.
   */
  def ephemeral = myEphemeral
  def ephemeral_=(x: Boolean): Unit = { myEphemeral = x }

  private var myIsCommittable = true

  def isCommittable = myIsCommittable

  def setCommittable(committable: Boolean): this.type = { this.myIsCommittable = committable; this }

  def isGlobalCommittable: Boolean =
    isCommittable && (previous == null || previous.isGlobalCommittable)

  private var isCommitted = false

  /** A fresh typer state with the same constraint as this one. */
  def fresh(): TyperState =
    new TyperState(this).setReporter(new StoreReporter(reporter)).setCommittable(isCommittable)

  /** The uninstantiated variables */
  def uninstVars = constraint.uninstVars

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

  /** The closest ancestor of this typer state (including possibly this typer state itself)
   *  which is not yet committed, or which does not have a parent.
   */
  def uncommittedAncestor: TyperState =
    if (isCommitted) previous.uncommittedAncestor else this

  private var testReporter: StoreReporter = null

  /** Test using `op`, restoring typerState to previous state afterwards */
  def test(op: => Boolean): Boolean = {
    val savedReporter = myReporter
    val savedConstraint = myConstraint
    val savedCommittable = myIsCommittable
    val savedCommitted = isCommitted
    myIsCommittable = false
    myReporter = {
      if (testReporter == null) {
        testReporter = new StoreReporter(reporter)
      } else {
        testReporter.reset()
      }
      testReporter
    }
    try op
    finally {
      myReporter = savedReporter
      myConstraint = savedConstraint
      myIsCommittable = savedCommittable
      isCommitted = savedCommitted
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
  def commit()(implicit ctx: Context) = {
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

  /** Make type variable instances permanent by assigning to `inst` field if
   *  type variable instantiation cannot be retracted anymore. Then, remove
   *  no-longer needed constraint entries.
   */
  def gc()(implicit ctx: Context): Unit = {
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

  def hashesStr: String =
    if (previous == null) "" else hashCode.toString + " -> " + previous.hashesStr
}
