package dotty.tools
package dotc
package core

import Types._
import Contexts._
import util.{SimpleIdentityMap, SimpleIdentitySet}
import reporting._
import config.Config
import config.Printers.constr
import collection.mutable
import java.lang.ref.WeakReference
import util.Stats
import Decorators._

import scala.annotation.internal.sharable

object TyperState {
  @sharable private var nextId: Int = 0
  def initialState() =
    TyperState()
      .init(null, OrderingConstraint.empty)
      .setReporter(new ConsoleReporter())
      .setCommittable(true)
}

class TyperState() {

  private var myId: Int = _
  def id: Int = myId

  private var previous: TyperState /* | Null */ = _

  private var myReporter: Reporter = _

  def reporter: Reporter = myReporter

  /** A fresh type state with the same constraint as this one and the given reporter */
  def setReporter(reporter: Reporter): this.type = { myReporter = reporter; this }

  private var myConstraint: Constraint = _

  def constraint: Constraint = myConstraint
  def constraint_=(c: Constraint)(using Context): Unit = {
    if (Config.debugCheckConstraintsClosed && isGlobalCommittable) c.checkClosed()
    myConstraint = c
  }

  private var previousConstraint: Constraint = _

  private var myIsCommittable: Boolean = _

  def isCommittable: Boolean = myIsCommittable

  def setCommittable(committable: Boolean): this.type =
    this.myIsCommittable = committable
    this

  def isGlobalCommittable: Boolean =
    isCommittable && (previous == null || previous.isGlobalCommittable)

  private var isCommitted: Boolean = _

  /** The set of uninstantiated type variables which have this state as their owning state */
  private var myOwnedVars: TypeVars = _
  def ownedVars: TypeVars = myOwnedVars
  def ownedVars_=(vs: TypeVars): Unit = myOwnedVars = vs

  /** Initializes all fields except reporter, isCommittable, which need to be
   *  set separately.
   */
  private[core] def init(previous: TyperState /* | Null */, constraint: Constraint): this.type =
    this.myId = TyperState.nextId
    TyperState.nextId += 1
    this.previous = previous
    this.myConstraint = constraint
    this.previousConstraint = constraint
    this.myOwnedVars = SimpleIdentitySet.empty
    this.isCommitted = false
    this

  /** A fresh typer state with the same constraint as this one. */
  def fresh(reporter: Reporter = StoreReporter(this.reporter)): TyperState =
    util.Stats.record("TyperState.fresh")
    TyperState().init(this, this.constraint)
      .setReporter(reporter)
      .setCommittable(this.isCommittable)

  /** The uninstantiated variables */
  def uninstVars: collection.Seq[TypeVar] = constraint.uninstVars

  /** The closest ancestor of this typer state (including possibly this typer state itself)
   *  which is not yet committed, or which does not have a parent.
   */
  def uncommittedAncestor: TyperState =
    if (isCommitted) previous.uncommittedAncestor else this

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
  def commit()(using Context): Unit = {
    Stats.record("typerState.commit")
    val targetState = ctx.typerState
    if (constraint ne targetState.constraint)
      constr.println(i"committing $this to $targetState, fromConstr = $constraint, toConstr = ${targetState.constraint}")
    assert(isCommittable)
    if (targetState.constraint eq previousConstraint) targetState.constraint = constraint
    else targetState.mergeConstraintWith(this)
    constraint foreachTypeVar { tvar =>
      if (tvar.owningState.get eq this) tvar.owningState = new WeakReference(targetState)
    }
    targetState.ownedVars ++= ownedVars
    targetState.gc()
    reporter.flush()
    isCommitted = true
  }

  def mergeConstraintWith(that: TyperState)(using Context): Unit =
    constraint = constraint & (that.constraint, otherHasErrors = that.reporter.errorsReported)

  /** Make type variable instances permanent by assigning to `inst` field if
   *  type variable instantiation cannot be retracted anymore. Then, remove
   *  no-longer needed constraint entries.
   */
  def gc()(using Context): Unit = {
    Stats.record("typerState.gc")
    val toCollect = new mutable.ListBuffer[TypeLambda]
    constraint foreachTypeVar { tvar =>
      if (!tvar.inst.exists) {
        val inst = constraint.instType(tvar)
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

  override def toString: String = {
    def ids(state: TyperState): List[String] =
      s"${state.id}${if (state.isCommittable) "" else "X"}" ::
        (if (state.previous == null) Nil else ids(state.previous))
    s"TS[${ids(this).mkString(", ")}]"
  }

  def stateChainStr: String = s"$this${if (previous == null) "" else previous.stateChainStr}"
}
