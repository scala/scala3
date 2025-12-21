package dotty.tools
package dotc
package core

import Types.*
import Contexts.*
import util.SimpleIdentitySet
import reporting.*
import config.Config
import config.Printers.constr
import collection.mutable
import java.lang.ref.WeakReference
import util.{Stats, SimpleIdentityMap}
import Decorators.*

import scala.annotation.internal.sharable
import scala.compiletime.uninitialized

object TyperState {
  @sharable private var nextId: Int = 0
  def initialState() =
    TyperState()
      .init(null, OrderingConstraint.empty)
      .setReporter(new ConsoleReporter())
      .setCommittable(true)

  type LevelMap = SimpleIdentityMap[TypeVar, Integer]

  opaque type Snapshot = (Constraint, TypeVars, LevelMap)

  class BadTyperStateAssertion(msg: String) extends AssertionError(msg)

  extension (ts: TyperState)
    def snapshot()(using Context): Snapshot =
      (ts.constraint, ts.ownedVars, ts.upLevels)

    def resetTo(state: Snapshot)(using Context): Unit =
      val (constraint, ownedVars, upLevels) = state
      for tv <- ownedVars do
        if !ts.ownedVars.contains(tv) then // tv has been instantiated
          tv.resetInst(ts)
      ts.constraint = constraint
      ts.ownedVars = ownedVars
      ts.upLevels = upLevels
}

class TyperState() {
  import TyperState.{LevelMap, BadTyperStateAssertion}

  private var myId: Int = uninitialized
  def id: Int = myId

  private var previous: TyperState | Null = uninitialized

  private var myReporter: Reporter = uninitialized

  def reporter: Reporter = myReporter

  /** A fresh type state with the same constraint as this one and the given reporter */
  def setReporter(reporter: Reporter): this.type = { myReporter = reporter; this }

  private var myConstraint: Constraint = uninitialized

  def constraint: Constraint = myConstraint
  def constraint_=(c: Constraint)(using Context): Unit = {
    if (Config.debugCheckConstraintsClosed && isGlobalCommittable) c.checkClosed()
    myConstraint = c
    if Config.checkConsistentVars && !ctx.reporter.errorsReported then
      c.checkConsistentVars()
  }

  private var previousConstraint: Constraint = uninitialized

  private var myIsCommittable: Boolean = uninitialized

  def isCommittable: Boolean = myIsCommittable

  def setCommittable(committable: Boolean): this.type =
    this.myIsCommittable = committable
    this

  def isGlobalCommittable: Boolean =
    isCommittable && (previous == null || previous.uncheckedNN.isGlobalCommittable)

  private var isCommitted: Boolean = uninitialized

  /** The set of uninstantiated type variables which have this state as their owning state.
   *
   *  Invariant:
   *   if `tstate.isCommittable` then
   *     `tstate.ownedVars.contains(tvar)` iff `tvar.owningState.get eq tstate`
   */
  private var myOwnedVars: TypeVars = uninitialized
  def ownedVars: TypeVars = myOwnedVars
  def ownedVars_=(vs: TypeVars): Unit = myOwnedVars = vs

  private var upLevels: LevelMap = uninitialized

  /** Initializes all fields except reporter, isCommittable, which need to be
   *  set separately.
   */
  private[core] def init(previous: TyperState | Null, constraint: Constraint): this.type =
    this.myId = TyperState.nextId
    TyperState.nextId += 1
    this.previous = previous
    this.myConstraint = constraint
    this.previousConstraint = constraint
    this.myOwnedVars = SimpleIdentitySet.empty
    this.upLevels = SimpleIdentityMap.empty
    this.isCommitted = false
    this

  /** A fresh typer state with the same constraint as this one. */
  def fresh(reporter: Reporter = StoreReporter(this.reporter, fromTyperState = true),
      committable: Boolean = this.isCommittable): TyperState =
    util.Stats.record("TyperState.fresh")
    val ts = TyperState().init(this, this.constraint)
      .setReporter(reporter)
      .setCommittable(committable)
    ts.upLevels = upLevels
    ts

  /** The uninstantiated variables */
  def uninstVars: collection.Seq[TypeVar] = constraint.uninstVars

  /** The nestingLevel of `tv` in this typer state */
  def nestingLevel(tv: TypeVar): Int =
    val own = upLevels(tv)
    if own == null then tv.initNestingLevel else own.intValue()

  /** Set the nestingLevel of `tv` in this typer state
   *  @pre this level must be smaller than `tv.initNestingLevel`
  */
  def setNestingLevel(tv: TypeVar, level: Int) =
    assert(level < tv.initNestingLevel)
    upLevels = upLevels.updated(tv, level)

  /** The closest ancestor of this typer state (including possibly this typer state itself)
   *  which is not yet committed, or which does not have a parent.
   */
  def uncommittedAncestor: TyperState =
    if (isCommitted && previous != null) previous.uncheckedNN.uncommittedAncestor else this

  /** Commit `this` typer state by copying information into the current typer state,
   *  where "current" means contextual, so meaning `ctx.typerState`.
   *  In addition (1) the owning state of undetermined or temporarily instantiated
   *  type variables changes from this typer state to the current one. (2) Variables
   *  that were temporarily instantiated in the current typer state are permanently
   *  instantiated instead.
   *
   *  A note on merging: An interesting test case is isApplicableSafe.scala. It turns out that this
   *  requires a context merge using the new `&` operator. Sequence of actions:
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
    assert(isCommittable, s"$this is not committable")
    assert(!isCommitted, s"$this is already committed")
    val targetState = ctx.typerState

    val nothingToCommit = (constraint eq targetState.constraint) && !reporter.hasUnreportedMessages
    assert(!targetState.isCommitted || nothingToCommit ||
       // Committing into an already committed TyperState usually doesn't make
       // sense since it means the constraints and messages we're committing won't be propagated
       // further, but it can happen if the targetState gets captured in a reported
       // Message, because forcing that Message might involve creating and
       // committing new TyperStates into the captured one after it's been committed.
        targetState.reporter.hasErrors || targetState.reporter.hasWarnings,
      s"Attempt to commit $this into already committed $targetState")

    reporter.flush()
    setCommittable(false)

    if constraint ne targetState.constraint then
      Stats.record("typerState.commit.new constraint")
      constr.println(i"committing $this to $targetState, fromConstr = $constraint, toConstr = ${targetState.constraint}")
      if targetState.constraint eq previousConstraint then
        targetState.constraint = constraint
        if !ownedVars.isEmpty then ownedVars.foreach(targetState.includeVar)
      else
        targetState.mergeConstraintWith(this)

    upLevels.foreachBinding { (tv, level) =>
      if level < targetState.nestingLevel(tv) then
        targetState.setNestingLevel(tv, level)
    }

    targetState.gc()
    isCommitted = true
    ownedVars = SimpleIdentitySet.empty
  }

  /** Ensure that this constraint does not associate different TypeVars for the
   *  same type lambda than the `other` constraint. Do this by renaming type lambdas
   *  in this constraint and its predecessors where necessary.
   */
  def ensureNotConflicting(other: Constraint)(using Context): Unit =
    val conflicting = constraint.domainLambdas.filter(constraint.hasConflictingTypeVarsFor(_, other))
    for tl <- conflicting do
      val tl1 = constraint.ensureFresh(tl)
      for case (tvar: TypeVar, pref1) <- tl.paramRefs.map(constraint.typeVarOfParam).lazyZip(tl1.paramRefs) do
        tvar.setOrigin(pref1)
      var ts = this
      while ts.constraint.domainLambdas.contains(tl) do
        ts.constraint = ts.constraint.subst(tl, tl1)
        ts = ts.previous.nn

  /** Integrate the constraints from `that` into this TyperState.
   *
   *  @pre If `this` and `that` are committable, `that` must not contain any type variable which
   *       does not exist in `this` (in other words, all its type variables must
   *       be owned by a common parent of `this` and `that`).
   */
  def mergeConstraintWith(that: TyperState)(using Context): this.type =
    if this eq that then return this

    that.ensureNotConflicting(constraint)

    val comparingCtx = ctx.withTyperState(this)

    inContext(comparingCtx)(comparing(typeComparer =>
      val other = that.constraint
      val res = other.domainLambdas.forall(tl =>
        // Integrate the type lambdas from `other`
        constraint.contains(tl) || other.isRemovable(tl) || {
          val tvars = tl.paramRefs.map(other.typeVarOfParam(_)).collect { case tv: TypeVar => tv }
          if this.isCommittable then
            tvars.foreach(tvar =>
              if !tvar.isPermanentlyInstantiated && !isOwnedAnywhere(this, tvar) then includeVar(tvar))
          typeComparer.addToConstraint(tl, tvars)
        }) &&
        // Integrate the additional constraints on type variables from `other`
        // and merge hardness markers
        constraint.uninstVars.forall(tv =>
          if other.isHard(tv) then constraint = constraint.withHard(tv)
          val p = tv.origin
          val otherLos = other.lower(p)
          val otherHis = other.upper(p)
          val otherEntry = other.entry(p)
          (  (otherLos eq constraint.lower(p)) || otherLos.forall(_ <:< p)) &&
          (  (otherHis eq constraint.upper(p)) || otherHis.forall(p <:< _)) &&
          ((otherEntry eq constraint.entry(p)) || otherEntry.match
            case NoType =>
              true
            case tp: TypeBounds =>
              tp.contains(tv)
            case tp =>
              tv =:= tp
          )
        )
      assert(res || ctx.reporter.errorsReported, i"cannot merge $constraint with $other.")
    ))

    for tl <- constraint.domainLambdas do
      if constraint.isRemovable(tl) then constraint = constraint.remove(tl)
    this
  end mergeConstraintWith

  /** Take ownership of `tvar`.
   *
   *  @pre `tvar` is not owned by a committable TyperState. This ensures
   *       each tvar can only be instantiated by one TyperState.
   */
  private def includeVar(tvar: TypeVar)(using Context): Unit =
    val oldState = tvar.owningState.nn.get

    if oldState != null && oldState.isCommittable then
      throw BadTyperStateAssertion(
        i"$this attempted to take ownership of $tvar which is already owned by committable $oldState")
    tvar.owningState = new WeakReference(this)
    ownedVars += tvar

  private def isOwnedAnywhere(ts: TyperState, tvar: TypeVar): Boolean =
    ts.ownedVars.contains(tvar) || ts.previous != null && isOwnedAnywhere(ts.previous.uncheckedNN, tvar)

  /** Make type variable instances permanent by assigning to `inst` field if
   *  type variable instantiation cannot be retracted anymore. Then, remove
   *  no-longer needed constraint entries.
   */
  def gc()(using Context): Unit =
    if !ownedVars.isEmpty then
      Stats.record("typerState.gc")
      val toCollect = new mutable.ListBuffer[TypeLambda]
      for tvar <- ownedVars do
        val tvarState = tvar.owningState.nn.get
        assert(tvarState eq this, s"Inconsistent state in $this: it owns $tvar whose owningState is ${tvarState}")
        assert(!tvar.isPermanentlyInstantiated, s"Inconsistent state in $this: it owns $tvar which is already instantiated")
        val inst = constraint.instType(tvar)
        if inst.exists then
          tvar.setPermanentInst(inst)
          val tl = tvar.origin.binder
          if constraint.isRemovable(tl) then toCollect += tl
      for tl <- toCollect do
        constraint = constraint.remove(tl)

  override def toString: String = {
    def ids(state: TyperState): List[String] =
      s"${state.id}${if (state.isCommittable) "" else "X"}" ::
        (if (state.previous == null) Nil else ids(state.previous.uncheckedNN))
    s"TS[${ids(this).mkString(", ")}]"
  }

  def stateChainStr: String = s"$this${if (previous == null) "" else previous.uncheckedNN.stateChainStr}"
}
