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
import annotation.elidable

class TyperState(val reporter: Reporter) extends DotClass with Showable {

  /** The current constraint set */
  def constraint: Constraint = new Constraint(SimpleMap.Empty)

  /** The currently uninstantiated TypeVars */
  def undetVars: Set[TypeVar] = collection.immutable.ListSet()

  /** A map that records for instantiated type vars their instance type.
   *  Used only in a temporary way for contexts that may be retracted
   *  without also retracting the type var as a whole.
   */
  def instType: SimpleMap[TypeVar, Type] = SimpleMap.Empty

  def constraint_=(c: Constraint): Unit = {}
  def undetVars_=(vs: Set[TypeVar]): Unit = unsupported("undetVars_=")
  def instType_=(m: SimpleMap[TypeVar, Type]): Unit = unsupported("instType_=")

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
  private var myUndetVars: Set[TypeVar] = previous.undetVars
  private var myInstType: SimpleMap[TypeVar, Type] = previous.instType
  private var checkingEnabled: Boolean = true

  override def constraint = myConstraint
  override def undetVars = myUndetVars
  override def instType = myInstType

  override def constraint_=(c: Constraint) = {
    myConstraint = c
    checkConsistent()
  }
  override def undetVars_=(vs: Set[TypeVar]) = {
    myUndetVars = vs
    checkConsistent()
  }
  override def instType_=(m: SimpleMap[TypeVar, Type]): Unit = myInstType = m

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
    targetState.undetVars = undetVars
    targetState.instType = instType
    targetState.enableChecking(prev)

    def adjustOwningState(tvar: TypeVar) =
      if (tvar.owningState eq this) tvar.owningState = targetState
    undetVars foreach adjustOwningState
    instType foreachKey { tvar =>
      adjustOwningState(tvar)
      if (tvar.owningState == targetState) {
        tvar.inst = instType(tvar)
        targetState.instType = targetState.instType remove tvar
      }
    }
    targetState.checkConsistent // !!! DEBUG

    reporter.flush()
  }

  @elidable(elidable.FINER)
  def checkConsistent(show: Showable => String = MutableTyperState.toStr): Unit = if (checkingEnabled) {
    def err(msg: String, what: Showable) = s"$msg: ${show(what)}\n${show(this)}"
    for (tvar <- undetVars)
      assert(constraint contains tvar.origin, err("unconstrained type var", tvar.origin))
    if (isCommittable) {
      val undetParams = undetVars map (_.origin)
      for (param <- constraint.domainParams)
        assert(undetParams contains param, err("junk constraint on", param))
      instType.foreachKey { tvar =>
        assert(!(undetVars contains tvar), err("duplicate undetVar and instType", tvar))
      }
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
    val undetVarsText =
      " undetVars: " ~
      Text(undetVars map (_.toText(printer)), ", ") ~ "."
    val constrainedText =
      " constrained types: " ~ constraint.constrainedTypesText(printer) ~ "."
    val constraintText =
      " constraint: " ~ constraint.constraintText(3, printer)
    val instTypeText =
      " instType: " ~
      Text(instType.map2((k, v) => s"${k.toText(printer)} -> ${v.toText(printer)}"), ", ") ~ "."
    Text.lines(List(header, undetVarsText, constrainedText, constraintText, instTypeText))
  }
}

object MutableTyperState {
  private def toStr(x: Any) = x.toString
}
