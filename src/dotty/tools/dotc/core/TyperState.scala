package dotty.tools
package dotc
package core

import Types._
import Flags._
import Contexts._
import util.SimpleMap
import reporting._

class TyperState(val reporter: Reporter = ThrowingReporter) extends DotClass {

  /** The current constraint set */
  def constraint: Constraint = new Constraint(SimpleMap.Empty)

  /** The currently uninstantiated TypeVars */
  def undetVars: Set[TypeVar] = Set()

  /** A map that records for instantiated type vars their instance type.
   *  Used only in a temporary way for contexts that may be retracted
   *  without also retracting the type var.
   */
  def instType: SimpleMap[TypeVar, Type] = SimpleMap.Empty

  def constraint_=(c: Constraint): Unit = {}
  def undetVars_=(vs: Set[TypeVar]): Unit = unsupported("undetVars_=")
  def instType_=(m: SimpleMap[TypeVar, Type]): Unit = unsupported("instType_=")

  def fresh: TyperState = this

  def commit()(implicit ctx: Context): Unit = unsupported("commit")
}

class MutableTyperState(previous: TyperState, reporter: Reporter)
extends TyperState(reporter) {

  def checkConsistent() =
    for (tvar <- undetVars) assert(constraint(tvar.origin) != NoType, tvar)

  private var myConstraint: Constraint = previous.constraint
  private var myUndetVars: Set[TypeVar] = previous.undetVars
  private var myInstType: SimpleMap[TypeVar, Type] = previous.instType

  override def constraint = myConstraint
  override def undetVars = myUndetVars
  override def instType = myInstType

  override def constraint_=(c: Constraint) = myConstraint = c
  override def undetVars_=(vs: Set[TypeVar]) = {
    myUndetVars = vs
    //checkConsistent()
  }
  override def instType_=(m: SimpleMap[TypeVar, Type]): Unit = myInstType = m

  override def fresh: TyperState = new MutableTyperState(this, new StoreReporter)

  /** Commit typer state so that its information is copied into current typer state
   *  In addition (1) the owning state of undetermined or temporarily instantiated
   *  type variables changes from this typer state to the current one. (2) Variables
   *  that were temporarily instantiated in the current typer state are permanently
   *  instantiated instead.
   */
  override def commit()(implicit ctx: Context) = {
    var targetState = ctx.typerState
    targetState.constraint = constraint
    targetState.undetVars = undetVars
    targetState.instType = instType

    def adjustOwningState(tvar: TypeVar) =
      if (tvar.owningState eq this) tvar.owningState = targetState
    undetVars foreach adjustOwningState
    instType foreachKey { case tvar: TypeVar =>
      adjustOwningState(tvar)
      if (tvar.owningState == targetState) {
        tvar.inst = instType(tvar)
        targetState.instType = targetState.instType remove tvar
      }
    }

    reporter.flush()
  }
}
