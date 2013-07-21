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
  def undetVars: List[TypeVar] = Nil

  def constraint_=(c: Constraint): Unit = {}
  def undetVars_=(vs: List[TypeVar]): Unit = unsupported("undetVars_=")

  def fresh: TyperState = this

  def commit()(implicit ctx: Context): Unit = unsupported("commit")
}

class MutableTyperState(previous: TyperState, reporter: Reporter)
extends TyperState(reporter) {

  private var myConstraint: Constraint = previous.constraint
  private var myUndetVars: List[TypeVar] = previous.undetVars

  override def constraint = myConstraint
  override def undetVars = myUndetVars

  override def constraint_=(c: Constraint) = myConstraint = c
  override def undetVars_=(vs: List[TypeVar]) = myUndetVars = vs

  override def fresh: TyperState = new MutableTyperState(this, new StoreReporter)

  override def commit()(implicit ctx: Context) = {
    ctx.typerState.constraint = constraint
    ctx.typerState.undetVars = undetVars
    reporter.flush()
  }
}
