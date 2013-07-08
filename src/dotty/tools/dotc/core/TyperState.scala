package dotty.tools
package dotc
package core

import Types._
import Flags._
import Contexts._
import util.SimpleMap

class TyperState extends DotClass {

  /** The current constraint set */
  def constraint: Constraint = new Constraint(SimpleMap.Empty)

  /** The currently uninstantiated TypeVars */
  def undetVars: List[TypeVar] = Nil

  /** The currently outstanding errors, warnings, or infos */
  def diagnostics: List[Diagnostic] = Nil

  def constraint_=(c: Constraint): Unit = {}
  def undetVars_=(vs: List[TypeVar]): Unit = unsupported("undetVars_=")
  def diagnostics_=(ds: List[Diagnostic]): Unit = unsupported("diagnostics_=")

  def fresh: TyperState = this
}

class MutableTyperState (previous: TyperState) extends TyperState {

  private var myConstraint: Constraint = previous.constraint
  private var myUndetVars: List[TypeVar] = previous.undetVars
  private var myDiagnostics: List[Diagnostic] = Nil

  override def constraint = myConstraint
  override def undetVars = myUndetVars
  override def diagnostics = myDiagnostics

  override def constraint_=(c: Constraint) = myConstraint = c
  override def undetVars_=(vs: List[TypeVar]) = myUndetVars = vs
  override def diagnostics_=(ds: List[Diagnostic]) = myDiagnostics = ds

  override def fresh: TyperState = new MutableTyperState(this)
}
