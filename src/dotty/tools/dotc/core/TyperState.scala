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
  def undetVars: Set[TypeVar] = Set()

  /** A map that records for instantiated type vars their instance type.
   *  Used only in a temporary way for contexts that may be retracted
   *  without also retracting the type var as a whole.
   */
  def instType: SimpleMap[TypeVar, Type] = SimpleMap.Empty

  def constraint_=(c: Constraint): Unit = {}
  def undetVars_=(vs: Set[TypeVar]): Unit = unsupported("undetVars_=")
  def instType_=(m: SimpleMap[TypeVar, Type]): Unit = unsupported("instType_=")

  def fresh: TyperState = this

  def commit()(implicit ctx: Context): Unit = unsupported("commit")

  @elidable(elidable.FINER)
  def checkConsistent(implicit ctx: Context) = ()

  override def toText(printer: Printer): Text = "ImmutableTyperState"
}

class MutableTyperState(previous: TyperState, reporter: Reporter)
extends TyperState(reporter) {

  private var myConstraint: Constraint = previous.constraint
  private var myUndetVars: Set[TypeVar] = previous.undetVars
  private var myInstType: SimpleMap[TypeVar, Type] = previous.instType

  override def constraint = myConstraint
  override def undetVars = myUndetVars
  override def instType = myInstType

  override def constraint_=(c: Constraint) = myConstraint = c
  override def undetVars_=(vs: Set[TypeVar]) = myUndetVars = vs
  override def instType_=(m: SimpleMap[TypeVar, Type]): Unit = myInstType = m

  override def fresh: TyperState = new MutableTyperState(this, new StoreReporter)

  /** Commit typer state so that its information is copied into current typer state
   *  In addition (1) the owning state of undetermined or temporarily instantiated
   *  type variables changes from this typer state to the current one. (2) Variables
   *  that were temporarily instantiated in the current typer state are permanently
   *  instantiated instead.
   */
  override def commit()(implicit ctx: Context) = {
    val targetState = ctx.typerState
    targetState.constraint = constraint
    targetState.undetVars = undetVars
    targetState.instType = instType

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

    reporter.flush()
  }

  @elidable(elidable.FINER)
  override def checkConsistent(implicit ctx: Context) = {
    def err(msg: String, what: Showable) = s"$msg: ${what.show}\n${this.show}"
    for (tvar <- undetVars)
      assert(constraint(tvar.origin).exists, err("unconstrained type var", tvar))
    val undetParams = undetVars map (_.origin)
    for (param <- constraint.domainParams)
      assert(undetParams contains param, err("junk constraint on", param))
    instType.foreachKey { tvar =>
      assert(undetVars contains tvar, err("junk instType on", tvar))
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
