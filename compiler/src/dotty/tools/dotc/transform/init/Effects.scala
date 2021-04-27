package dotty.tools.dotc
package transform
package init

import ast.tpd._
import reporting.trace
import config.Printers.init
import core.Types._
import core.Symbols._
import core.Contexts._

import Potentials._

object Effects {
  type Effects = Vector[Effect]
  val empty: Effects = Vector.empty

  def show(effs: Effects)(using Context): String =
    effs.map(_.show).mkString(", ")

  /** Effects that are related to safe initialization performed on potentials */
  sealed trait Effect {
    def potential: Potential

    def show(using Context): String

    def source: Tree

    def toEffs: Effects = Vector(this)
  }

  /** A promotion effect means that a value that's possibly under initialization
   *  is promoted from the initializing world to the fully-initialized world.
   *
   *  Essentially, this effect enforces that the object pointed to by
   *  `potential` is transitively initialized.
   *
   *  This effect is trigger in several scenarios:
   *  - a potential is used as arguments to method calls or new-expressions
   *  - a potential is assigned (not initialize) to a field
   *  - the selection chain on a potential is too long
   */
  case class Promote(potential: Potential)(val source: Tree) extends Effect {
    def show(using Context): String = potential.show + "↑"
  }

  /** Field access, `a.f` */
  case class FieldAccess(potential: Potential, field: Symbol)(val source: Tree) extends Effect {
    assert(field != NoSymbol)

    def show(using Context): String = potential.show + "." + field.name.show + "!"
  }

  /** Method call, `a.m()` */
  case class MethodCall(potential: Potential, method: Symbol)(val source: Tree) extends Effect {
    assert(method != NoSymbol)

    def show(using Context): String = potential.show + "." + method.name.show + "!"
  }

  // ------------------ operations on effects ------------------

  def asSeenFrom(eff: Effect, thisValue: Potential)(implicit env: Env): Effect =
    trace(eff.show + " asSeenFrom " + thisValue.show + ", current = " + currentClass.show, init, _.asInstanceOf[Effect].show) { eff match {
      case Promote(pot) =>
        val pot1 = Potentials.asSeenFrom(pot, thisValue)
        Promote(pot1)(eff.source)

      case FieldAccess(pot, field) =>
        val pot1 = Potentials.asSeenFrom(pot, thisValue)
        FieldAccess(pot1, field)(eff.source)

      case MethodCall(pot, sym) =>
        val pot1 = Potentials.asSeenFrom(pot, thisValue)
        MethodCall(pot1, sym)(eff.source)
    } }

  def asSeenFrom(effs: Effects, thisValue: Potential)(implicit env: Env): Effects =
    effs.map(asSeenFrom(_, thisValue))
}