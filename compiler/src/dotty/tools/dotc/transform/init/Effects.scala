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
  type Effects = Set[Effect]
  val empty: Effects = Set.empty

  def show(effs: Effects)(using Context): String =
    effs.map(_.show).mkString(", ")

  /** Effects that are related to safe initialization */
  sealed trait Effect {
    def size: Int
    def show(using Context): String
    def source: Tree
  }

  /** An effect means that a value that's possibly under initialization
   *  is promoted from the initializing world to the fully-initialized world.
   *
   *  Essentially, this effect enforces that the object pointed to by
   *  `potential` is fully initialized.
   *
   *  This effect is trigger in several scenarios:
   *  - a potential is used as arguments to method calls or new-expressions
   *  - a potential is assigned (not initialize) to a field
   *  - the selection chain on a potential is too long
   */
  case class Promote(potential: Potential)(val source: Tree) extends Effect {
    def size: Int = potential.size
    def show(using Context): String =
      potential.show + "↑"
  }

  /** Field access, `a.f` */
  case class FieldAccess(potential: Potential, field: Symbol)(val source: Tree) extends Effect {
    assert(field != NoSymbol)

    def size: Int = potential.size
    def show(using Context): String =
      potential.show + "." + field.name.show + "!"
  }

  /** Method call, `a.m()` */
  case class MethodCall(potential: Potential, method: Symbol)(val source: Tree) extends Effect {
    assert(method != NoSymbol)

    def size: Int = potential.size
    def show(using Context): String = potential.show + "." + method.name.show + "!"
  }

  // ------------------ operations on effects ------------------

  extension (eff: Effect) def toEffs: Effects = Effects.empty + eff

  def asSeenFrom(eff: Effect, thisValue: Potential)(implicit env: Env): Effects =
    trace(eff.show + " asSeenFrom " + thisValue.show + ", current = " + currentClass.show, init, effs => show(effs.asInstanceOf[Effects])) { eff match {
      case Promote(pot) =>
        Potentials.asSeenFrom(pot, thisValue).promote(eff.source)

      case FieldAccess(pot, field) =>
        Potentials.asSeenFrom(pot, thisValue).map { pot =>
          FieldAccess(pot, field)(eff.source)
        }

      case MethodCall(pot, sym) =>
        Potentials.asSeenFrom(pot, thisValue).map { pot =>
          MethodCall(pot, sym)(eff.source)
        }
    } }

  def asSeenFrom(effs: Effects, thisValue: Potential)(implicit env: Env): Effects =
    effs.flatMap(asSeenFrom(_, thisValue))
}