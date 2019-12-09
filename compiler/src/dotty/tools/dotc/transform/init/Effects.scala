package dotty.tools.dotc
package transform
package init

import ast.tpd._
import core.Types._
import core.Symbols._
import core.Contexts._

import Potentials._

object Effects {
  type Effects = Set[Effect]
  val empty: Effects = Set.empty

  def show(effs: Effects)(implicit ctx: Context): String =
    effs.map(_.show).mkString(", ")

  sealed trait Effect {
    def size: Int
    def show(implicit ctx: Context): String
    def source: Tree
  }

  case class Leak(potential: Potential)(val source: Tree) extends Effect {
    def size: Int = potential.size
    def show(implicit ctx: Context): String =
      potential.show + "↑"
  }

  case class FieldAccess(potential: Potential, field: Symbol)(val source: Tree) extends Effect {
    def size: Int = potential.size
    def show(implicit ctx: Context): String =
      potential.show + "." + field.name.show + "!"
  }

  case class MethodCall(potential: Potential, method: Symbol, virtual: Boolean)(val source: Tree) extends Effect {
    def size: Int = potential.size
    def show(implicit ctx: Context): String = {
      val modifier = if (virtual) "" else "(static)"
      potential.show + "." + method.name.show + "!" + modifier
    }
  }
}