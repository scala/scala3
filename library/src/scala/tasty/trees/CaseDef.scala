package scala.tasty.trees

import scala.runtime.tasty.Toolbox
import scala.tasty.Tree

trait CaseDef extends Tree

object CaseDef {
  type Data = (Pattern, Option[Term], Term)
  def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyCaseDef(arg)
}
