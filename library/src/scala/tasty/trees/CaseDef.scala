package scala.tasty.trees

import scala.runtime.tasty.Toolbox

trait CaseDef extends Tree

object CaseDef {
  type Data = (Pattern, Option[Term], Term)
  def unapply(arg: CaseDef)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyCaseDef(arg)
}
