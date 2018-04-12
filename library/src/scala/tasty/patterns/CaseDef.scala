package scala.tasty.patterns

import scala.runtime.tasty.Toolbox
import scala.tasty.terms.Term
import scala.tasty.Positioned

trait CaseDef extends Positioned

object CaseDef {
  type Data = (Pattern, Option[Term], Term)
  def unapply(arg: CaseDef)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyCaseDef(arg)
}
