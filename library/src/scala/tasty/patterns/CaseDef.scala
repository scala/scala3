package scala.tasty
package patterns

import scala.runtime.tasty.Toolbox
import scala.tasty.terms.Term

trait CaseDef extends Tree

object CaseDef {
  type Data = (Pattern, Option[Term], Term)
  def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyCaseDef(arg)
}
