package scala.tasty
package statements

import scala.runtime.tasty.Toolbox
import scala.tasty.modifiers.Modifier

trait ValDef extends Definition

object ValDef {
  type Data = (names.TermName, typetrees.TypeTree, Option[terms.Term], List[Modifier])
  def unapply(arg: TopLevelStatement)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyValDef(arg)
}
