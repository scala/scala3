package scala.tasty
package trees

import scala.runtime.tasty.Toolbox
import scala.tasty.modifiers.Modifier

trait ValDef extends Definition

object ValDef {
  type Data = (names.TermName, TypeTree, Option[Term], List[Modifier])
  def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyValDef(arg)
}
