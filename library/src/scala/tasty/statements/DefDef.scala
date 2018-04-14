package scala.tasty
package statements

import scala.runtime.tasty.Toolbox
import scala.tasty.modifiers.Modifier

trait DefDef extends Definition

object DefDef {
  type Data = (names.TermName, List[TypeDef],  List[List[ValDef]], typetrees.TypeTree, Option[terms.Term], List[Modifier])
  def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyDefDef(arg)
}
