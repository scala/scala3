package scala.tasty
package statements

import scala.runtime.tasty.Toolbox
import scala.tasty.modifiers.Modifier

trait TypeDef extends Definition

object TypeDef {
  type Data = (names.TypeName, typetrees.TypeTree, List[Modifier])
  def unapply(arg: TopLevelStatement)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyTypeDef(arg)
}
