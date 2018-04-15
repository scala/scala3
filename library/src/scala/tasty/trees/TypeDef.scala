package scala.tasty.trees

import scala.runtime.tasty.Toolbox
import scala.tasty.modifiers.Modifier
import scala.tasty.names

trait TypeDef extends Definition

object TypeDef {
  type Data = (names.TypeName, Tree /* Type | TypeBounds */, List[Modifier])
  def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyTypeDef(arg)
}
