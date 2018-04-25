package scala.tasty
package trees

import scala.tasty.modifiers.Modifier

trait TypeDef extends Definition

object TypeDef {
  type Data = (names.TypeName, Tree /* Type | TypeBounds */, List[Modifier])
  def unapply(arg: TypeDef)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyTypeDef(arg)
}
