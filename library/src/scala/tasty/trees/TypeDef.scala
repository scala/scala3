package scala.tasty
package trees

import scala.tasty.modifiers.Modifier

trait TypeDef extends Definition {
  def mods(implicit ctx: Context): List[Modifier]
}

object TypeDef {
  type Data = (names.TypeName, Tree /* Type | TypeBounds */)
  def unapply(arg: TypeDef)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyTypeDef(arg)
}
