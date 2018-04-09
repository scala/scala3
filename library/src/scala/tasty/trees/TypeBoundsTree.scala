package scala.tasty
package trees

import scala.tasty.types.TypeBounds

trait TypeBoundsTree extends Tree {
  def tpe: TypeBounds
}

object TypeBoundsTree {
  type Data = (TypeTree, TypeTree)
  def unapply(arg: TypeBoundsTree)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyTypeBoundsTree(arg)
}
