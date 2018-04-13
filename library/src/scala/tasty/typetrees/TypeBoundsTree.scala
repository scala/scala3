package scala.tasty.typetrees

import scala.runtime.tasty.Toolbox

import scala.tasty.types.TypeBounds

trait TypeBoundsTree extends MaybeTypeTree {
  def tpe: TypeBounds
}

object TypeBoundsTree {
  type Data = (TypeTree, TypeTree)
  def unapply(arg: MaybeTypeTree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyTypeBoundsTree(arg)
}