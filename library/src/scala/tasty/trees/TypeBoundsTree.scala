package scala.tasty.trees

import scala.runtime.tasty.Toolbox
import scala.tasty.Tree
import scala.tasty.types.TypeBounds

trait TypeBoundsTree extends Tree {
  def tpe: TypeBounds
}

object TypeBoundsTree {
  type Data = (TypeTree, TypeTree)
  def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyTypeBoundsTree(arg)
}
