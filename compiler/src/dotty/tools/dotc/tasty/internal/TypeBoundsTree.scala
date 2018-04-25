package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types

import scala.tasty.trees
import scala.tasty.types

object TypeBoundsTree {

  def apply(bounds: tpd.TypeBoundsTree): trees.TypeBoundsTree = new Impl(bounds)

  def unapplyTypeBounds(arg: Impl)(implicit ctx: Context): Option[trees.TypeBoundsTree.Data] = {
    Some(TypeTree(arg.tree.lo), TypeTree(arg.tree.hi))
  }

  private[tasty] class Impl(val tree: tpd.TypeBoundsTree) extends trees.TypeBoundsTree with Positioned {

    override def tpe: types.TypeBounds = TypeBounds(tree.tpe.asInstanceOf[Types.TypeBounds])

    override def toString: String = "TypeBoundsTree"
  }
}
