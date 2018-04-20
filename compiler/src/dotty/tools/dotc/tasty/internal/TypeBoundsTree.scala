package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types

import scala.tasty.trees
import scala.tasty.types

object TypeBoundsTree {

  def apply(bounds: tpd.TypeBoundsTree)(implicit ctx: Context): trees.TypeBoundsTree = new Impl(bounds)

  def unapplyTypeBounds(arg: Impl): Option[trees.TypeBoundsTree.Data] = {
    implicit val ctx: Context = arg.ctx
    Some(TypeTree(arg.tree.lo), TypeTree(arg.tree.hi))
  }

  private[tasty] class Impl(val tree: tpd.TypeBoundsTree)(implicit val ctx: Context) extends trees.TypeBoundsTree with Positioned {

    override def tpe: types.TypeBounds = TypeBounds(tree.tpe.asInstanceOf[Types.TypeBounds])(ctx)

    override def toString: String = {
      import Toolbox.extractor
      val trees.TypeBoundsTree(lo, hi) = this
      s"TypeBoundsTree($lo, $hi)"
    }
  }
}
