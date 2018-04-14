package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types

import scala.tasty.{trees, types}

object TypeBoundsTree {

  def apply(bounds: tpd.TypeBoundsTree)(implicit ctx: Context): trees.TypeBoundsTree = Impl(bounds, ctx)

  def unapplyTypeBounds(tree: scala.tasty.Tree): Option[trees.TypeBoundsTree.Data] = tree match {
    case Impl(Trees.TypeBoundsTree(lo, hi), ctx) => Some(TypeTree(lo)(ctx), TypeTree(hi)(ctx))
    case _ => None
  }

  private case class Impl(tree: tpd.TypeBoundsTree, ctx: Context) extends trees.TypeBoundsTree with Positioned {

    override def tpe: types.TypeBounds = TypeBounds(tree.tpe.asInstanceOf[Types.TypeBounds])(ctx)

    override def toString: String = {
      import Toolbox.extractor
      val trees.TypeBoundsTree(lo, hi) = this
      s"TypeBoundsTree($lo, $hi)"
    }
  }
}
