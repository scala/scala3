package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.trees

object PackageDef {

  // TODO make sure all extractors are tested

  def apply(tree: tpd.PackageDef)(implicit ctx: Context): trees.PackageDef = new Impl(tree)

  def unapplyPackageDef(arg: Impl): Option[trees.PackageDef.Data] = {
    implicit val ctx: Context = arg.ctx
    val localContext = ctx.withOwner(arg.tree.symbol)
    Some(Term(arg.tree.pid), arg.tree.stats.map(TopLevelStatement(_)(localContext)))
  }

  private[tasty] class Impl(val tree: tpd.PackageDef)(implicit val ctx: Context) extends trees.PackageDef with Positioned {
    override def toString: String = {
      import Toolbox.extractor
      val trees.PackageDef(pkg, body) = this
      s"Package($pkg, $body)"
    }
  }
}
