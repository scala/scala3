package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.trees

object PackageClause {

  // TODO make sure all extractors are tested

  def apply(tree: tpd.PackageDef)(implicit ctx: Context): trees.PackageClause = new Impl(tree)

  def unapplyPackageClause(arg: Impl): Option[trees.PackageClause.Data] = {
    implicit val ctx: Context = arg.ctx
    val localContext = ctx.withOwner(arg.tree.symbol)
    Some(Term(arg.tree.pid), arg.tree.stats.map(TopLevelStatement(_)(localContext)))
  }

  private[tasty] class Impl(val tree: tpd.PackageDef)(implicit val ctx: Context) extends trees.PackageClause with Positioned {
    override def toString: String = {
      import Toolbox.extractor
      val trees.PackageClause(pkg, body) = this
      s"PackageClause($pkg, $body)"
    }
  }
}
