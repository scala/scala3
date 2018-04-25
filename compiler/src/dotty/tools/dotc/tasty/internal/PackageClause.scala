package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.trees

object PackageClause {

  // TODO make sure all extractors are tested

  def apply(tree: tpd.PackageDef): trees.PackageClause = new Impl(tree)

  def unapplyPackageClause(arg: Impl)(implicit ctx: Context): Option[trees.PackageClause.Data] =
    Some(Term(arg.tree.pid), arg.tree.stats.map(TopLevelStatement(_)))

  private[tasty] class Impl(val tree: tpd.PackageDef) extends trees.PackageClause with Positioned {

    def definition(implicit tctx: scala.tasty.Context): trees.Definition = {
      implicit val ctx = tctx.asInstanceOf[TastyContext].ctx
      PackageDef(tree.symbol)
    }

    override def toString: String = "PackageClause"
  }
}
