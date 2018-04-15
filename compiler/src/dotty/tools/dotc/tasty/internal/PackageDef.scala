package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.trees

object PackageDef {

  // TODO make sure all extractors are tested

  def apply(tree: tpd.Tree)(implicit ctx: Context): trees.PackageDef = Impl(tree, ctx)

  def unapplyPackageDef(tree: trees.Tree): Option[trees.PackageDef.Data] = tree match {
    case Impl(pkgDef@Trees.PackageDef(pkg, body), ctx) =>
      val localContext = ctx.withOwner(pkgDef.symbol(ctx))
      Some(Term(pkg)(ctx), body.map(TopLevelStatement(_)(localContext)))
    case _ => None
  }

  private case class Impl(tree: tpd.Tree, ctx: Context) extends trees.PackageDef with Positioned {
    override def toString: String = {
      import Toolbox.extractor
      val trees.PackageDef(pkg, body) = this
      s"Package($pkg, $body)"
    }
  }
}
