package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.{Trees, tpd, untpd}
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.trees

object Import {

  def apply(tree: tpd.Tree)(implicit ctx: Context): trees.Import = Impl(tree, ctx)

  def unapplyImport(tree: trees.Tree): Option[trees.Import.Data] = tree match {
    case Impl(Trees.Import(expr, selectors), ctx) => Some(Term(expr)(ctx), selectors.map(ImportSelector(_)(ctx)))
    case _ => None
  }

  private case class Impl(tree: tpd.Tree, ctx: Context) extends trees.Import with Positioned {
    override def toString: String = {
      import Toolbox.extractor
      val trees.Import(pkg, body) = this
      s"Import($pkg, $body)"
    }
  }

}
