package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.{Trees, tpd, untpd}
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.trees

object Import {

  def apply(tree: tpd.Tree)(implicit ctx: Context): trees.Import = new Impl(tree)

  def unapplyImport(arg: Impl): Option[trees.Import.Data] = {
    implicit val ctx: Context = arg.ctx
    val Trees.Import(expr, selectors) = arg.tree
    Some(Term(expr), selectors.map(ImportSelector(_)))
  }

  private[tasty] class Impl(val tree: tpd.Tree)(implicit val ctx: Context) extends trees.Import with Positioned {
    override def toString: String = {
      import Toolbox.extractor
      val trees.Import(pkg, body) = this
      s"Import($pkg, $body)"
    }
  }

}
