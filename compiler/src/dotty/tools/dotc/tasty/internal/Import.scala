package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.{Trees, tpd, untpd}
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.trees

object Import {

  def apply(tree: tpd.Tree): trees.Import = new Impl(tree)

  def unapplyImport(arg: Impl)(implicit ctx: Context) : Option[trees.Import.Data] = {
    val Trees.Import(expr, selectors) = arg.tree
    Some(Term(expr), selectors.map(ImportSelector(_)))
  }

  private[tasty] class Impl(val tree: tpd.Tree)extends trees.Import with Positioned {
    override def toString: String = "Import"
  }

}
