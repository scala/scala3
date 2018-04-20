package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.trees
import scala.tasty.types

object ValDef {

  def apply(tree: tpd.ValDef)(implicit ctx: Context): trees.ValDef = new Impl(tree)

  def unapplyValDef(arg: Impl): Option[trees.ValDef.Data] = {
    val vdef = arg.tree
    implicit val ctx = localContext(vdef)(arg.ctx)
    val rhs = if (vdef.rhs.isEmpty) None else Some(Term(vdef.rhs))
    Some((TermName(vdef.name), TypeTree(vdef.tpt), rhs, Modifiers(vdef)))
  }

  private def localContext(tree: tpd.Tree)(implicit ctx: Context): Context =
    if (tree.hasType && tree.symbol.exists) ctx.withOwner(tree.symbol) else ctx

  private[tasty] class Impl(val tree: tpd.ValDef)(implicit val ctx: Context) extends trees.ValDef with Positioned {

    def tpe: types.Type = Type(tree.tpe)(ctx)

    def sym: scala.tasty.Symbol = TastySymbol(tree.symbol(ctx))(ctx)

    override def toString: String = {
      import Toolbox.extractor
      val trees.ValDef(name, tpt, rhs, mods) = this
      s"ValDef($name, $tpt, $rhs, $mods)"
    }

  }

}
