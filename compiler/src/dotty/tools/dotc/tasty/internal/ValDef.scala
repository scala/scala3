package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.TermSymbol

import scala.tasty.{modifiers, trees, types}

object ValDef {

  def apply(tree: tpd.ValDef): trees.ValDef = new Impl(tree)

  def apply(sym: TermSymbol)(implicit ctx: Context): trees.ValDef = new Impl(tpd.ValDef(sym))

  def unapplyValDef(arg: Impl)(implicit ctx: Context): Option[trees.ValDef.Data] = {
    val vdef = arg.tree
    val rhs = if (vdef.rhs.isEmpty) None else Some(Term(vdef.rhs))
    Some((TermName(vdef.name), TypeTree(vdef.tpt), rhs))
  }

  private def localContext(tree: tpd.Tree)(implicit ctx: Context): Context =
    if (tree.hasType && tree.symbol.exists) ctx.withOwner(tree.symbol) else ctx

  private[tasty] class Impl(val tree: tpd.ValDef) extends trees.ValDef with Definition with Positioned {
    def tpe: types.Type = Type(tree.tpe)
    def mods(implicit ctx: scala.tasty.Context): List[modifiers.Modifier] = Modifiers(tree)
    override def toString: String = "ValDef"
  }

}
