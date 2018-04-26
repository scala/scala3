package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.TermSymbol

import scala.tasty.modifiers
import scala.tasty.trees
import scala.tasty.types

object DefDef {

  def apply(tree: tpd.DefDef): trees.DefDef = new Impl(tree)

  def apply(sym: TermSymbol)(implicit ctx: Context): trees.DefDef = new Impl(tpd.DefDef(sym))

  def unapplyDefDef(arg: Impl)(implicit ctx: Context): Option[trees.DefDef.Data] = {
    val ddef = arg.tree
    Some((TermName(ddef.name), ddef.tparams.map(TypeDef(_)), ddef.vparamss.map(_.map(ValDef(_))), TypeTree(ddef.tpt), if (ddef.rhs.isEmpty) None else Some(Term(ddef.rhs))))
  }

  private[tasty] class Impl(val tree: tpd.DefDef) extends trees.DefDef with Definition with Positioned {
    def tpe: types.Type = Type(tree.tpe)
    def mods(implicit ctx: scala.tasty.Context): List[modifiers.Modifier] = Modifiers(tree)
    override def toString: String = "DefDef"
  }

}
