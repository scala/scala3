package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.TermSymbol

import scala.tasty.trees
import scala.tasty.types

object DefDef {

  def apply(tree: tpd.DefDef)(implicit ctx: Context): trees.DefDef = new Impl(tree)

  def apply(sym: TermSymbol)(implicit ctx: Context): trees.DefDef = new Impl(tpd.DefDef(sym))

  def unapplyDefDef(arg: Impl): Option[trees.DefDef.Data] = {
    val ddef = arg.tree
    implicit val localContext: Context = arg.ctx.withOwner(ddef.symbol(arg.ctx))
    Some((TermName(ddef.name), ddef.tparams.map(TypeDef(_)), ddef.vparamss.map(_.map(ValDef(_))), TypeTree(ddef.tpt), if (ddef.rhs.isEmpty) None else Some(Term(ddef.rhs)), Modifiers(ddef)))
  }

  private[tasty] class Impl(val tree: tpd.DefDef)(implicit val ctx: Context) extends trees.DefDef with Positioned {

    def tpe: types.Type = Type(tree.tpe)(ctx)

    def owner: trees.Definition = Definition(tree.symbol.owner)

    override def toString: String = {
      import Toolbox.extractor
      val trees.DefDef(name, typeParams, paramss, returnTpt, rhs, mods) = this
      s"DefDef($name, $typeParams, $paramss, $returnTpt, $rhs, $mods)"
    }
  }

}
