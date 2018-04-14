package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.statements
import scala.tasty.types

object DefDef {

  def apply(tree: tpd.DefDef)(implicit ctx: Context): statements.DefDef = Impl(tree, ctx)

  def unapplyDefDef(tree: scala.tasty.Tree): Option[statements.DefDef.Data] = tree match {
    case Impl(ddef, ctx) =>
      implicit val ctx_ = ctx
      Some((TermName(ddef.name), ddef.tparams.map(TypeDef(_)), ddef.vparamss.map(_.map(ValDef(_))), TypeTree(ddef.tpt), if (ddef.rhs.isEmpty) None else Some(Term(ddef.rhs)), Modifiers(ddef)))
    case _ => None
  }

  private case class Impl(tree: tpd.DefDef, ctx: Context) extends statements.DefDef with Positioned {

    def tpe: types.Type = Type(tree.tpe)(ctx)

    def sym: scala.tasty.Symbol = TastySymbol(tree.symbol(ctx))(ctx)

    override def toString: String = {
      import Toolbox.extractor
      val statements.DefDef(name, typeParams, paramss, returnTpt, rhs, mods) = this
      s"DefDef($name, $typeParams, $paramss, $returnTpt, $rhs, $mods)"
    }
  }

}
