package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.statement

object DefDef {

  def apply(tree: tpd.DefDef)(implicit ctx: Context): statement.DefDef = Impl(tree, ctx)

  object DefDef {
    def unapply(term: statement.TopLevelStatement): Option[statement.DefDef.Data] = term match {
      case Impl(ddef, ctx) =>
        implicit val ctx_ = ctx
        Some((TermName(ddef.name), ddef.tparams.map(TypeDef(_)), ddef.vparamss.map(_.map(ValDef(_))), TypeTree(ddef.tpt), if (ddef.isEmpty) None else Some(Term(ddef.rhs)), ddef.rawMods.mods.map(Modifier(_))))
      case _ => None
    }
  }

  private case class Impl(tree: tpd.DefDef, ctx: Context) extends statement.DefDef with Positioned {

    def tpe: scala.tasty.Type = Type(tree.tpe)(ctx)

    override def name: scala.tasty.Name = ???

    override def owner: scala.tasty.statement.Definition = ???

    override def toString: String = this match {
      case DefDef(name, typeParams, paramss, returnTpt, rhs, mods) =>
        s"DefDef($name, $typeParams, $paramss, $returnTpt, $rhs, $mods)"
      case _ => s"DefDef{## $tree ##}"
    }
  }

}
