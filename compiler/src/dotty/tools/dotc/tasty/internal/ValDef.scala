package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.statement

object ValDef {

  def apply(tree: tpd.ValDef)(implicit ctx: Context): statement.ValDef = Impl(tree, ctx)

  object ValDef {
    def unapply(term: statement.TopLevelStatement): Option[statement.ValDef.Data] = term match {
      case Impl(vdef, ctx) =>
        implicit val ctx_ = ctx
        Some((TermName(vdef.name), TypeTree(vdef.tpt), if (vdef.rhs.isEmpty) None else Some(Term(vdef.rhs)), vdef.rawMods.mods.map(mod => Modifier(mod))))
      case _ => None
    }
  }

  private case class Impl(tree: tpd.ValDef, ctx: Context) extends statement.ValDef with Positioned {

    def tpe: scala.tasty.Type = Type(tree.tpe)(ctx)

    override def name: scala.tasty.Name = ???

    override def owner: statement.Definition = ???

    override def toString: String = this match {
      case ValDef(name, tpt, rhs, mods) => s"ValDef($name, $tpt, $rhs, $mods)"
      case _ => s"ValDef{## $tree ##}"
    }

  }

}
