package dotty.tools.dotc.tasty

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

object ValDef {

  def apply(tree: tpd.ValDef)(implicit ctx: Context): scala.tasty.ValDef = Impl(tree, ctx)

  object ValDef {
    def unapply(term: scala.tasty.Definition): Option[(scala.tasty.TermName, scala.tasty.TypeTree, Option[scala.tasty.Term], List[scala.tasty.Modifier])] = term match {
      case Impl(vdef, ctx) =>
        implicit val ctx_ = ctx
        Some((TermName(vdef.name), TypeTree(vdef.tpt), if (vdef.rhs.isEmpty) None else Some(Term(vdef.rhs)), vdef.rawMods.mods.map(mod => Modifier(mod))))
      case _ => None
    }
  }

  private case class Impl(tree: tpd.ValDef, ctx: Context) extends scala.tasty.ValDef with Positioned {

    def tpe: scala.tasty.Type = Type(tree.tpe)(ctx)

    override def name: scala.tasty.Name = ???

    override def owner: scala.tasty.Definition = ???

    override def toString: String = this match {
      case ValDef(name, tpt, rhs, mods) => s"ValDef($name, $tpt, $rhs, $mods)"
      case _ => s"ValDef{## $tree ##}"
    }

  }

}
