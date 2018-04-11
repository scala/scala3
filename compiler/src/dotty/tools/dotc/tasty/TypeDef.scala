package dotty.tools.dotc.tasty

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

object TypeDef {

  def apply(tree: tpd.TypeDef)(implicit ctx: Context): scala.tasty.TypeDef = Impl(tree, ctx)

  object TypeDef {
    def unapply(term: scala.tasty.Definition): Option[(scala.tasty.TypeName, scala.tasty.TypeTree, List[scala.tasty.Modifier])] = term match {
      case Impl(tdef, ctx) if !tdef.symbol(ctx).isClass =>
        implicit val ctx_ = ctx
        if (tdef.symbol.isClass) None
        else Some((TypeName(tdef.name), TypeTree(tdef.rhs), tdef.rawMods.mods.map(mod => Modifier(mod))))
      case _ => None
    }
  }

  private case class Impl(tree: tpd.TypeDef, ctx: Context) extends scala.tasty.TypeDef with Positioned {

    def tpe: scala.tasty.Type = Type(tree.tpe)(ctx)

    override def name: scala.tasty.Name = ???

    override def owner: scala.tasty.Definition = ???

    override def toString: String = this match {
      case TypeDef(name, rhs, mods) => s"TypeDef($name, $rhs, $mods)"
      case _ => s"TypeDef{## $tree ##}"
    }
  }

}
