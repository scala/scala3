package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.statement

object TypeDef {

  def apply(tree: tpd.TypeDef)(implicit ctx: Context): statement.TypeDef = Impl(tree, ctx)

  def unapply(term: statement.TopLevelStatement): Option[statement.TypeDef.Data] = term match {
    case Impl(tdef, ctx) if !tdef.symbol(ctx).isClass =>
      implicit val ctx_ = ctx
      if (tdef.symbol.isClass) None
      else Some((TypeName(tdef.name), TypeTree(tdef.rhs), tdef.rawMods.mods.map(mod => Modifier(mod))))
    case _ => None
  }

  private case class Impl(tree: tpd.TypeDef, ctx: Context) extends statement.TypeDef with Positioned {

    def tpe: scala.tasty.Type = Type(tree.tpe)(ctx)

    override def name: scala.tasty.Name = ???

    override def owner: statement.Definition = ???

    override def toString: String = this match {
      case TypeDef(name, rhs, mods) => s"TypeDef($name, $rhs, $mods)"
      case _ => s"TypeDef{## $tree ##}"
    }
  }

}
