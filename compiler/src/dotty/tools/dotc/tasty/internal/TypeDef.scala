package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.statements
import scala.tasty.types

object TypeDef {

  def apply(tree: tpd.TypeDef)(implicit ctx: Context): statements.TypeDef = Impl(tree, ctx)

  def unapply(term: statements.TopLevelStatement): Option[statements.TypeDef.Data] = term match {
    case Impl(tdef, ctx) if !tdef.symbol(ctx).isClass =>
      implicit val ctx_ = ctx
      if (tdef.symbol.isClass) None
      else Some((TypeName(tdef.name), TypeTree(tdef.rhs), tdef.rawMods.mods.map(mod => Modifier(mod))))
    case _ => None
  }

  private case class Impl(tree: tpd.TypeDef, ctx: Context) extends statements.TypeDef with Positioned {

    def tpe: types.Type = Type(tree.tpe)(ctx)

    override def name: scala.tasty.names.Name = ???

    override def owner: statements.Definition = ???

    override def toString: String = this match {
      case TypeDef(name, rhs, mods) => s"TypeDef($name, $rhs, $mods)"
      case _ => s"TypeDef{## $tree ##}"
    }
  }

}
