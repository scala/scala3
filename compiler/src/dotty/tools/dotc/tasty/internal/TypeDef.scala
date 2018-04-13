package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty
import scala.tasty.statements
import scala.tasty.types

object TypeDef {

  def apply(tree: tpd.TypeDef)(implicit ctx: Context): statements.TypeDef = Impl(tree, ctx)

  def unapplyTypeDef(term: statements.TopLevelStatement): Option[statements.TypeDef.Data] = term match {
    case Impl(tdef, ctx) if !tdef.symbol(ctx).isClass =>
      implicit val ctx_ = ctx
      if (tdef.symbol.isClass) None
      else Some((TypeName(tdef.name), MaybeTypeTree(tdef.rhs), Modifiers(tdef)))
    case _ => None
  }

  private case class Impl(tree: tpd.TypeDef, ctx: Context) extends statements.TypeDef with Positioned {

    def tpe: types.Type = Type(tree.tpe)(ctx)

    def sym: scala.tasty.Symbol = TastySymbol(tree.symbol(ctx))(ctx)

    override def toString: String = {
      import Toolbox.extractor
      val statements.TypeDef(name, rhs, mods) = this
      s"TypeDef($name, $rhs, $mods)"
    }
  }

}
