package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.trees
import scala.tasty.types

object TypeDef {

  def apply(tree: tpd.TypeDef)(implicit ctx: Context): trees.TypeDef = Impl(tree, ctx)

  def unapplyTypeDef(tree: scala.tasty.Tree): Option[trees.TypeDef.Data] = tree match {
    case Impl(tdef, ctx) if !tdef.symbol(ctx).isClass =>
      implicit val ctx_ = ctx
      if (tdef.symbol.isClass) None
      else {
        val rhs = tdef.rhs match {
          case rhs: Trees.TypeBoundsTree[_] => TypeBoundsTree(rhs)
          case rhs => TypeTree(rhs)
        }
        Some((TypeName(tdef.name), rhs, Modifiers(tdef)))
      }
    case _ => None
  }

  private case class Impl(tree: tpd.TypeDef, ctx: Context) extends trees.TypeDef with Positioned {

    def tpe: types.Type = Type(tree.tpe)(ctx)

    def sym: scala.tasty.Symbol = TastySymbol(tree.symbol(ctx))(ctx)

    override def toString: String = {
      import Toolbox.extractor
      val trees.TypeDef(name, rhs, mods) = this
      s"TypeDef($name, $rhs, $mods)"
    }
  }

}
