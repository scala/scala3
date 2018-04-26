package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.TypeSymbol

import scala.tasty.{modifiers, trees, types}

object TypeDef {

  def apply(tree: tpd.TypeDef): trees.TypeDef = new Impl(tree)

  def apply(sym: TypeSymbol)(implicit ctx: Context): trees.TypeDef = new Impl(tpd.TypeDef(sym))

  def unapplyTypeDef(arg: Impl)(implicit ctx: Context): Option[trees.TypeDef.Data] = {
    val tdef = arg.tree
    val rhs = tdef.rhs match {
      case rhs: tpd.TypeBoundsTree => TypeBoundsTree(rhs)
      case rhs => TypeTree(rhs)
    }
    Some((TypeName(tdef.name), rhs))
  }

  private[tasty] class Impl(val tree: tpd.TypeDef) extends trees.TypeDef with Definition with Positioned {
    def tpe: types.Type = Type(tree.tpe)
    def mods(implicit ctx: scala.tasty.Context): List[modifiers.Modifier] = Modifiers(tree)
    override def toString: String = "TypeDef"
  }

}
