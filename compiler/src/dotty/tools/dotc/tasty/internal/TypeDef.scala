package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.TypeSymbol

import scala.tasty
import scala.tasty.trees
import scala.tasty.types

object TypeDef {

  def apply(tree: tpd.TypeDef): trees.TypeDef = new Impl(tree)

  def apply(sym: TypeSymbol)(implicit ctx: Context): trees.TypeDef = new Impl(tpd.TypeDef(sym))

  def unapplyTypeDef(arg: Impl)(implicit ctx: Context): Option[trees.TypeDef.Data] = {
    val tdef = arg.tree
    val rhs = tdef.rhs match {
      case rhs: tpd.TypeBoundsTree => TypeBoundsTree(rhs)
      case rhs => TypeTree(rhs)
    }
    Some((TypeName(tdef.name), rhs, Modifiers(tdef)))
  }

  private[tasty] class Impl(val tree: tpd.TypeDef) extends trees.TypeDef with Definition with Positioned {

    def tpe: types.Type = Type(tree.tpe)

    override def toString: String = "TypeDef"
  }

}
