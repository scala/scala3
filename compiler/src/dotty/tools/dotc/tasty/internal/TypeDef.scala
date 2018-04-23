package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.TypeSymbol

import scala.tasty.trees
import scala.tasty.types

object TypeDef {

  def apply(tree: tpd.TypeDef)(implicit ctx: Context): trees.TypeDef = new Impl(tree)

  def apply(sym: TypeSymbol)(implicit ctx: Context): trees.TypeDef = new Impl(tpd.TypeDef(sym))

  def unapplyTypeDef(arg: Impl): Option[trees.TypeDef.Data] = {
    val tdef = arg.tree
    implicit val localContext: Context = arg.ctx.withOwner(tdef.symbol(arg.ctx))
    val rhs = tdef.rhs match {
      case rhs: Trees.TypeBoundsTree[_] => TypeBoundsTree(rhs)
      case rhs => TypeTree(rhs)
    }
    Some((TypeName(tdef.name), rhs, Modifiers(tdef)))
  }

  private[tasty] class Impl(val tree: tpd.TypeDef)(implicit val ctx: Context) extends trees.TypeDef with Positioned {

    def tpe: types.Type = Type(tree.tpe)(ctx)

    def owner: trees.Definition = Definition(tree.symbol.owner)

    override def toString: String = {
      import Toolbox.extractor
      val trees.TypeDef(name, rhs, mods) = this
      s"TypeDef($name, $rhs, $mods)"
    }
  }

}
