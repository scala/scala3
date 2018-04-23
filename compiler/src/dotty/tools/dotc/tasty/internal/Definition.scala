package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Flags._

import scala.tasty.trees

object Definition {

  def apply(tree: tpd.Tree)(implicit ctx: Context): trees.Definition = tree match {
    case tree: tpd.ValDef => ValDef(tree)
    case tree: tpd.DefDef => DefDef(tree)
    case tree: tpd.TypeDef =>
      if (tree.symbol.isClass) ClassDef(tree)
      else TypeDef(tree)
  }

  def apply(sym: Symbol)(implicit ctx: Context): trees.Definition = {
    if (sym.is(Package)) PackageDef(sym)
    else if (sym.isClass) ClassDef(sym.asClass)
    else if (sym.isType) TypeDef(sym.asType)
    else if (sym.is(Method)) DefDef(sym.asTerm)
    else ValDef(sym.asTerm)
  }

  private[tasty] object NoDefinition extends trees.Definition {
    override def owner: trees.Definition = NoDefinition

    override def pos: scala.tasty.Position = ???

    override def toString: String = "NoDefinition"
  }

}
