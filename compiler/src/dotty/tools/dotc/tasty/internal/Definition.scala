package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.statements

object Definition {

  def apply(tree: tpd.Tree)(implicit ctx: Context): statements.Definition = tree match {
    case tree: tpd.ValDef => ValDef(tree)
    case tree: tpd.DefDef => DefDef(tree)
    case tree: tpd.TypeDef =>
      if (tree.symbol.isClass) ClassDef(tree)
      else TypeDef(tree)
  }

}
