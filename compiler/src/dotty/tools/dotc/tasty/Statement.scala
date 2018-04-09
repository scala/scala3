package dotty.tools.dotc.tasty

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.tpd.Tree
import dotty.tools.dotc.core.Contexts.Context


object TopLevelStatement {
  def apply(tree: Tree)(implicit ctx: Context): scala.tasty.TopLevelStatement = tree match {
    case tree @ Trees.PackageDef(_, _) => Package(tree)
    case _ => Statement(tree)
  }
}

object Statement {
  def apply(tree: Tree)(implicit ctx: Context): scala.tasty.Statement = tree match {
    case tree @ Trees.Import(_, _) => Import(tree)
    // TODO definitions
    case _ => Term(tree)
  }
}

