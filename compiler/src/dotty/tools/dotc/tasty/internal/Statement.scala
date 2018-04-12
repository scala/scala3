package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.statements

object Statement {
  def apply(tree: tpd.Tree)(implicit ctx: Context): statements.Statement = tree match {
    case tree: tpd.Import => Import(tree)
    case tree: tpd.DefTree => Definition(tree)(ctx)
    case _ => Term(tree)
  }
}

