package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.statement

object TopLevelStatement {
  def apply(tree: tpd.Tree)(implicit ctx: Context): statement.TopLevelStatement = tree match {
    case tree: tpd.PackageDef => Package(tree)
    case _ => Statement(tree)
  }
}

object Statement {
  def apply(tree: tpd.Tree)(implicit ctx: Context): statement.Statement = tree match {
    case tree: tpd.Import => Import(tree)
    case tree: tpd.DefTree => Definition(tree)(ctx)
    case _ => Term(tree)
  }
}

