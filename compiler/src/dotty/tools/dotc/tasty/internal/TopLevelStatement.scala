package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.trees

object TopLevelStatement {
  def apply(tree: tpd.Tree)(implicit ctx: Context): trees.TopLevelStatement = tree match {
    case tree: tpd.PackageDef => PackageClause(tree)
    case _ => Statement(tree)
  }
}
