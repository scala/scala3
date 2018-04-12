package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.statements

object TopLevelStatement {
  def apply(tree: tpd.Tree)(implicit ctx: Context): statements.TopLevelStatement = tree match {
    case tree: tpd.PackageDef => Package(tree)
    case _ => Statement(tree)
  }
}
