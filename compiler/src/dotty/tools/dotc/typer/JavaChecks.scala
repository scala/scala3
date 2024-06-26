package dotty.tools.dotc
package typer

import core.Contexts.*
import ast.tpd.*

/** PostTyper doesn't run on java sources,
 *  but some checks still need to be applied.
 */
object JavaChecks {
  /** Check the bounds of AppliedTypeTrees. */
  private object AppliedTypeChecker extends TreeTraverser {
    def traverse(tree: Tree)(using Context): Unit = tree match
      case tpt: TypeTree =>
        Checking.checkAppliedTypesIn(tpt)
      case tree: AppliedTypeTree =>
        Checking.checkAppliedType(tree)
      case _ =>
        traverseChildren(tree)
  }

  /** Scan a tree and check it. */
  def check(tree: Tree)(using Context): Unit =
    report.debuglog("checking type bounds in " + ctx.compilationUnit.source.name)
    AppliedTypeChecker.traverse(tree)
}
