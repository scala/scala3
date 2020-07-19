package dottyBench.tools.dotc
package transform

import core._
import Decorators._, Flags._, Types._, Contexts._, Symbols._
import ast.tpd._
import Flags._
import MegaPhase.MiniPhase

/** Eliminates syntactic references to package terms as prefixes of classes, so that there's no chance
 *  they accidentally end up in the backend.
 */
class ElimPackagePrefixes extends MiniPhase {

  override def phaseName: String = "elimPackagePrefixes"

  override def transformSelect(tree: Select)(using Ctx, CState): Tree =
    if (isPackageClassRef(tree)) Ident(tree.tpe.asInstanceOf[TypeRef]) else tree

  override def checkPostCondition(tree: Tree)(using Ctx, CState): Unit = tree match {
    case tree: Select =>
      assert(!isPackageClassRef(tree), i"Unexpected reference to package in $tree")
    case _ =>
  }

  /** Is the given tree a reference to a type in a package? */
  private def isPackageClassRef(tree: Select)(using Ctx, CState): Boolean = tree.tpe match {
    case TypeRef(prefix, _) => prefix.termSymbol.is(Package)
    case _ => false
  }
}
