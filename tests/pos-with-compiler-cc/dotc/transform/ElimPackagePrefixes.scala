package dotty.tools.dotc
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

  override def phaseName: String = ElimPackagePrefixes.name

  override def description: String = ElimPackagePrefixes.description

  override def transformSelect(tree: Select)(using Context): Tree =
    if (isPackageClassRef(tree)) Ident(tree.tpe.asInstanceOf[TypeRef]) else tree

  override def checkPostCondition(tree: Tree)(using Context): Unit = tree match {
    case tree: Select =>
      assert(!isPackageClassRef(tree), i"Unexpected reference to package in $tree")
    case _ =>
  }

  /** Is the given tree a reference to a type in a package? */
  private def isPackageClassRef(tree: Select)(using Context): Boolean = tree.tpe match {
    case TypeRef(prefix, _) => prefix.termSymbol.is(Package)
    case _ => false
  }
}

object ElimPackagePrefixes:
  val name: String = "elimPackagePrefixes"
  val description: String = "eliminate references to package prefixes in Select nodes"
