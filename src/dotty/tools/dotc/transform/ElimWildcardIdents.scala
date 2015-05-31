package dotty.tools.dotc
package transform

import TreeTransforms.{MiniPhaseTransform, TransformerInfo}
import ast.tpd
import ast.Trees._
import core._
import Contexts.Context
import Symbols._
import Types._
import StdNames._

/**
 * Replace Ident("_") in tree with default values of corresponding type:
 *   numerics: `0`
 *   booleans: `false`
 *   classes: `null`
 */
class ElimWildcardIdents extends MiniPhaseTransform {
  import ast.tpd._
  def phaseName: String = "elimWildcardIdents"

  def wildcardToDefaultValue(tree: Tree)(implicit ctx: Context) = {
    def recur(x: Tree): Tree = x match {
      case x: Ident if x.name == nme.WILDCARD && x.symbol.isClass => defaultValue(tree.tpe)
      case Block(Nil, y) => recur(y)
      case _ => tree
    }
    recur(tree)
  }

  override def transformValDef(tree: tpd.ValDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree =
    cpy.ValDef(tree)(rhs = wildcardToDefaultValue(tree.rhs))

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree =
    cpy.DefDef(tree)(rhs = wildcardToDefaultValue(tree.rhs))
}
