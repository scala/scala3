package dotty.tools.dotc
package transform

import core._
import MegaPhase._
import Contexts.Context
import Symbols._
import Types._
import StdNames._
import ast.Trees._
import dotty.tools.dotc.ast.tpd


/** This phase rewrites calls to `Array.apply` to primitive array instantion.
 *
 *  Transforms `scala.Array.apply([....])` and `scala.Array.apply(..., [....])` into `[...]`
 */
class ArrayApply extends MiniPhase {
  import tpd._

  override def phaseName: String = "arrayApply"

  override def transformApply(tree: tpd.Apply)(implicit ctx: Context): tpd.Tree = {
    if (tree.symbol.name == nme.apply && tree.symbol.owner == defn.ArrayModule) { // Is `Array.apply`
      tree.args match {
        case CleanTree(Apply(wrapRefArrayMeth, (seqLit: tpd.JavaSeqLiteral) :: Nil)) :: ct :: Nil
            if defn.WrapArrayMethods().contains(wrapRefArrayMeth.symbol) && elideClassTag(ct) =>
          seqLit

        case elem0 :: CleanTree(Apply(wrapRefArrayMeth, (seqLit: tpd.JavaSeqLiteral) :: Nil)) :: Nil
            if defn.WrapArrayMethods().contains(wrapRefArrayMeth.symbol) =>
          tpd.JavaSeqLiteral(elem0 :: seqLit.elems, seqLit.elemtpt)

        case _ =>
          tree
      }

    } else tree
  }

  // Only optimize when classtag is `ClassTag.apply` or `ClassTag.{Byte, Boolean, ...}`
  private def elideClassTag(ct: Tree)(implicit ctx: Context): Boolean = {
    ct.symbol.maybeOwner.companionModule == defn.ClassTagModule
  }

  object CleanTree {
    def unapply(tree: Tree)(implicit ctx: Context): Some[Tree] = tree match {
      case Block(Nil, expr) => unapply(expr)
      case Typed(expr, _) => unapply(expr)
      case _ => Some(tree)
    }
  }
}
