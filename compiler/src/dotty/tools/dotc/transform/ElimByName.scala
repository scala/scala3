package dotty.tools.dotc
package transform

import MegaPhase._
import core._
import DenotTransformers.InfoTransformer
import Symbols._
import SymDenotations._
import Contexts._
import Types._
import Flags._
import Decorators._
import SymUtils._
import util.Attachment
import core.StdNames.nme
import ast.Trees._

/** This phase eliminates ExprTypes `=> T` as types of method parameter references, and replaces them b
 *  nullary function types.  More precisely:
 *
 *  For the types of parameter symbols:
 *
 *         => T       ==>    () => T
 *
 *  For cbn parameter values
 *
 *         x          ==>    x()
 *
 *  Note: This scheme to have inconsistent types between method types (whose formal types are still
 *  ExprTypes and parameter valdefs (which are now FunctionTypes) is not pretty. There are two
 *  other options which have been abandoned or not yet pursued.
 *
 *  Option 1: Transform => T to () => T also in method and function types. The problem with this is
 *  that is that it requires to look at every type, and this forces too much, causing
 *  Cyclic Reference errors. Abandoned for this reason.
 *
 *  Option 2: Merge ElimByName with erasure, or have it run immediately before. This has not been
 *  tried yet.
 */
class ElimByName extends TransformByNameApply with InfoTransformer {
  import ast.tpd._

  override def phaseName: String = "elimByName"

  override def runsAfterGroupsOf = Set(classOf[Splitter])
    // I got errors running this phase in an earlier group, but I did not track them down.

  /** Map `tree` to `tree.apply()` is `ftree` was of ExprType and becomes now a function */
  private def applyIfFunction(tree: Tree, ftree: Tree)(implicit ctx: Context) =
    if (isByNameRef(ftree))
      ctx.atPhase(next) { implicit ctx => tree.select(defn.Function0_apply).appliedToNone }
    else tree

  override def transformIdent(tree: Ident)(implicit ctx: Context): Tree =
    applyIfFunction(tree, tree)

  override def transformSelect(tree: Select)(implicit ctx: Context): Tree =
    applyIfFunction(tree, tree)

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context): Tree = tree match {
    case TypeApply(Select(_, nme.asInstanceOf_), arg :: Nil) =>
      // tree might be of form e.asInstanceOf[x.type] where x becomes a function.
      // See pos/t296.scala
      applyIfFunction(tree, arg)
    case _ => tree
  }

  override def transformValDef(tree: ValDef)(implicit ctx: Context): Tree =
    ctx.atPhase(next) { implicit ctx =>
      if (exprBecomesFunction(tree.symbol))
        cpy.ValDef(tree)(tpt = tree.tpt.withType(tree.symbol.info))
      else tree
    }

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = tp match {
    case ExprType(rt) if exprBecomesFunction(sym) => defn.FunctionOf(Nil, rt)
    case _ => tp
  }

  override def mayChange(sym: Symbol)(implicit ctx: Context): Boolean = sym.isTerm
}
