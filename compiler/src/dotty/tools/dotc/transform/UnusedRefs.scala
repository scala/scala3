package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

/** This phase removes all references and calls to unused methods or vals
 *
 *   if `@unused def f(x1,...,xn): T = ...`
 *   then `f(y1,...,yn)` --> `y1; ...; yn; (default value for T)`
 *
 *   if   `@unused val x: T = ...` including parameters
 *   then `x` --> `(default value for T)`
 */
class UnusedRefs extends MiniPhaseTransform {
  import tpd._

  override def phaseName: String = "removeUnusedCalls"

  /** Check what the phase achieves, to be called at any point after it is finished. */
  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = tree match {
    case _: Apply | _: RefTree => assert(!tree.symbol.isUnused)
    case _ =>
  }

  /* Tree transform */

  override def transformApply(tree: tpd.Apply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = transformUnused(tree)
  override def transformIdent(tree: tpd.Ident)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = transformUnused(tree)
  override def transformSelect(tree: tpd.Select)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = transformUnused(tree)

  private def transformUnused(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = {
    if (!tree.symbol.isUnused) tree
    else {
      tree.tpe.widen match {
        case _: MethodType => tree // Do the transformation higher in the tree if needed
        case _ =>
          val result = tpd.defaultValue(tree.tpe)
          tree match {
            case _: RefTree => result
            case Apply(_ , args) => seq(args, result)
          }
      }
    }
  }

}
