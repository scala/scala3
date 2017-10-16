package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.DenotTransformers.InfoTransformer
import dotty.tools.dotc.core.Flags._
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
class UnusedRefs extends MiniPhaseTransform with InfoTransformer {
  import tpd._

  override def phaseName: String = "unusedRefs"

  /** Check what the phase achieves, to be called at any point after it is finished. */
  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = {
    val checker = new TypeMap {
      override def apply(tp: Type): Type = tp match {
        case tp: TermParamRef => assert(!tp.binder.isUnusedMethod); tp
        case tp: TermRef if !tp.symbol.is(Method) => assert(!tp.symbol.is(Unused)); tp
        case _ => mapOver(tp)
      }
    }
    tree match {
      case _: ValDef =>
      case _: Apply | _: RefTree =>
        assert(!tree.symbol.is(Unused))
        checker.apply(tree.tpe)
      case _ =>
        checker.apply(tree.tpe)
    }
  }

  /* Tree transform */

  override def transformApply(tree: tpd.Apply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = transformUnused(tree)
  override def transformIdent(tree: tpd.Ident)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = transformUnused(tree)
  override def transformSelect(tree: tpd.Select)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = transformUnused(tree)

  private def transformUnused(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = {
    if (!tree.symbol.is(Unused)) tree
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

  override def transformTypeTree(tree: tpd.TypeTree)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val newType = removeUnusedPaths(tree.tpe)
    if (tree.tpe == newType) tree
    else TypeTree(newType)
  }


  /* Tree info */

  override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type =
    removeUnusedPaths(tp)

  private def removeUnusedPaths(tp: Type)(implicit ctx: Context): Type = {
    // TODO: handle variance
    new TypeMap {
      override def apply(tp: Type): Type = tp match {
        case tp: TermParamRef if tp.binder.isUnusedMethod => tp.classSymbol.typeRef
        case tp: TermRef if tp.symbol.is(Unused) => tp.widen
        case _ => mapOver(tp)
      }
    }.apply(tp)
  }
}
