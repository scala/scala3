package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.DenotTransformers.InfoTransformer
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

/** This phase removes unused parameters and arguments.
 *  It assumes that:
 *    - arguments are pure and can just be dropped
 *    - parameters not never used
 *
 *  `f(x1,...)(@unused y1,...)` with at least one unused argument list (might not be at the end)
 *    -->
 *  `f(x1,...)`
 *
 *  `def f(x1: T1,...)(@unused y1: U1,...) = ...`
 *    -->
 *  `def f(x1: T1,...) = ...`
 */
class UnusedParams extends MiniPhaseTransform with InfoTransformer {
  import tpd._

  override def phaseName: String = "unusedParams"

  override def runsAfterGroupsOf = Set(
    classOf[UnusedArgLift], // ensures args are pure
    classOf[UnusedRefs] // ensures params are not used
  )

  /** Check what the phase achieves, to be called at any point after it is finished. */
  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = tree match {
    case tree: DefDef => assert(!hasUnusedParams(tree.tpe))
    case tree: ValDef if tree.symbol.is(Param) => assert(!tree.symbol.isUnused)
    case _ =>
  }


  /* Tree transform */

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree =
    if (hadUnusedParams(tree.symbol.termRef)) removeUnusedParams(tree)
    else tree

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = tree.tpe.widen match {
    case _: MethodType => tree // Do the transformation higher in the tree if needed
    case _ =>
      if (hadUnusedParams(tree.symbol.termRef)) removeUnusedApplies(tree)
      else tree
  }


  /* Info transform */

  override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = {
    if (!hasUnusedParams(tp)) tp
    else removeUnusedMethodTypes(tp) match {
      case mt: MethodOrPoly => mt
      case tpe if sym.isConstructor => MethodType(Nil, Nil, tpe)
      case tpe => ExprType(tpe)
    }
  }


  /* private methods */

  private def hadUnusedParams(tp: TermRef)(implicit ctx: Context): Boolean =
    hasUnusedParams(widenInPreviousPhase(tp))

  private def hasUnusedParams(tp: Type): Boolean = tp match {
    case tp: MethodType if tp.isUnusedMethod => true
    case tp: MethodOrPoly => hasUnusedParams(tp.resType)
    case _ => false
  }

  private def removeUnusedMethodTypes(tp: Type)(implicit ctx: Context): Type = tp match {
    case tp: MethodType if tp.isUnusedMethod => removeUnusedMethodTypes(tp.resType)
    case tp: MethodOrPoly => tp.derivedLambdaType(resType = removeUnusedMethodTypes(tp.resType))
    case _ => tp
  }

  private def removeUnusedParams(tree: DefDef)(implicit ctx: Context): DefDef = {
    def removeUnused(args: List[List[ValDef]], tp: Type): List[List[ValDef]] = args match {
      case x :: xs =>
        assert(tp.isInstanceOf[MethodType])
        if (tp.isUnusedMethod) removeUnused(xs, tp.resultType)
        else x :: removeUnused(xs, tp.resultType)
      case _ =>
        assert(!tp.isInstanceOf[MethodOrPoly])
        Nil
    }
    val paramTypes = widenInPreviousPhase(tree.symbol.termRef) match {
      case tp: PolyType => tp.resType
      case tp => tp
    }
    cpy.DefDef(tree)(vparamss = removeUnused(tree.vparamss, paramTypes))
  }

  private def removeUnusedApplies(tree: Tree)(implicit ctx: Context): Tree = tree match {
    case tree: Apply =>
      val newFun = removeUnusedApplies(tree.fun)
      if (widenInPreviousPhase(tree.fun.tpe).isUnusedMethod) newFun
      else cpy.Apply(tree)(newFun, tree.args)
    case _ => tree
  }

  private def widenInPreviousPhase(tpe: Type)(implicit ctx: Context): Type =
    tpe.widen(ctx.withPhase(ctx.phase.prev))

}
