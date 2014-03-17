package dotty.tools.dotc
package transform

import TreeTransforms._
import core.DenotTransformers._
import core.Denotations._
import core.SymDenotations._
import core.Contexts._
import core.Types._
import ast.Trees._
import ast.tpd.{Apply, Tree, cpy}

class UncurryTreeTransform extends TreeTransform with DenotTransformer {

  override def name: String = "uncurry"
  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree =
    ctx.traceIndented(s"transforming ${tree.show}", show = true) {
    tree.fun match {
      case Apply(fn, args) =>
        def showType(implicit ctx: Context) =
          ctx.log(s"at ${ctx.phase} ${fn.symbol} has type ${fn.symbol.info.widen.show}")
        showType
        ctx.atNextPhase(showType(_))
        showType
        cpy.Apply(tree, fn, args ++ tree.args)
      case _ => tree
    }}

  def uncurry(tp: Type)(implicit ctx: Context): Type = tp match {
    case tp @ MethodType(pnames1, ptypes1) =>
      tp.resultType match {
        case rt @ MethodType(pnames2, ptypes2) =>
          tp.derivedMethodType(pnames1 ++ pnames2, ptypes1 ++ ptypes2, rt.resultType)
        case _ =>
          tp
      }
    case tp: PolyType =>
      tp.derivedPolyType(tp.paramNames, tp.paramBounds, uncurry(tp.resultType))
    case _ =>
      tp
  }

  def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation = {
    val info1 = uncurry(ref.info)
    if (info1 eq ref.info) ref
    else ref match {
      case ref: SymDenotation => ref.copySymDenotation(info = info1)
      case _ => ref.derivedSingleDenotation(ref.symbol, info1)
    }
  }
}