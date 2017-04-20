package dotty.tools.dotc
package transform

import TreeTransforms._
import core._
import Symbols._
import SymDenotations._
import Contexts._
import Types._
import Flags._
import Decorators._
import DenotTransformers._
import core.StdNames.nme
import ast.Trees._

/** Abstract base class of ByNameClosures and ELimByName, factoring out the
 *  common functionality to transform arguments of by-name parameters.
 */
abstract class TransformByNameApply extends MiniPhaseTransform { thisTransformer: DenotTransformer =>
  import ast.tpd._

  /** The info of the tree's symbol at phase Nullarify (i.e. before transformation) */
  private def originalDenotation(tree: Tree)(implicit ctx: Context) =
    tree.symbol.denot(ctx.withPhase(thisTransformer))

  /** If denotation had an ExprType before, it now gets a function type */
  protected def exprBecomesFunction(symd: SymDenotation)(implicit ctx: Context) =
    (symd is Param) || (symd is (ParamAccessor, butNot = Method))

  protected def isByNameRef(tree: Tree)(implicit ctx: Context) = {
    val origDenot = originalDenotation(tree)
    origDenot.info.isInstanceOf[ExprType] && exprBecomesFunction(origDenot)
  }

  def mkClosure(arg: Tree, argType: Type)(implicit ctx: Context): Tree = unsupported("mkClosure")

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree =
    ctx.traceIndented(s"transforming ${tree.show} at phase ${ctx.phase}", show = true) {

    def transformArg(arg: Tree, formal: Type): Tree = formal.dealias match {
      case formalExpr: ExprType =>
        var argType = arg.tpe.widenIfUnstable
        if (defn.isBottomType(argType)) argType = formal.widenExpr
        def wrap(arg: Tree) = ref(defn.dummyApply).appliedToType(argType).appliedTo(arg)
        arg match {
          case Apply(Select(qual, nme.apply), Nil)
          if qual.tpe.derivesFrom(defn.FunctionClass(0)) && isPureExpr(qual) =>
            wrap(qual)
          case _ =>
            if (isByNameRef(arg) || arg.symbol == defn.dummyApply) arg
            else wrap(mkClosure(arg, argType))
        }
      case _ =>
        arg
    }

    val mt @ MethodType(_) = tree.fun.tpe.widen
    val args1 = tree.args.zipWithConserve(mt.paramInfos)(transformArg)
    cpy.Apply(tree)(tree.fun, args1)
  }
}
