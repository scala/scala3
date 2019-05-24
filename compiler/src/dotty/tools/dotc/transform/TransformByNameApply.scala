package dotty.tools
package dotc
package transform

import MegaPhase._
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
import reporting.trace

/** Abstract base class of ByNameClosures and ElimByName, factoring out the
 *  common functionality to transform arguments of by-name parameters.
 */
abstract class TransformByNameApply extends MiniPhase { thisPhase: DenotTransformer =>
  import ast.tpd._

  /** The info of the tree's symbol before it is potentially transformed in this phase */
  private def originalDenotation(tree: Tree)(implicit ctx: Context) =
    tree.symbol.denot(ctx.withPhase(thisPhase))

  /** If denotation had an ExprType before, it now gets a function type */
  protected def exprBecomesFunction(symd: SymDenotation)(implicit ctx: Context): Boolean =
    (symd.is(Param)) || symd.is(ParamAccessor, butNot = Method)

  protected def isByNameRef(tree: Tree)(implicit ctx: Context): Boolean = {
    val origDenot = originalDenotation(tree)
    origDenot.info.isInstanceOf[ExprType] && exprBecomesFunction(origDenot)
  }

  def mkByNameClosure(arg: Tree, argType: Type)(implicit ctx: Context): Tree = unsupported(i"mkClosure($arg)")

  override def transformApply(tree: Apply)(implicit ctx: Context): Tree =
    trace(s"transforming ${tree.show} at phase ${ctx.phase}", show = true) {

    def transformArg(arg: Tree, formal: Type): Tree = formal.dealias match {
      case formalExpr: ExprType =>
        var argType = arg.tpe.widenIfUnstable
        if (defn.isBottomType(argType)) argType = formal.widenExpr
        def wrap(arg: Tree) =
          ref(defn.cbnArg).appliedToType(argType).appliedTo(arg).withSpan(arg.span)
        arg match {
          case Apply(Select(qual, nme.apply), Nil)
          if qual.tpe.derivesFrom(defn.FunctionClass(0)) && isPureExpr(qual) =>
            wrap(qual)
          case _ =>
            if (isByNameRef(arg) || arg.symbol == defn.cbnArg) arg
            else wrap(mkByNameClosure(arg, argType))
        }
      case _ =>
        arg
    }

    val mt @ MethodType(_) = tree.fun.tpe.widen
    val args1 = tree.args.zipWithConserve(mt.paramInfos)(transformArg)
    cpy.Apply(tree)(tree.fun, args1)
  }
}
