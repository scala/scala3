package dotty.tools.dotc
package transform

import TreeTransforms._
import core.DenotTransformers._
import core.Symbols._
import core.Contexts._
import core.Types._
import core.Flags._
import core.Decorators._
import SymUtils._
import core.StdNames.nme
import ast.Trees._

/** This phase eliminates ExprTypes `=> T` and replaces them by
 *  nullary function types.  More precisely:
 *
 *  For parameter types:
 *
 *      => T        ==>    () => T
 *
 *  For terms:
 *
 *      x           ==>    x.apply()   if x is a parameter that had type => T
 *      e.apply()   ==>    e           if e.apply() is an argument to a call-by-name parameter
 *      expr        ==>    () => expr  if other expr is an argument to a call-by-name parameter
 */
class ElimByName extends MiniPhaseTransform with InfoTransformer { thisTransformer =>
  import ast.tpd._

  override def phaseName: String = "elimByName"

  override def runsAfterGroupsOf = Set(classOf[Splitter])
    // assumes idents and selects have symbols; interferes with splitter distribution
    // that's why it's "after group".

  override def treeTransformPhase = thisTransformer.next

  /** The info of the tree's symbol at phase Nullarify (i.e. before transformation) */
  private def originalDenotation(tree: Tree)(implicit ctx: Context) =
    tree.symbol.denot(ctx.withPhase(thisTransformer))

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree =
    ctx.traceIndented(s"transforming ${tree.show} at phase ${ctx.phase}", show = true) {

    def transformArg(arg: Tree, formal: Type): Tree = formal match {
      case _: ExprType =>
        arg match {
          case Apply(Select(qual, nme.apply), Nil) if qual.tpe derivesFrom defn.FunctionClass(0) =>
            qual
          case _ =>
            val meth = ctx.newSymbol(
                ctx.owner, nme.ANON_FUN, Synthetic | Method, MethodType(Nil, Nil, arg.tpe.widen))
            Closure(meth, _ => arg.changeOwner(ctx.owner, meth))
        }
      case _ =>
        arg
    }

    /** Given that `info` is the possibly curried) method type of the
     *  tree's symbol, the method type that corresponds to the current application.
     */
    def matchingMethType(info: Type, tree: Tree): Type = tree match {
      case Apply(fn, _) => matchingMethType(info.resultType, fn)
      case _ => info
    }

    val origMethType = originalDenotation(tree.fun).info match {
      case pt: PolyType => pt.resultType
      case mt => mt
    }

    val MethodType(_, formals) = matchingMethType(origMethType, tree.fun)
    val args1 = tree.args.zipWithConserve(formals)(transformArg)
    cpy.Apply(tree)(tree.fun, args1)
  }

  override def transformIdent(tree: Ident)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val origDenot = originalDenotation(tree)
    if ((origDenot is Param) && (origDenot.info.isInstanceOf[ExprType]))
      tree.select(defn.Function0_apply).appliedToNone
    else tree
  }

  def elimByNameParams(tp: Type)(implicit ctx: Context): Type = tp match {
    case tp: PolyType =>
      tp.derivedPolyType(tp.paramNames, tp.paramBounds, elimByNameParams(tp.resultType))
    case tp: MethodType =>
      tp.derivedMethodType(tp.paramNames, tp.paramTypes mapConserve transformParamInfo,
          elimByNameParams(tp.resultType))
    case _ =>
      tp
  }

  def transformParamInfo(tp: Type)(implicit ctx: Context) = tp match {
    case ExprType(rt) => defn.FunctionType(Nil, rt)
    case _ => tp
  }

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type =
    if (sym is Param) transformParamInfo(tp)
    else elimByNameParams(tp)
}
