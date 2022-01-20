package dotty.tools
package dotc
package transform

import core._
import Contexts._
import Symbols._
import Types._
import Flags._
import SymDenotations.*
import DenotTransformers.InfoTransformer
import NameKinds.SuperArgName
import core.StdNames.nme
import MegaPhase.*
import Decorators.*
import reporting.trace

/** This phase translates arguments to call-by-name parameters, using the rules
 *
 *      x           ==>    x                  if x is a => parameter
 *      e.apply()   ==>    <cbn-arg>(e)       if e is pure
 *      e           ==>    <cbn-arg>(() => e) for all other arguments
 *
 *  where
 *
 *     <cbn-arg>: [T](() => T): T
 *
 *  is a synthetic method defined in Definitions. Erasure will later strip the <cbn-arg> wrappers.
 */
class ElimByNameParams extends MiniPhase, InfoTransformer:
  thisPhase =>

  import ast.tpd._

  override def phaseName: String = ElimByNameParams.name

  override def runsAfterGroupsOf: Set[String] = Set(ExpandSAMs.name, ElimRepeated.name)
    // - ExpanSAMs applied to partial functions creates methods that need
    //   to be fully defined before converting. Test case is pos/i9391.scala.
    // - ByNameLambda needs to run in a group after ElimRepeated since ElimRepeated
    //   works on simple arguments but not converted closures, and it sees the arguments
    //   after transformations by subsequent miniphases in the same group.

  override def changesParents: Boolean = true
    // Expr types in parent type arguments are changed to function types.

  /** If denotation had an ExprType before, it now gets a function type */
  private def exprBecomesFunction(symd: SymDenotation)(using Context): Boolean =
    symd.is(Param) || symd.is(ParamAccessor, butNot = Method)

  def transformInfo(tp: Type, sym: Symbol)(using Context): Type = tp match {
    case ExprType(rt) if exprBecomesFunction(sym) =>
      defn.ByNameFunction(rt)
    case tp: MethodType =>
      def exprToFun(tp: Type) = tp match
        case ExprType(rt) => defn.ByNameFunction(rt)
        case tp => tp
      tp.derivedLambdaType(
        paramInfos = tp.paramInfos.mapConserve(exprToFun),
        resType = transformInfo(tp.resType, sym))
    case tp: PolyType =>
      tp.derivedLambdaType(resType = transformInfo(tp.resType, sym))
    case _ => tp
  }

  override def infoMayChange(sym: Symbol)(using Context): Boolean =
    sym.is(Method) || exprBecomesFunction(sym)

  def byNameClosure(arg: Tree, argType: Type)(using Context): Tree =
    val meth = newAnonFun(ctx.owner, MethodType(Nil, argType), coord = arg.span)
    Closure(meth,
        _ => arg.changeOwnerAfter(ctx.owner, meth, thisPhase),
        targetType = defn.ByNameFunction(argType)
      ).withSpan(arg.span)

  private def isByNameRef(tree: Tree)(using Context): Boolean =
    defn.isByNameFunction(tree.tpe.widen)

  /** Map `tree` to `tree.apply()` is `tree` is of type `() ?=> T` */
  private def applyIfFunction(tree: Tree)(using Context) =
    if isByNameRef(tree) then
      val tree0 = transformFollowing(tree)
      atPhase(next) { tree0.select(defn.ContextFunction0_apply).appliedToNone }
    else tree

  override def transformIdent(tree: Ident)(using Context): Tree =
    applyIfFunction(tree)

  override def transformSelect(tree: Select)(using Context): Tree =
    applyIfFunction(tree)

  override def transformTypeApply(tree: TypeApply)(using Context): Tree = tree match {
    case TypeApply(Select(_, nme.asInstanceOf_), arg :: Nil) =>
      // tree might be of form e.asInstanceOf[x.type] where x becomes a function.
      // See pos/t296.scala
      applyIfFunction(tree)
    case _ => tree
  }

  override def transformApply(tree: Apply)(using Context): Tree =
    trace(s"transforming ${tree.show} at phase ${ctx.phase}", show = true) {

      def transformArg(arg: Tree, formal: Type): Tree = formal match
        case defn.ByNameFunction(formalResult) =>
          def stripTyped(t: Tree): Tree = t match
            case Typed(expr, _) => stripTyped(expr)
            case _ => t
          stripTyped(arg) match
            case Apply(Select(qual, nme.apply), Nil)
            if isByNameRef(qual) && (isPureExpr(qual) || qual.symbol.isAllOf(InlineParam)) =>
              qual
            case _ =>
              if isByNameRef(arg) || arg.symbol.name.is(SuperArgName)
              then arg
              else
                var argType = arg.tpe.widenIfUnstable
                if argType.isBottomType then argType = formalResult
                byNameClosure(arg, argType)
        case _ =>
          arg

      val mt @ MethodType(_) = tree.fun.tpe.widen
      val args1 = tree.args.zipWithConserve(mt.paramInfos)(transformArg)
      cpy.Apply(tree)(tree.fun, args1)
    }

  override def transformValDef(tree: ValDef)(using Context): Tree =
    atPhase(next) {
      if exprBecomesFunction(tree.symbol) then
        cpy.ValDef(tree)(tpt = tree.tpt.withType(tree.symbol.info))
      else tree
    }

object ElimByNameParams:
  val name: String = "elimByNameParams"