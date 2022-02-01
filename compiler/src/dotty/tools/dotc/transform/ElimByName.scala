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
import typer.RefChecks
import reporting.trace

/** This phase implements the following transformations:
 *
 *  1. For types of method and class parameters:
 *
 *     => T     becomes    () ?=> T
 *
 *  2. For references to cbn-parameters:
 *
 *     x        becomes    x.apply()
 *
 *  3. For arguments to cbn parameters
 *
 *     e        becomes    () ?=> e
 *
 *  An optimization is applied: If the argument `e` to a cbn parameter is already
 *  of type `() ?=> T` and is a pure expression, we avoid (2) and (3), i.e. we
 *  pass `e` directly instead of `() ?=> e.apply()`.
 *
 *  Note that `() ?=> T`  cannot be written in source since user-defined context functions
 *  must have at least one parameter. We use the type here as a convenient marker
 *  of something that will erase to Function0, and where we know that it came from
 *  a by-name parameter.
 *
 *  Note also that the transformation applies only to types of parameters, not to other
 *  occurrences of ExprTypes. In particular, embedded occurrences in function types
 *  such as `(=> T) => U` are left as-is here (they are eliminated in erasure).
 *  Trying to convert these as well would mean traversing all the types, and that
 *  leads to cyclic reference errors in many cases.  This can cause problems in that
 *  we might have sometimes a `() ?=> T` where a `=> T` is expected. To compensate,
 *  there is a new clause in TypeComparer#subArg that declares `() ?=> T` to be a
 *  subtype of `=> T` for arguments of type applications at any point after this phase
 *  and up to erasure.
 */
class ElimByName extends MiniPhase, InfoTransformer:
  thisPhase =>

  import ast.tpd._

  override def phaseName: String = ElimByName.name

  override def runsAfterGroupsOf: Set[String] = Set(ExpandSAMs.name, ElimRepeated.name, RefChecks.name)
    // - ExpanSAMs applied to partial functions creates methods that need
    //   to be fully defined before converting. Test case is pos/i9391.scala.
    // - ElimByName needs to run in a group after ElimRepeated since ElimRepeated
    //   works on simple arguments but not converted closures, and it sees the arguments
    //   after transformations by subsequent miniphases in the same group.
    // - ElimByName should run in a group after RefChecks, since RefChecks does heavy
    //   comparisons of signatures, and ElimByName distorts these signatures by not
    //   replacing `=>` with `() ?=> T` everywhere.

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
    report.log(i"creating by name closure for $argType")
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
              if isByNameRef(arg) || arg.symbol.name.is(SuperArgName) then arg
              else byNameClosure(arg, formalResult)
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

object ElimByName:
  val name: String = "elimByName"