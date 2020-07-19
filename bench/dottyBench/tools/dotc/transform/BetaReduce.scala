package dottyBench.tools
package dotc
package transform

import core._
import Flags._
import MegaPhase._
import Symbols._, Contexts._, Types._, Decorators._
import StdNames.nme
import ast.Trees._
import ast.TreeTypeMap

/** Rewrite an application
 *
 *    (((x1, ..., xn) => b): T)(y1, ..., yn)
 *
 *  where
 *
 *    - all yi are pure references without a prefix
 *    - the closure can also be contextual or erased, but cannot be a SAM type
 *    _ the type ascription ...: T is optional
 *
 *  to
 *
 *    [xi := yi]b
 *
 *  This is more limited than beta reduction in inlining since it only works for simple variables `yi`.
 *  It is more general since it also works for type-ascripted closures.
 *
 *  A typical use case is eliminating redundant closures for blackbox macros that
 *  return context functions. See i6375.scala.
 */
class BetaReduce extends MiniPhase:
  import ast.tpd._

  def phaseName: String = "betaReduce"

  override def transformApply(app: Apply)(using Ctx, CState): Tree = app.fun match
    case Select(fn, nme.apply) if defn.isFunctionType(fn.tpe) =>
      val app1 = betaReduce(app, fn, app.args)
      if app1 ne app then report.log(i"beta reduce $app -> $app1")
      app1
    case _ =>
      app

  private def betaReduce(tree: Apply, fn: Tree, args: List[Tree])(using Ctx, CState): Tree =
    fn match
      case Typed(expr, _) => betaReduce(tree, expr, args)
      case Block(Nil, expr) => betaReduce(tree, expr, args)
      case Block((anonFun: DefDef) :: Nil, closure: Closure) => BetaReduce(anonFun, args)
      case _ => tree

object BetaReduce:
  import ast.tpd._

  /** Beta-reduces a call to `ddef` with arguments `argSyms` */
  def apply(ddef: DefDef, args: List[Tree])(using Ctx, CState) =
    val bindings = List.newBuilder[ValDef]
    val vparams = ddef.vparamss.iterator.flatten.toList
    assert(args.hasSameLengthAs(vparams))
    val argSyms =
      for (arg, param) <- args.zip(vparams) yield
        arg.tpe.dealias match
          case ref @ TermRef(NoPrefix, _) if isPurePath(arg) =>
            ref.symbol
          case _ =>
            val flags = Synthetic | (param.symbol.flags & Erased)
            val binding = ValDef(newSymbol(ctx.owner, param.name, flags, arg.tpe.widen, coord = arg.span), arg)
            bindings += binding
            binding.symbol

    val expansion = TreeTypeMap(
      oldOwners = ddef.symbol :: Nil,
      newOwners = ctx.owner :: Nil,
      substFrom = vparams.map(_.symbol),
      substTo = argSyms
    ).transform(ddef.rhs)

    seq(bindings.result(), expansion)
  end apply
