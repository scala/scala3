package dotty.tools
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

  override def transformApply(app: Apply)(using Context): Tree = app.fun match
    case Select(fn, nme.apply) if defn.isFunctionType(fn.tpe) =>
      val app1 = BetaReduce(app, fn, app.args)
      if app1 ne app then report.log(i"beta reduce $app -> $app1")
      app1
    case _ =>
      app


object BetaReduce:
  import ast.tpd._

  /** Beta-reduces a call to `fn` with arguments `argSyms` or returns `tree` */
  def apply(tree: Apply, fn: Tree, args: List[Tree])(using Context): Tree =
    fn match
      case Typed(expr, _) => BetaReduce(tree, expr, args)
      case Block(Nil, expr) => BetaReduce(tree, expr, args)
      case Inlined(_, Nil, expr) => BetaReduce(tree, expr, args)
      case Block((anonFun: DefDef) :: Nil, closure: Closure) => BetaReduce(anonFun, args)
      case _ => tree
  end apply

  /** Beta-reduces a call to `ddef` with arguments `argSyms` */
  def apply(ddef: DefDef, args: List[Tree])(using Context) =
    val bindings = List.Buffer[ValDef]()
    val vparams = ddef.vparamss.flatten
    assert(args.hasSameLengthAs(vparams))
    val argSyms =
      args.zipped(vparams).map ((arg, param) =>
        arg.tpe.dealias match
          case ref @ TermRef(NoPrefix, _) if isPurePath(arg) =>
            ref.symbol
          case _ =>
            val flags = Synthetic | (param.symbol.flags & Erased)
            val tpe = if arg.tpe.dealias.isInstanceOf[ConstantType] then arg.tpe.dealias else arg.tpe.widen
            val binding = ValDef(newSymbol(ctx.owner, param.name, flags, tpe, coord = arg.span), arg).withSpan(arg.span)
            bindings += binding
            binding.symbol
      )

    val expansion = TreeTypeMap(
      oldOwners = ddef.symbol :: Nil,
      newOwners = ctx.owner :: Nil,
      substFrom = vparams.map(_.symbol),
      substTo = argSyms
    ).transform(ddef.rhs)

    val expansion1 = new TreeMap {
      override def transform(tree: Tree)(using Context) = tree.tpe.widenTermRefExpr match
        case ConstantType(const) if isPureExpr(tree) => cpy.Literal(tree)(const)
        case _ => super.transform(tree)
    }.transform(expansion)
    val bindings1 =
      bindings.toList.filterNot(vdef => vdef.tpt.tpe.isInstanceOf[ConstantType] && isPureExpr(vdef.rhs))

    seq(bindings1, expansion1)
  end apply
