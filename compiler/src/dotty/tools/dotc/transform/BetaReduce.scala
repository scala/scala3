package dotty.tools
package dotc
package transform

import core._
import Flags._
import MegaPhase._
import Symbols._, Contexts._, Types._, Decorators._
import StdNames.nme
import ast.TreeTypeMap

import scala.collection.mutable.ListBuffer

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

  override def phaseName: String = BetaReduce.name

  override def description: String = BetaReduce.description

  override def transformApply(app: Apply)(using Context): Tree = app.fun match
    case Select(fn, nme.apply) if defn.isFunctionType(fn.tpe) =>
      val app1 = BetaReduce(app, fn, List(app.args))
      if app1 ne app then report.log(i"beta reduce $app -> $app1")
      app1
    case TypeApply(Select(fn, nme.apply), targs) if fn.tpe.typeSymbol eq defn.PolyFunctionClass =>
      val app1 = BetaReduce(app, fn, List(targs, app.args))
      if app1 ne app then report.log(i"beta reduce $app -> $app1")
      app1
    case _ =>
      app

object BetaReduce:
  import ast.tpd._

  val name: String = "betaReduce"
  val description: String = "reduce closure applications"

  /** Beta-reduces a call to `fn` with arguments `argSyms` or returns `tree` */
  def apply(original: Tree, fn: Tree, argss: List[List[Tree]])(using Context): Tree =
    fn match
      case Typed(expr, _) =>
        BetaReduce(original, expr, argss)
      case Block((anonFun: DefDef) :: Nil, closure: Closure) =>
        BetaReduce(anonFun, argss)
      case Block((TypeDef(_, template: Template)) :: Nil, Typed(Apply(Select(New(_), _), _), _)) if template.constr.rhs.isEmpty =>
        template.body match
          case (anonFun: DefDef) :: Nil =>
            BetaReduce(anonFun, argss)
          case _ =>
            original
      case Block(stats, expr) =>
        val tree = BetaReduce(original, expr, argss)
        if tree eq original then original
        else cpy.Block(fn)(stats, tree)
      case Inlined(call, bindings, expr) =>
        val tree = BetaReduce(original, expr, argss)
        if tree eq original then original
        else cpy.Inlined(fn)(call, bindings, tree)
      case _ =>
        original
  end apply

  /** Beta-reduces a call to `ddef` with arguments `args` */
  def apply(ddef: DefDef, argss: List[List[Tree]])(using Context) =
    val bindings = new ListBuffer[DefTree]()
    val expansion1 = reduceApplication(ddef, argss, bindings)
    val bindings1 = bindings.result()
    seq(bindings1, expansion1)

  /** Beta-reduces a call to `ddef` with arguments `args` and registers new bindings */
  def reduceApplication(ddef: DefDef, argss: List[List[Tree]], bindings: ListBuffer[DefTree])(using Context): Tree =
    assert(argss.size == 1 || argss.size == 2)
    val targs = if argss.size == 2 then argss.head else Nil
    val args = argss.last
    val tparams = ddef.leadingTypeParams
    val vparams = ddef.termParamss.flatten
    assert(targs.hasSameLengthAs(tparams))
    assert(args.hasSameLengthAs(vparams))

    val targSyms =
      for (targ, tparam) <- targs.zip(tparams) yield
        targ.tpe.dealias match
          case ref @ TypeRef(NoPrefix, _) =>
            ref.symbol
          case _ =>
            val binding = TypeDef(newSymbol(ctx.owner, tparam.name, EmptyFlags, targ.tpe, coord = targ.span)).withSpan(targ.span)
            bindings += binding
            binding.symbol

    val argSyms =
      for (arg, param) <- args.zip(vparams) yield
        arg.tpe.dealias match
          case ref @ TermRef(NoPrefix, _) if isPurePath(arg) =>
            ref.symbol
          case _ =>
            val flags = Synthetic | (param.symbol.flags & Erased)
            val tpe = if arg.tpe.dealias.isInstanceOf[ConstantType] then arg.tpe.dealias else arg.tpe.widen
            val binding = ValDef(newSymbol(ctx.owner, param.name, flags, tpe, coord = arg.span), arg).withSpan(arg.span)
            if !(tpe.isInstanceOf[ConstantType] && isPureExpr(arg)) then
              bindings += binding
            binding.symbol

    val expansion = TreeTypeMap(
      oldOwners = ddef.symbol :: Nil,
      newOwners = ctx.owner :: Nil,
      substFrom = (tparams ::: vparams).map(_.symbol),
      substTo = targSyms ::: argSyms
    ).transform(ddef.rhs)

    val expansion1 = new TreeMap {
      override def transform(tree: Tree)(using Context) = tree.tpe.widenTermRefExpr match
        case ConstantType(const) if isPureExpr(tree) => cpy.Literal(tree)(const)
        case _ => super.transform(tree)
    }.transform(expansion)

    expansion1
