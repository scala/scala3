package dotty.tools
package dotc
package transform

import core._
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

  override def transformApply(app: Apply)(using ctx: Context): Tree = app.fun match
    case Select(fn, nme.apply) if defn.isFunctionType(fn.tpe) =>
      val app1 = betaReduce(app, fn, app.args)
      if app1 ne app then ctx.log(i"beta reduce $app -> $app1")
      app1
    case _ =>
      app

  private def betaReduce(tree: Apply, fn: Tree, args: List[Tree])(using ctx: Context): Tree =
    fn match
      case Typed(expr, _) => betaReduce(tree, expr, args)
      case Block(Nil, expr) => betaReduce(tree, expr, args)
      case Block((anonFun: DefDef) :: Nil, closure: Closure) =>
        val argSyms =
          for arg <- args yield
            arg.tpe.dealias match
              case ref @ TermRef(NoPrefix, _) if isPurePath(arg) => ref.symbol
              case _ => NoSymbol
        val vparams = anonFun.vparamss.head
        if argSyms.forall(_.exists) && argSyms.hasSameLengthAs(vparams) then
          TreeTypeMap(
            oldOwners = anonFun.symbol :: Nil,
            newOwners = ctx.owner :: Nil,
            substFrom = vparams.map(_.symbol),
            substTo = argSyms).transform(anonFun.rhs)
        else tree
      case _ => tree
