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
class ByNameLambda extends MiniPhase:
  import ast.tpd._

  def phaseName: String = ByNameLambda.name

  override def transformApply(app: Apply)(using Context): Tree = app match
    case ByName(body) =>
      body match
        case Apply(Select(fn, nme.apply), Nil) if isPurePath(fn) && fn.tpe.widen.isByName =>
          fn
        case _ =>
          ByNameLambda(body)
    case _ => app

object ByNameLambda:
  import ast.tpd._

  val name = "byNameLambda"

  def apply(body: Tree)(using Context): Block =
    val restpe = body.tpe.widenIfUnstable.deskolemized
    val meth = newAnonFun(ctx.owner, MethodType(Nil, restpe), coord = body.span)
    Closure(meth, _ => body.changeOwner(ctx.owner, meth),
      targetType = defn.ContextFunction0.typeRef.appliedTo(restpe))

end ByNameLambda