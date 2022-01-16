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

/** Rewrite applications `<byname>(...)` to context closures `() ?=> ...` */
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