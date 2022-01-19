package dotty.tools
package dotc
package transform

import core._
import Flags._
import MegaPhase._, DenotTransformers.IdentityDenotTransformer
import Symbols._, Contexts._, Types._, Decorators._
import StdNames.nme
import ast.Trees._
import ast.TreeTypeMap

/** Rewrite applications `<byname>(...)` to context closures `() ?=> ...` */
class ByNameLambda extends MiniPhase, IdentityDenotTransformer:
  thisPhase =>

  import ast.tpd._

  def phaseName: String = ByNameLambda.name

  override def runsAfterGroupsOf: Set[String] = Set(ElimRepeated.name)
    // ByNameLambda needs to run in a group after ElimRepeated since ElimRepeated
    // works on ByName arguments but not converted closures, and it sees the arguments
    // after transformations by subsequent miniphases in the same group.

  override def transformByName(tree: ByName)(using Context): Tree = tree.expr match
    case Apply(Select(fn, nme.apply), Nil) if isPurePath(fn) && fn.tpe.widen.isByName =>
      fn
    case _ =>
      byNameClosure(tree.expr)

  def byNameClosure(body: Tree)(using Context): Block =
    val restpe = body.tpe.widenIfUnstable.deskolemized
    val meth = newAnonFun(ctx.owner, MethodType(Nil, restpe), coord = body.span)
    Closure(meth, _ => body.changeOwnerAfter(ctx.owner, meth, thisPhase),
      targetType = defn.ContextFunction0.typeRef.appliedTo(restpe))

object ByNameLambda:
  import ast.tpd._

  val name = "byNameLambda"

end ByNameLambda