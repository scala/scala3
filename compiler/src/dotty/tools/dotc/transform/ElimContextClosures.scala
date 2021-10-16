package dotty.tools
package dotc
package transform

import MegaPhase._
import core._
import Symbols._
import SymDenotations._
import Contexts._
import Types._
import Flags._
import Decorators._
import DenotTransformers._
import core.StdNames.nme
import core.StdNames
import ast.Trees._
import reporting.trace

/** Transforms function arguments which are context functions to
 * avoid a build-up of redundant thunks when passed repeatedly,
 * e.g. due to recursion.
 *
 * This is necessary because the compiler produces a contextual
 * closure around values passed as arguments where a context function
 * is expected, unless that value has the syntactic form of a context
 * function literal.
 *
 * This makes for very ergonomic client code, but the implementation
 * requires the wrapper to be generated before type information is available.
 * Thus, it can't be determined if the passed value is already a context function
 * of the expected type, and the closure must be generated either way.
 *
 * Without this phase, when a contextual function is passed as an argument to a
 * recursive function, that would have the unfortunate effect of a linear growth
 * in transient thunks of identical type wrapped around each other, leading
 * to performance degradation, and in some cases, stack overflows.
 *
 * For additional reading material, please refer to the Simplicitly paper and/or
 * the discussion at https://github.com/lampepfl/dotty/issues/10889
 */
class ElimContextClosures extends MiniPhase with IdentityDenotTransformer { thisPhase: DenotTransformer =>
  import ast.tpd._
  import ast.untpd

  override def phaseName:String = ElimContextClosures.name

  override def transformApply(tree: Apply)(using Context): Tree =
    trace(s"transforming ${tree.show} at phase ${ctx.phase}", show = true) {

      def transformArg(arg: Tree, formal: Type): Tree = {
        val formal1 = formal.widenDealias
        if defn.isContextFunctionType(formal1) && untpd.isContextualClosure(arg) then
          val body = unsplice(closureBody(arg)) match {
            case Apply(Select(fn, nme.apply), _) => fn
            case other => other
          } // no-op if not a nested closure of some kind
          val underlyingBodyType = body.tpe.widenDealias
          val bodyIsContextual = defn.isContextFunctionType(underlyingBodyType)
          val bodyTypeMatches = TypeComparer.isSubType(underlyingBodyType, formal1)
          if bodyIsContextual && bodyTypeMatches then
            body
          else
            arg

        else
          arg
      }

      val mt @ MethodType(_) = tree.fun.tpe.widen
      val args1 = tree.args.zipWithConserve(mt.paramInfos)(transformArg)
      cpy.Apply(tree)(tree.fun, args1)
    }
}

object ElimContextClosures {
  val name: String = "elimContextClosures"
}