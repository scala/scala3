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
import config.Printers.{ transforms => debug }

/** Abstract base class of ByNameClosures and ElimByName, factoring out the
 *  common functionality to transform arguments of by-name parameters.
 */
class ElimRedundantContextualClosure extends MiniPhase with IdentityDenotTransformer { thisPhase: DenotTransformer =>
  import ast.tpd._
  import ast.untpd

  override def phaseName:String = ElimRedundantContextualClosure.name

  /** The info of the tree's symbol before it is potentially transformed in this phase */
  private def originalDenotation(tree: Tree)(using Context) =
    atPhase(thisPhase)(tree.symbol.denot)

  override def transformApply(tree: Apply)(using Context): Tree =
    trace(s"transforming ${tree.show} at phase ${ctx.phase}", show = true) {

      def transformArg(arg: Tree, formal: Type): Tree = {
        val veryFormal = formal.widenDealias
        if(defn.isContextFunctionType(veryFormal) && untpd.isContextualClosure(arg)) {
          val body = unsplice(closureBody(arg)) match {
            case Apply(Select(fn, nme.apply), _) => fn
            case other => other
          } // no-op if not a nested closure of some kind
          val underlyingBodyType = body.tpe.widenDealias
          val bodyIsContextual = defn.isContextFunctionType(underlyingBodyType)
          val bodyTypeMatches = TypeComparer.isSubType(underlyingBodyType, veryFormal)
          if(bodyIsContextual && bodyTypeMatches) {
            body
          } else {
            arg
          }
        } else {
          arg
        }
      }

      val mt @ MethodType(_) = tree.fun.tpe.widen
      val args1 = tree.args.zipWithConserve(mt.paramInfos)(transformArg)
      cpy.Apply(tree)(tree.fun, args1)
    }
}

object ElimRedundantContextualClosure {
  val name: String = "elimRedundantContextualClosure"
}