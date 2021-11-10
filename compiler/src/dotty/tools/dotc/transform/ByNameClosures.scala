package dotty.tools.dotc
package transform

import core._
import Contexts._
import Symbols._
import Types._
import Flags._
import DenotTransformers.IdentityDenotTransformer
import core.StdNames.nme

/** This phase translates arguments to call-by-name parameters, using the rules
 *
 *      x           ==>    x                  if x is a => parameter
 *      e.apply()   ==>    <cbn-arg>(e)       if e is pure
 *      e           ==>    <cbn-arg>(() => e) for all other arguments
 *
 *  where
 *
 *     <cbn-arg>: [T](() => T): T
 *
 *  is a synthetic method defined in Definitions. Erasure will later strip the <cbn-arg> wrappers.
 */
class ByNameClosures extends TransformByNameApply with IdentityDenotTransformer { thisPhase =>
  import ast.tpd._

  override def phaseName: String = ByNameClosures.name

  override def runsAfterGroupsOf: Set[String] = Set(ExpandSAMs.name)
    // ExpanSAMs applied to partial functions creates methods that need
    // to be fully defined before converting. Test case is pos/i9391.scala.

  override def mkByNameClosure(arg: Tree, argType: Type)(using Context): Tree =
    val meth = newAnonFun(ctx.owner, MethodType(Nil, argType))
    Closure(meth, _ => arg.changeOwnerAfter(ctx.owner, meth, thisPhase)).withSpan(arg.span)
}

object ByNameClosures {
  val name: String = "byNameClosures"
}