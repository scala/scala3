package dotty.tools.dotc
package transform

import TreeTransforms._
import core._
import Symbols._
import SymDenotations._
import Contexts._
import Types._
import Flags._
import Decorators._
import DenotTransformers.IdentityDenotTransformer
import core.StdNames.nme

/** This phase translates arguments to call-by-name parameters, using the rules
 *
 *      x           ==>    x                   if x is a => parameter
 *      e.apply()   ==>    <cbn-arg>(e)       if e is pure
 *      e           ==>    <cbn-arg>(() => e) for all other arguments
 *
 *  where
 *
 *     <cbn-arg>: [T](() => T): T
 *
 *  is a synthetic method defined in Definitions. Erasure will later strip the <cbn-arg> wrappers.
 */
class ByNameClosures extends TransformByNameApply with IdentityDenotTransformer { thisTransformer =>
  import ast.tpd._

  override def phaseName: String = "byNameClosures"

  override def mkClosure(arg: Tree, argType: Type)(implicit ctx: Context): Tree = {
    val meth = ctx.newSymbol(
      ctx.owner, nme.ANON_FUN, Synthetic | Method, MethodType(Nil, Nil, argType))
    Closure(meth, _ => arg.changeOwnerAfter(ctx.owner, meth, thisTransformer))
  }
}
