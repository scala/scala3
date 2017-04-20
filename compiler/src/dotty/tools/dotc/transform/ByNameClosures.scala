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
 *      e.apply()   ==>    DummyApply(e)       if e is pure
 *      e           ==>    DummyApply(() => e) for all other arguments
 *
 *  where
 *
 *     DummyApply: [T](() => T): T
 *
 *  is a synthetic method defined in Definitions. Erasure will later strip these DummyApply wrappers.
 */
class ByNameClosures extends TransformByNameApply with IdentityDenotTransformer { thisTransformer =>
  import ast.tpd._

  override def phaseName: String = "bynameClosures"

  override def mkClosure(arg: Tree, argType: Type)(implicit ctx: Context): Tree = {
    val inSuper = if (ctx.mode.is(Mode.InSuperCall)) InSuperCall else EmptyFlags
    val meth = ctx.newSymbol(
      ctx.owner, nme.ANON_FUN, Synthetic | Method | inSuper, MethodType(Nil, Nil, argType))
    Closure(meth, _ => arg.changeOwnerAfter(ctx.owner, meth, thisTransformer))
  }
}
