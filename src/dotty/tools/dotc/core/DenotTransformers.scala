package dotty.tools.dotc
package core

import Periods._
import SymDenotations._
import Contexts._
import Types._
import Symbols._
import Denotations._
import Phases._
import java.lang.AssertionError
import dotty.tools.dotc.util.DotClass

object DenotTransformers {

  /** A transformer group contains a sequence of transformers,
   *  ordered by the phase where they apply. Transformers are added
   *  to a group via `install`.
   */

  /** A transformer transforms denotations at a given phase */
  trait DenotTransformer extends Phase {

    /** The last phase during which the transformed denotations are valid */
    def lastPhaseId(implicit ctx: Context) = ctx.nextDenotTransformerId(id + 1)

    /** The validity period of the transformer in the given context */
    def validFor(implicit ctx: Context): Period =
      Period(ctx.runId, id, lastPhaseId)

    /** The transformation method */
    def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation
  }

  trait InfoTransformer extends DenotTransformer {

    def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type

    /** The transformation method */
    def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation = {
      val info1 = transformInfo(ref.info, ref.symbol)
      if (info1 eq ref.info) ref
      else ref match {
        case ref: SymDenotation => ref.copySymDenotation(info = info1)
        case _ => ref.derivedSingleDenotation(ref.symbol, info1)
      }
    }
  }

  /** A `DenotTransformer` trait that has the identity as its `transform` method.
   *  You might want to inherit from this trait so that new denotations can be
   *  installed using `installAfter` and `enteredAfter` at the end of the phase.
   */
  trait IdentityDenotTransformer extends DenotTransformer {
    def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation = ref
  }
}
