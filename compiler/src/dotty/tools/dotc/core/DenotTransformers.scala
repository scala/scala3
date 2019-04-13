package dotty.tools.dotc
package core

import Periods._
import SymDenotations._
import Contexts._
import Types._
import Symbols._
import Denotations._
import Phases._

object DenotTransformers {

  /** A transformer group contains a sequence of transformers,
   *  ordered by the phase where they apply. Transformers are added
   *  to a group via `install`.
   */

  /** A transformer transforms denotations at a given phase */
  trait DenotTransformer extends Phase {

    /** The last phase during which the transformed denotations are valid */
    def lastPhaseId(implicit ctx: Context): Int = ctx.base.nextDenotTransformerId(id + 1)

    /** The validity period of the transformed denotations in the given context */
    def validFor(implicit ctx: Context): Period =
      Period(ctx.runId, id + 1, lastPhaseId)

    /** The transformation method */
    def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation
  }

  /** A transformer that only transforms the info field of denotations */
  trait InfoTransformer extends DenotTransformer {

    def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type

    def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation = {
      val sym = ref.symbol
      if (sym.exists && !mayChange(sym)) ref
      else {
        val info1 = transformInfo(ref.info, ref.symbol)
        if (info1 eq ref.info) ref
        else ref match {
          case ref: SymDenotation =>
            ref.copySymDenotation(info = info1).copyCaches(ref, ctx.phase.next)
          case _ =>
            ref.derivedSingleDenotation(ref.symbol, info1)
        }
      }
    }

    /** Denotations with a symbol where `mayChange` is false are guaranteed to be
     *  unaffected by this transform, so `transformInfo` need not be run. This
     *  can save time, and more importantly, can help avoid forcing symbol completers.
     */
    protected def mayChange(sym: Symbol)(implicit ctx: Context): Boolean = true
  }

  /** A transformer that only transforms SymDenotations.
   *  Note: Infos of non-sym denotations are left as is. So the transformer should
   *  be used before erasure only if this is not a problem. After erasure, all
   *  denotations are SymDenotations, so SymTransformers can be used freely.
   */
  trait SymTransformer extends DenotTransformer {

    def transformSym(sym: SymDenotation)(implicit ctx: Context): SymDenotation

    def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation = ref match {
      case ref: SymDenotation => transformSym(ref)
      case _ => ref
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
