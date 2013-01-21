package dotty.tools.dotc
package core

import Periods._, SymDenotations._, Contexts._, Types._, Denotations._
import java.lang.AssertionError

trait Transformers { self: RootContext =>

  import Transformers._

  def transformersFor(ref: SingleDenotation): TransformerGroup = ref match {
    case _: SymDenotation => denotTransformers
    case _ => refTransformers
  }

  val denotTransformers = new TransformerGroup
  val refTransformers = new TransformerGroup
}

object Transformers {

  val lastPhaseId = 31

  class TransformerGroup {

    abstract class Transformer extends DotClass {
      val phaseId: Int
      def lastPhaseId = nextTransformer(phaseId).phaseId - 1
      def validFor(implicit ctx: Context): Period =
        Period(ctx.runId, phaseId, lastPhaseId)
      def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation
    }

    object NoTransformer extends Transformer {
      val phaseId = lastPhaseId + 1
      def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation =
        unsupported("transform")
    }

    private val nxTransformer =
      Array.fill[Transformer](lastPhaseId + 1)(NoTransformer)

    def nextTransformer(i: Int) = nxTransformer(i)

    def install(pid: PhaseId, trans: Transformer): Unit =
      if ((pid > NoPhaseId) && (nxTransformer(pid).phaseId > pid)) {
        nxTransformer(pid) = trans
        install(pid - 1, trans)
      }
  }
}
