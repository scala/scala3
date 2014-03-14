package dotty.tools.dotc
package core

import Periods._
import SymDenotations._
import Contexts._
import Types._
import Denotations._
import java.lang.AssertionError
import dotty.tools.dotc.util.DotClass

object Transformers {

  trait TransformerBase { self: ContextBase =>
    val infoTransformers = new TransformerGroup
  }

  /** A transformer group contains a sequence of transformers,
   *  ordered by the phase where they apply. Transformers are added
   *  to a group via `install`.
   *
   *  There are two transformerGroups in a context base:
   *  symTransformers and refTransformers. symTransformers translate
   *  full symbol denotations, refTransformers translate only symbol references
   *  of type Unique/JointRefDenotation.
   */
  class TransformerGroup {

    private val nxTransformer =
      Array.fill[Transformer](MaxPossiblePhaseId + 1)(NoTransformer)

    def nextTransformer(pid: PhaseId) = nxTransformer(pid)

    def install(pid: PhaseId, transFn: TransformerGroup => Transformer): Unit =
      if ((pid > NoPhaseId) && (nxTransformer(pid).phaseId > pid)) {
        val trans = transFn(this)
        trans._phaseId = pid
        nxTransformer(pid) = transFn(this)
        install(pid - 1, transFn)
      }

    /** A sentinel transformer object */
    object NoTransformer extends Transformer(this) {
      _phaseId = MaxPossiblePhaseId + 1
      override def lastPhaseId = phaseId - 1 // TODO JZ Probably off-by-N error here. MO: Don't think so: we want empty validity period.
      def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation =
        unsupported("transform")
    }
  }

  /** A transformer transforms denotations at a given phase */
  abstract class Transformer(group: TransformerGroup) extends DotClass {

    private[Transformers] var _phaseId: PhaseId = _

    /** The phase at the start of which the denotations are transformed */
    def phaseId: PhaseId = _phaseId

    /** The last phase during which the transformed denotations are valid */
    def lastPhaseId = group.nextTransformer(phaseId).phaseId - 1

    /** The validity period of the transformer in the given context */
    def validFor(implicit ctx: Context): Period =
      Period(ctx.runId, phaseId, lastPhaseId)

    /** The transformation method */
    def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation
  }
}
