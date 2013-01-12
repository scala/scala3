package dotty.tools.dotc.core

import Periods._, Denotations._, Contexts._, Types._
import java.lang.AssertionError

trait DenotationTransformers { self: RootContext =>

  import DenotationTransformers._

  def lastPhaseId: PhaseId

  private val nxTransformer =
    Array.fill[DenotationTransformer](lastPhaseId + 1)(NoTransformer)

  object NoTransformer extends DenotationTransformer {
    val phaseId = lastPhaseId + 1
    def transform(enot: Denotation): Denotation =
      throw new AssertionError("NoTransformer.transform")
  }

  def install(pid: PhaseId, trans: DenotationTransformer): Unit = {
    if ((pid > NoPhaseId) && (nxTransformer(pid).phaseId > pid)) {
      nxTransformer(pid) = trans
      install(pid - 1, trans)
    }
  }

  def nextTransformer(i: Int) = nxTransformer(i)
}

object DenotationTransformers {

  abstract class DenotationTransformer {
    val phaseId: PhaseId
    def transform(denot: Denotation): Denotation
    def transform(tpe: Type): Type = ???
  }


}