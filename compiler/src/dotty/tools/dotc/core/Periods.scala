package dotty.tools.dotc.core

import Contexts._
import dotty.tools.dotc.util.DotClass

/** Periods are the central "clock" of the compiler.
 *  A period consists of a run id and a phase id.
 *  run ids represent compiler runs
 *  phase ids represent compiler phases
 */
abstract class Periods extends DotClass { self: Context =>
  import Periods._

  /** The current phase identifier */
  def phaseId: Int = period.phaseId

  /** The current run identifier */
  def runId: Int = period.runId

  /** Execute `op` at given period */
  def atPeriod[T](pd: Period)(op: Context => T): T =
    op(ctx.fresh.setPeriod(pd))

  /** Execute `op` at given phase id */
  def atPhase[T](pid: PhaseId)(op: Context => T): T =
    op(ctx.withPhase(pid))

  /** The period containing the current period where denotations do not change.
   *  We compute this by taking as first phase the first phase less or equal to
   *  the current phase that has the same "nextTransformerId". As last phase
   *  we take the next transformer id following the current phase.
   */
  def stablePeriod = {
    var first = phaseId
    val nxTrans = ctx.base.nextDenotTransformerId(first)
    while (first - 1 > NoPhaseId && (ctx.base.nextDenotTransformerId(first - 1) == nxTrans)) {
      first -= 1
    }
    Period(runId, first, nxTrans)
  }

  /** Are all base types in the current period guaranteed to be the same as in period `p`? */
  def hasSameBaseTypesAs(p: Period) = {
    val period = this.period
    period == p ||
    period.runId == p.runId &&
      this.phases(period.phaseId).sameParentsStartId ==
      this.phases(p.phaseId).sameParentsStartId
  }
}

object Periods {

  /** A period is a contiguous sequence of phase ids in some run.
   *  It is coded as follows:
   *
   *     sign, always 0        1 bit
   *     runid                17 bits
   *     last phase id:        7 bits
   *     #phases before last:  7 bits
   *
   *     // Dmitry: sign == 0 isn't actually always true, in some cases phaseId == -1 is used for shifts, that easily creates code < 0
   */
  class Period(val code: Int) extends AnyVal {

    /** The run identifier of this period. */
    def runId: RunId = code >>> (PhaseWidth * 2)

    /** The phase identifier of this single-phase period. */
    def phaseId: PhaseId = (code >>> PhaseWidth) & PhaseMask

    /** The last phase of this period */
    def lastPhaseId: PhaseId =
      (code >>> PhaseWidth) & PhaseMask

    /** The first phase of this period */
    def firstPhaseId = lastPhaseId - (code & PhaseMask)

    def containsPhaseId(id: PhaseId) = firstPhaseId <= id && id <= lastPhaseId

    /** Does this period contain given period? */
    def contains(that: Period): Boolean = {
      // Let    this = (r1, l1, d1), that = (r2, l2, d2)
      // where  r = runid, l = last phase, d = duration - 1
      // Then seen as intervals:
      //
      //  this = r1 / (l1 - d1) .. l1
      //  that = r2 / (l2 - d2) .. l2
      //
      // Let's compute:
      //
      //  lastDiff = X * 2^5 + (l1 - l2) mod 2^5
      //             where X >= 0, X == 0 iff r1 == r2 & l1 - l2 >= 0
      //  result = lastDiff + d2 <= d1
      //  We have:
      //      lastDiff + d2 <= d1
      //  iff X == 0 && l1 - l2 >= 0 && l1 - l2 + d2 <= d1
      //  iff r1 == r2 & l1 >= l2 && l1 - d1 <= l2 - d2
      //  q.e.d
      val lastDiff = (code - that.code) >>> PhaseWidth
      lastDiff + (that.code & PhaseMask ) <= (this.code & PhaseMask)
    }

    /** Does this period overlap with given period? */
    def overlaps(that: Period): Boolean =
      this.runId == that.runId &&
      this.firstPhaseId <= that.lastPhaseId &&
      that.firstPhaseId <= this.lastPhaseId

    /** The intersection of two periods */
    def & (that: Period): Period =
      if (this overlaps that)
        Period(
          this.runId,
          this.firstPhaseId max that.firstPhaseId,
          this.lastPhaseId min that.lastPhaseId)
      else
        Nowhere

    /** The smallest period containing two periods */
    def | (that: Period): Period =
      Period(this.runId,
          this.firstPhaseId min that.firstPhaseId,
          this.lastPhaseId max that.lastPhaseId)

    override def toString = s"Period($firstPhaseId..$lastPhaseId, run = $runId)"
  }

  object Period {

    /** The single-phase period consisting of given run id and phase id */
    def apply(rid: RunId, pid: PhaseId): Period = {
      new Period(((rid << PhaseWidth) | pid) << PhaseWidth)
    }

    /** The period consisting of given run id, and lo/hi phase ids */
    def apply(rid: RunId, loPid: PhaseId, hiPid: PhaseId): Period = {
      new Period(((rid << PhaseWidth) | hiPid) << PhaseWidth | (hiPid - loPid))
    }

    /** The interval consisting of all periods of given run id */
    def allInRun(rid: RunId) = {
      apply(rid, 0, PhaseMask)
    }
  }

  final val Nowhere = new Period(0)

  final val InitialPeriod = Period(InitialRunId, FirstPhaseId)

  final val InvalidPeriod = Period(NoRunId, NoPhaseId)

  /** An ordinal number for compiler runs. First run has number 1. */
  type RunId = Int
  final val NoRunId = 0
  final val InitialRunId = 1
  final val RunWidth = java.lang.Integer.SIZE - PhaseWidth * 2 - 1/* sign */
  final val MaxPossibleRunId = (1 << RunWidth) - 1

  /** An ordinal number for phases. First phase has number 1. */
  type PhaseId = Int
  final val NoPhaseId = 0
  final val FirstPhaseId = 1

  /** The number of bits needed to encode a phase identifier. */
  final val PhaseWidth = 7
  final val PhaseMask = (1 << PhaseWidth) - 1
  final val MaxPossiblePhaseId = PhaseMask
}
