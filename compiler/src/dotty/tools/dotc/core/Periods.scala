package dotty.tools
package dotc
package core

import Contexts.*
import printing.*
import Texts.*
import Phases.unfusedPhases

object Periods {

  /** The period containing the current period where denotations do not change.
   *  We compute this by taking as first phase the first phase less or equal to
   *  the current phase that has the same "nextTransformerId". As last phase
   *  we take the next transformer id following the current phase.
   */
  def currentStablePeriod(using Context): Period =
    var first = ctx.phaseId
    val nxTrans = ctx.base.nextDenotTransformerId(first)
    while (first - 1 > NoPhaseId && (ctx.base.nextDenotTransformerId(first - 1) == nxTrans))
      first -= 1
    Period(ctx.runId, first, nxTrans)

  /** Are all base types in the current period guaranteed to be the same as in period `p`? */
  def currentHasSameBaseTypesAs(p: Period)(using Context): Boolean =
    val period = ctx.period
    period == p ||
    period.runId == p.runId &&
      unfusedPhases(period.firstPhaseId).sameBaseTypesStartId ==
      unfusedPhases(p.firstPhaseId).sameBaseTypesStartId

  /** A period is a contiguous sequence of phase ids in some run.
   *  It is coded as follows:
   *
   *     sign, always 0:      1 bit
   *     run id:              17 bits
   *     last phase id:       7 bits
   *     #phases before last: 7 bits
   *
   *  This encoding ensures the < and > operators can be delegated as-is to `code`, making them cheap.
   */
  class Period private[Periods] (private val code: Int) extends AnyVal with Showable {

    /** The run identifier of this period. */
    def runId: RunId = code >>> (PhaseWidth * 2)

    /** The last phase of this period */
    def lastPhaseId: PhaseId = (code >>> PhaseWidth) & PhaseMask

    /** The first phase of this period */
    def firstPhaseId: PhaseId = lastPhaseId - (code & PhaseMask)

    def containsPhaseId(id: PhaseId): Boolean = firstPhaseId <= id && id <= lastPhaseId

    /** Does this period contain the given period? */
    def contains(that: Period): Boolean =
      // We want to check (run1 == run2) & (last1 >= last2) & (first1 <= first2).
      // We can do the first two comparisons in one by checking if subtracting code1 from code2 leads to all-zeroes
      // in the sign + run ID bits. If (run1 > run2) or (run2 < run1) or (run1 == run2 and last1 < last2),
      // then (code1 - code2) is either large enough to have some run ID bits set, or negative so the sign bit is set.
      ((this.code - that.code) & (-1 << (PhaseWidth * 2))) == 0 &&
        this.firstPhaseId <= that.firstPhaseId

    /** Does this period overlap with given period? */
    def overlaps(that: Period): Boolean =
      this.runId == that.runId &&
      this.firstPhaseId <= that.lastPhaseId &&
      that.firstPhaseId <= this.lastPhaseId

    /** The intersection of two periods */
    def & (that: Period): Period =
      if this `overlaps` that then
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

    inline def <(that: Period): Boolean =
      this.code < that.code

    inline def >(that: Period): Boolean =
      this.code > that.code

    def toText(p: Printer): Text =
      inContext(p.printerContext):
        this match
          case Nowhere                                           => "Nowhere"
          case InitialPeriod                                     => "InitialPeriod"
          case InvalidPeriod                                     => "InvalidPeriod"
          case Period(NoRunId, FirstPhaseId, MaxPossiblePhaseId) => s"Period(NoRunId.all)"
          case Period(runId, FirstPhaseId, MaxPossiblePhaseId)   => s"Period($runId.all)"
          case Period(runId, p1, pn) if p1 == pn                 => s"Period($runId.$p1(${ctx.base.phases(p1)}))"
          case Period(runId, p1, pn)                             => s"Period($runId.$p1(${ctx.base.phases(p1)})-$pn(${ctx.base.phases(pn)}))"

    override def toString: String = this match
      case Nowhere                                           => "Nowhere"
      case InitialPeriod                                     => "InitialPeriod"
      case InvalidPeriod                                     => "InvalidPeriod"
      case Period(NoRunId, FirstPhaseId, MaxPossiblePhaseId) => s"Period(NoRunId.all)"
      case Period(runId, FirstPhaseId, MaxPossiblePhaseId)   => s"Period($runId.all)"
      case Period(runId, p1, pn) if p1 == pn                 => s"Period($runId.$p1)"
      case Period(runId, p1, pn)                             => s"Period($runId.$p1-$pn)"

    def ==(that: Period): Boolean = this.code == that.code
    def !=(that: Period): Boolean = this.code != that.code
  }

  object Period {

    /** The single-phase period consisting of given run id and phase id */
    def apply(rid: RunId, pid: PhaseId): Period =
      new Period(((rid << PhaseWidth) | pid) << PhaseWidth)

    /** The period consisting of given run id, and lo/hi phase ids */
    def apply(rid: RunId, loPid: PhaseId, hiPid: PhaseId): Period =
      new Period((((rid << PhaseWidth) | hiPid) << PhaseWidth) | (hiPid - loPid))

    /** The interval consisting of all periods of given run id */
    def allInRun(rid: RunId): Period =
      apply(rid, FirstPhaseId, MaxPossiblePhaseId)

    def unapply(p: Period): Extractor = new Extractor(p.code)

    final class Extractor(private val code: Int) extends AnyVal {
      private def p = new Period(code)
      def isEmpty: false = false
      def get: this.type = this
      def _1 = p.runId
      def _2 = p.firstPhaseId
      def _3 = p.lastPhaseId
    }
  }

  /** An ordinal number for compiler runs. First run has number 1. */
  type RunId = Int
  inline val NoRunId = 0
  inline val InitialRunId = 1
  inline val RunWidth = java.lang.Integer.SIZE - PhaseWidth * 2 - 1/* sign */
  inline val MaxPossibleRunId = (1 << RunWidth) - 1

  /** An ordinal number for phases. First phase has number 1. */
  type PhaseId = Int
  inline val NoPhaseId = 0
  inline val FirstPhaseId = 1

  /** The number of bits needed to encode a phase identifier. */
  inline val PhaseWidth = 7
  private inline val PhaseMask = (1 << PhaseWidth) - 1
  inline val MaxPossiblePhaseId = PhaseMask

  private inline val NowhereCode = 0
  final val Nowhere: Period = new Period(NowhereCode)
  final val InitialPeriod: Period = Period(InitialRunId, FirstPhaseId)
  final val InvalidPeriod: Period = Period(NoRunId, NoPhaseId)
}
