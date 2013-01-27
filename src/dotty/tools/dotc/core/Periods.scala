package dotty.tools.dotc.core

import Contexts._

/** Periods are the central "clock" of the compiler.
 *  A period consists of a run id and a phase id.
 *  run ids represent compiler runs
 *  phase ids represent compiler phases
 */
abstract class Periods { self: Context =>
  import Periods._

  /** The current phase identifier */
  def phaseId = period.phaseId

  /** The current run identifier */
  def runId = period.runId

  /** A new context that differs from the current one in its period */
  def withPeriod(pd: Period): Context =
    if (period == pd) this
    else new SubContext(self) {
      override val period = pd
    }

  /** A new context that differs from the current one in its phase */
  def withPhase(pid: PhaseId): Context = withPeriod(Period(runId, pid))

  /** Execute `op` at given period */
  def atPeriod[T](pd: Period)(op: Context => T) =
    op(ctx withPeriod pd)

  /** Execute `op` at given phase id */
  def atPhase[T](pid: PhaseId)(op: Context => T) =
    op(ctx withPhase pid)
}

object Periods {

  /** A period is represented by an ordinal number for a phase in a run.
   *  Phases in later runs have higher periods than phases in earlier runs.
   *  Later phases have higher periods than earlier phases in the same run.
   *  Periods are coded (in big endian) as:
   *
   *     sign, always 0        1 bit
   *     runid                21 bits
   *     phase id:             5 bits
   *     unused:               5 bits
   *
   *  A period interval is an interval between two periods that share the same runid.
   *  It is coded as follows:
   *
   *     sign, always 0        1 bit
   *     runid                21 bits
   *     last phase id:        5 bits
   *     #phases before last:  5 bits
   */
  class Period(val code: Int) extends AnyVal {

    /** The run identifier of this period. */
    def runId: Int = code >>> (PhaseWidth * 2)

    /** The phase identifier of this single-phase period. */
    def phaseId: Int = {
      assert((code & PhaseMask) == 0)
      (code >>> PhaseWidth) & PhaseMask
    }

    /** The last phase of this period */
    def lastPhaseId: Int =
      (code >>> PhaseWidth) & PhaseMask

    /** The first phase of this period */
    def firstPhaseId = lastPhaseId - (code & PhaseMask)

    /** Does this period contain given period?
     *  this = A .. B
     */
    def contains(that: Period): Boolean = {
      val lastDiff = (code - that.code) >>> PhaseWidth
      lastDiff + (that.code & PhaseMask ) <= (this.code & PhaseMask)
    }

    /** Does this period overlpa with given period? */
    def overlaps(that: Period): Boolean =
      this.runId == that.runId &&
      this.firstPhaseId <= that.lastPhaseId &&
      that.firstPhaseId <= this.lastPhaseId

    def & (that: Period): Period =
      if (this overlaps that)
        Period(
          this.runId,
          this.firstPhaseId max that.firstPhaseId,
          this.lastPhaseId min that.lastPhaseId)
      else
        Nowhere
  }

  object Period {

    /** The single-phase period consisting of given run id and phase id */
    def apply(rid: RunId, pid: PhaseId): Period =
      new Period(((rid << PhaseWidth) | pid) << PhaseWidth)

    /** The period consisting of given run id, and lo/hi phase ids */
    def apply(rid: RunId, loPid: PhaseId, hiPid: PhaseId): Period =
      new Period(((rid << PhaseWidth) | hiPid) << PhaseWidth | (hiPid - loPid))

    /** The interval consisting of all periods of given run id */
    def allInRun(rid: RunId) =
      apply(rid, 0, PhaseMask)

  }

  final val Nowhere = new Period(0)

  /** An ordinal number for compiler runs. First run has number 1. */
  type RunId = Int
  final val NoRunId = 0

  /** An ordinal number for phases. First phase has number 1. */
  type PhaseId = Int
  final val NoPhaseId = 0
  final val FirstPhaseId = 1

  /** The number of bits needed to encode a phase identifier. */
  final val PhaseWidth = 5
  final val PhaseMask = (1 << PhaseWidth) - 1

}