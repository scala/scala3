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
  def phaseId = phaseIdOf(period)

  /** The current run identifier */
  def runId = runIdOf(period)

  /** A new context that differs from the current one in its period */
  def withPeriod(pd: Period): Context =
    if (period == pd) this
    else new SubContext(self) {
      override val period = pd
    }

  /** Execute `op` at given period */
  def atPeriod[T](pd: Period)(op: Context => T)(implicit ctx: Context) =
    op(ctx withPeriod pd)

  /** Execute `op` at given phase id */
  def atPhase[T](pid: PhaseId)(op: Context => T)(implicit ctx: Context) =
    op(ctx withPeriod periodOf(period, pid))
}

object Periods {

  /** A period is an ordinal number for a phase in a run.
   *  Phases in later runs have higher periods than phases in earlier runs.
   *  Later phases have higher periods than earlier phases in the same run.
   *  Periods are coded (in big endian) as:
   *
   *     sign, always 0        1 bit
   *     runid                21 bits
   *     phase id:             5 bits
   *     unused:               5 bits
   */
  type Period = Int
  final val NoPeriod = 0

  /** A period interval is an interval between two periods that share the same runid.
   *  It is coded as follows:
   *
   *     sign, always 0        1 bit
   *     runid                21 bits
   *     last phase id:        5 bits
   *     #phases before last:  5 bits
   */
  type Interval = Int
  final val Nowhere = NoPeriod

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

  /** The run identifier of the given period. */
  final def runIdOf(period: Period): RunId = period >>> (PhaseWidth * 2)

  /** The phase identifier of the given period. */
  final def phaseIdOf(period: Period): PhaseId = (period >>> PhaseWidth) & PhaseMask

  /** The last phase of the given interval */
  final def lastPhaseIdOf(ivl: Interval) = phaseIdOf(ivl)

  /** The first phase of the given interval */
  final def firstPhaseIdOf(ivl: Interval) = lastPhaseIdOf(ivl) - (ivl & PhaseMask)

  /** Does given interval contain given period */
  final def containsPeriod(ivl: Interval, period: Period): Boolean =
    ((ivl - period) >>> PhaseWidth) <= (ivl & PhaseMask)

  final def intervalsOverlap(ivl1: Interval, ivl2: Interval): Boolean = ???

 /** The period consisting of given run id and phase id */
  final def periodOf(rid: RunId, pid: PhaseId): Period =
    ((rid << PhaseWidth) | pid) << PhaseWidth

  /** The interval consisting of given run id, and lo/hi phase ids */
  final def intervalOf(rid: RunId, loPid: PhaseId, hiPid: PhaseId): Interval =
    periodOf(rid, hiPid) | (hiPid - loPid)

  /** The interval consisting of all periods of given run id */
  def allPeriods(rid: RunId): Interval = intervalOf(rid, 0, PhaseMask)

}