object Periods {
  class Period(val code: Int) extends AnyVal

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


  final val x = y       // error
  final val y: Int = 3
}
