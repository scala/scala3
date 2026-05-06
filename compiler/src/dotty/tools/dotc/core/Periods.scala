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
      unfusedPhases(period.lastPhaseId).sameBaseTypesStartId ==
      unfusedPhases(p.lastPhaseId).sameBaseTypesStartId

  /** A period is a contiguous sequence of phase ids in some run.
   *  It is coded as follows:
   *
   *     sign, always 0:  1 bit
   *     run id:          17 bits
   *     last phase id:   7 bits
   *     duration:        7 bits (a.k.a. #phases before last)
   *
   *  This encoding ensures the < and > operators can be delegated as-is to `code`, making them cheap.
   */
  class Period private[Periods] (private val code: Int) extends AnyVal with Showable {
    /* VERIFYING CORRECTNESS:
     * `Period` is very performance sensitive, so low-level tricks are useful, but they have to be correct.
     * To avoid introducing incorrect "optimizations", use your favorite SMT solver to check them.
     * You can model two Periods using the current encoding with the following:
       ; (r, l, d) components
       (declare-fun r1 () (_ BitVec 17))
       (declare-fun l1 () (_ BitVec 7))
       (declare-fun d1 () (_ BitVec 7))
       (declare-fun r2 () (_ BitVec 17))
       (declare-fun l2 () (_ BitVec 7))
       (declare-fun d2 () (_ BitVec 7))
       ; assume d <= l
       (assert (bvule d1 l1))
       (assert (bvule d2 l2))
       ; assume l < 64
       (assert (bvult l1 #b1000000))
       (assert (bvult l2 #b1000000))
       ; assume r > 0
       (assert (bvult #b00000000000000000 r1))
       (assert (bvult #b00000000000000000 r2))
       ; full code, including 0 as sign bit
       (declare-fun code1 () (_ BitVec 32))
       (assert (= code1 (concat #b0 r1 l1 d1)))
       (declare-fun code2 () (_ BitVec 32))
       (assert (= code2 (concat #b0 r2 l2 d2)))
     */

    /** The run identifier of this period. */
    def runId: RunId = code >>> (PhaseWidth * 2)

    /** The last phase of this period */
    def lastPhaseId: PhaseId = (code >>> PhaseWidth) & PhaseMask

    /** The first phase of this period */
    def firstPhaseId: PhaseId =
      /* More obvious version with one more bitmask: (lastPhaseId - (code & PhaseMask))
         SMT proof, given the definitions above:
           (declare-fun lastPhaseId () (_ BitVec 32))
           (assert (= lastPhaseId (bvand (bvlshr code1 #x00000007) #x0000007F)))
           (declare-fun simple () (_ BitVec 32))
           (assert (= simple (bvsub lastPhaseId (bvand code1 #x0000007F))))
           (declare-fun opt () (_ BitVec 32))
           (assert (= opt (bvand (bvsub (bvashr code1 #x00000007) code1) #x0000007F)))
           (assert (not (= opt simple)))
         This returns false for (check-sat), thus `opt` is always equal to `simple`.
       */
      ((code >>> PhaseWidth) - code) & PhaseMask

    def containsPhaseId(id: PhaseId): Boolean =
      /* More obvious version: firstPhaseId <= id && id <= lastPhaseId
         SMT proof, given the definitions above:
           (declare-fun lastPhaseId () (_ BitVec 32))
           (assert (= lastPhaseId (bvand (bvlshr code1 #x00000007) #x0000007F)))
           (declare-fun firstPhaseId () (_ BitVec 32))
           (assert (= firstPhaseId (bvsub lastPhaseId (bvand code1 #x0000007F))))
           (declare-fun diff () (_ BitVec 32))
           (assert (= diff (bvand code1 #x0000007F)))
           (declare-fun id () (_ BitVec 32))
           (assert (bvule id #x0000007F))
           (assert (not (= (and (bvsle firstPhaseId id) (bvsle id lastPhaseId))
                           (bvsle (bvxor (bvsub (bvadd id diff) lastPhaseId) #x80000000) (bvxor diff #x80000000)))))
         This returns false for (check-sat), thus the simple and complex versions are equivalent.
         Reasoning:
          if a <= b, then a <= x && x <= b is equivalent to (x - a) unsigned_<= (b - a). Here a == firstPhaseId == lastP - diff and b == lastP.
          So (x - a) == x - (lastP - diff) == x + diff - lastP
             (b - a) == lastP - (lastP - diff) == diff
          With (x ^ MinValue) <= (y ^ MinValue) as the encoding of x unsigned_<= y, then:
      */
      val diff = code & PhaseMask
      ((id + diff - lastPhaseId) ^ Int.MinValue) <= (diff ^ Int.MinValue)

    /** Same as `containsPhaseId`, except the first phase of this period isn't included in the range being tested */
    def containsPhaseIdNotFirst(id: PhaseId): Boolean =
      firstPhaseId < id && id <= lastPhaseId // not yet optimized

    /** Does this period contain the given period? i.e., (run1 == run2) & (last1 >= last2) & (first1 <= first2) */
    def contains(that: Period): Boolean =
      /* More obvious version: (run1 == run2) & (last1 >= last2) & ((last1 - duration1) <= (last2 - duration2))
         But we can transform each of these conditions to get a branchless version. SMT proof, given the definitions above:
           (declare-fun simple () (_ Bool))
           (assert (= simple (and (= r1 r2) (bvuge l1 l2) (bvule (bvsub l1 d1) (bvsub l2 d2)))))
           (declare-fun firstPhaseId1 () (_ BitVec 32))
           (assert (= firstPhaseId1 ((_ zero_extend 25) (bvsub l1 d1))))
           (declare-fun firstPhaseId2 () (_ BitVec 32))
           (assert (= firstPhaseId2 ((_ zero_extend 25) (bvsub l2 d2))))
           (declare-fun lastPhaseId1 () (_ BitVec 32))
           (assert (= lastPhaseId1 ((_ zero_extend 25) l1)))
           (declare-fun lastPhaseId2 () (_ BitVec 32))
           (assert (= lastPhaseId2 ((_ zero_extend 25) l2)))
           (declare-fun opt () (_ Bool))
           (assert (= opt (= #x00000000 (bvor (bvand (bvxor code1 code2) #xFFFFC000) (bvand (bvor (bvsub firstPhaseId2 firstPhaseId1) (bvsub lastPhaseId1 lastPhaseId2)) #x80000000)))))
           (assert (not (= simple opt)))
         This returns false for (check-sat), thus the simple and complex versions are equivalent.
       */
      (((this.code ^ that.code) & (-1 << (PhaseWidth * 2))) | (((that.firstPhaseId - this.firstPhaseId) | (this.lastPhaseId - that.lastPhaseId)) & Int.MinValue)) == 0

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
          case Period(NoRunId, FirstPhaseId, MaxPossiblePhaseId) => s"Period(NoRunId.all)"
          case Period(runId, FirstPhaseId, MaxPossiblePhaseId)   => s"Period($runId.all)"
          case Period(runId, p1, pn) if p1 == pn                 => s"Period($runId.$p1(${ctx.base.phases(p1)}))"
          case Period(runId, p1, pn)                             => s"Period($runId.$p1(${ctx.base.phases(p1)})-$pn(${ctx.base.phases(pn)}))"

    override def toString: String = this match
      case Nowhere                                           => "Nowhere"
      case InitialPeriod                                     => "InitialPeriod"
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
}
