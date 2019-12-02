package dotty.tools.dotc.util

object Util {

  /** The index `i` in `candidates.indices` such that `candidates(i) <= x` and
   *  `candidates(i)` is closest to `x`, determined by binary search, or -1
   *  if `x < candidates(0)`.
   *  @param  hint   If between 0 and `candidates.length` use this
   *                 as the first search point, otherwise use
   *                 `candidates.length/2`.
   *  @pre   candidates is sorted
   */
  def bestFit(candidates: Array[Int], length: Int, x: Int, hint: Int = -1): Int = {
    def recur(lo: Int, hi: Int, mid: Int): Int =
      if (x < candidates(mid))
        recur(lo, mid - 1, (lo + mid - 1) / 2)
      else if (mid + 1 < length && x >= candidates(mid + 1))
        recur(mid + 1, hi, (mid + 1 + hi) / 2)
      else mid
    val initMid = if (0 <= hint && hint < length) hint else length / 2
    if (length == 0 || x < candidates(0)) -1
    else recur(0, length, initMid)
  }

}
