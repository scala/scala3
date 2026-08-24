/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection.convert
package impl

import scala.language.`2.13`
import scala.collection.Stepper.EfficientSplit
import scala.collection.{BitSetOps, IntStepper, Stepper}


private[collection] final class BitSetStepper(
  @annotation.stableNull
  private var underlying: BitSetOps[?] | Null,
  private var cache0: Long, private var cache1: Long,
  _i0: Int, _iN: Int,
  private var cacheIndex: Int
)
extends InOrderStepperBase[IntStepper, BitSetStepper](_i0, _iN)
with IntStepper {
  import BitSetOps.{WordLength, LogWL}

  // When `found` is set, `i0` is an element that exists
  /** Set when `i0` is known to be the index of a set bit; cleared once that bit is consumed. */
  protected var found: Boolean = false

  /** Advances `i0` to the next set bit at or after its current position, loading further
   *  words from `underlying` into the two-word cache as needed; stops at `iN`.
   *
   *  @return `true` (with `found` set) if a set bit was found before `iN`, else `false`
   */
  @annotation.tailrec
  protected def findNext(): Boolean =
    if (i0 >= iN) false
    else {
      val ix = i0 >> LogWL
      if (ix == cacheIndex || ix == cacheIndex+1) {
        val i = scanLong(if (ix == cacheIndex) cache0 else cache1, i0 & (WordLength - 1))
        if (i >= 0) {
          i0 = (i0 & ~(WordLength - 1)) | i
          found = (i0 < iN)
          found
        }
        else {
          i0 = (i0 & ~(WordLength - 1)) + WordLength
          findNext()
        }
      }
      else if (underlying eq null) {
        i0 = iN
        found = false
        found
      }
      else {
        cacheIndex = ix
        cache0 = underlying.word(cacheIndex)
        cache1 = if ((iN - 1) >> LogWL == ix) -1L else underlying.word(cacheIndex+1)
        findNext()
      }
    }

  /** Creates a stepper over the set bits in `[i0, half)`, transferring the word cache and
   *  the `found` flag to it, and reloading this stepper's cache at the word containing
   *  `half` if that word lies beyond the cached pair.  Called by `trySplit`, which then
   *  advances this stepper's `i0` to `half`.  The new stepper drops its reference to the
   *  underlying bitset if the cached words cover its whole range.
   *
   *  @param half the ending index (exclusive) of the new stepper's range
   *  @return a stepper over the set bits in `[i0, half)`
   *
   *  @note NEEDS-HUMAN: in the branch where `underlying` is non-null and `half` falls
   *        within the cached words, this stepper's `found` flag is not cleared even
   *        though `trySplit` then moves `i0` to `half`; a stale `found` would make
   *        `nextStep` report `half` as a set bit without checking it.  The other two
   *        state-transfer paths do clear `found`.
   */
  def semiclone(half: Int): BitSetStepper =
    if (underlying == null) {
      val ans = new BitSetStepper(null, cache0, cache1, i0, half, cacheIndex)
      ans.found = found
      i0 = half
      found = false
      ans
    }
    else {
      // Set up new stepper
      val ixNewN = (half - 1) >> LogWL
      val ans =
        new BitSetStepper(if (ixNewN <= cacheIndex + 1) null else underlying, cache0, cache1, i0, half, cacheIndex)
      if (found) ans.found = true

      // Advance old stepper to breakpoint
      val ixOld0 = half       >> LogWL
      if (ixOld0 > cacheIndex + 1) {
        cache0 = underlying.word(ixOld0)
        cache1 = if (((iN - 1) >> LogWL) == ixOld0) -1L else underlying.word(ixOld0+1)
        cacheIndex = ixOld0
        i0 = half
        found = false
      }

      // Return new stepper
      ans
    }

  @annotation.tailrec
  private def scanLong(bits: Long, from: Int): Int =
    if (from >= WordLength) -1
    else if ((bits & (1L << from)) != 0) from
    else scanLong(bits, from + 1)

  /** Returns the index of the next set bit and advances past it.
   *
   *  @throws NoSuchElementException if no set bits remain
   */
  def nextStep(): Int =
    if (found || findNext()) {
      found = false
      val ans = i0
      i0 += 1
      ans
    }
    else Stepper.throwNSEE()
}

private[collection] object BitSetStepper {
  /** Creates a stepper over all set bits of the given bitset.
   *
   *  If the bitset fits in at most two words, the words are cached immediately and the
   *  bitset itself is not referenced; otherwise words are loaded into the cache two at
   *  a time as the traversal proceeds.
   *
   *  @param bs the bitset whose set bits to step over
   *  @return a stepper producing the indices of the set bits in increasing order
   */
  def from(bs: scala.collection.BitSetOps[?]): IntStepper & EfficientSplit =
    new BitSetStepper(
      if (bs.nwords <= 2) null else bs,
      if (bs.nwords <= 0) -1L else bs.word(0),
      if (bs.nwords <= 1) -1L else bs.word(1),
      0,
      bs.nwords * BitSetOps.WordLength,
      0
    )
}
