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
import java.util.Spliterator

import scala.collection.Stepper.EfficientSplit

/** Abstracts all the generic operations of stepping over a collection
 *  that has an indexable ordering but may have gaps.
 *
 *  For collections that are guaranteed to not have gaps, use `IndexedStepperBase` instead.
 *
 *  @tparam Sub the concrete stepper subtype, used as the self-type and return type of `trySplit`
 *  @tparam Semi the concrete type of the split-off stepper half, constrained to be a subtype of `Sub`
 *  @param i0 the starting index (inclusive) of the range to step over
 *  @param iN the ending index (exclusive) of the range to step over
 */
private[convert] abstract class InOrderStepperBase[Sub, Semi <: Sub](protected var i0: Int, protected var iN: Int)
extends EfficientSplit {
  /** Sets `true` if the element at `i0` is known to be there.  `false` if either not known or is a gap. */
  protected def found: Boolean

  /** Advance `i0` over any gaps, updating internal state so `found` is correct at the new position.
   *  Returns the new value of `found`.
   */
  protected def findNext(): Boolean

  /** Creates a stepper of the concrete subtype covering the index range from `i0`
   *  (inclusive) to `half` (exclusive), carrying over whatever internal state the
   *  subclass needs.  Called by `trySplit`, which then advances this stepper's
   *  `i0` to `half`.
   *
   *  @param half the ending index (exclusive) of the new stepper's range
   *  @return a stepper over the prefix `[i0, half)`
   */
  protected def semiclone(half: Int): Semi

  /** Returns `true` if at least one element remains, advancing `i0` past any gaps to find it. */
  final def hasStep: Boolean = found || findNext()

  /** Returns the Java `Spliterator` characteristics: `ORDERED` only, since gaps make
   *  the number of remaining elements inexact.
   */
  def characteristics: Int = Spliterator.ORDERED

  /** Returns `iN - i0`, an upper bound on the number of elements remaining (gaps may
   *  make the true count smaller).
   */
  def estimateSize: Long = iN - i0

  /** Splits the remaining index range in half, if possible: returns a new stepper over
   *  the first half, `[i0, half)`, and advances this stepper to cover `[half, iN)`.
   *  Because of gaps, the two halves may hold different numbers of elements.
   *
   *  @return the stepper over the prefix, or `null` if fewer than two indices remain
   */
  def trySplit(): Sub | Null = {
    if (iN-1 > i0) {
      val half = (i0 + iN) >>> 1
      val ans = semiclone(half)
      i0 = half
      ans
    }
    else null
  }
}
