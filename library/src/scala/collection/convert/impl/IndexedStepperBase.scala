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

/** Abstracts all the generic operations of stepping over an indexable collection.
 *
 *  @tparam Sub the concrete stepper subtype
 *  @tparam Semi the type returned by splitting, a subtype of `Sub`
 *  @param i0 the starting index (inclusive) into the underlying collection
 *  @param iN the ending index (exclusive) into the underlying collection
 */
private[convert] abstract class IndexedStepperBase[Sub, Semi <: Sub](protected var i0: Int, protected var iN: Int)
  extends EfficientSplit {
  /** Creates a stepper of the concrete subtype covering the index range from `i0`
   *  (inclusive) to `half` (exclusive).  Called by `trySplit`, which then advances
   *  this stepper past the split-off prefix.
   *
   *  @param half the ending index (exclusive) of the new stepper's range
   *  @return a stepper over the prefix `[i0, half)`
   */
  protected def semiclone(half: Int): Semi

  /** Returns `true` if at least one element remains, i.e. `i0 < iN`. */
  def hasStep: Boolean = i0 < iN

  /** Returns the Java `Spliterator` characteristics: `ORDERED`, `SIZED` and `SUBSIZED`,
   *  since an indexed range has an exact size and splits into halves of exact size.
   */
  def characteristics: Int = Spliterator.ORDERED + Spliterator.SIZED + Spliterator.SUBSIZED

  /** Returns the exact number of elements remaining, `iN - i0`. */
  def estimateSize: Long = iN - i0

  /** Splits this stepper in half, if possible: returns a new stepper over the first half
   *  of the remaining elements, `[i0, half)`, and advances this stepper to cover
   *  `[half, iN)`, where `half` is the midpoint of the remaining range.
   *
   *  @return the stepper over the prefix, or `null` if fewer than two elements remain
   */
  def trySplit(): Sub | Null = {
    if (iN-1 > i0) {
      val half = (i0+iN) >>> 1
      val ans = semiclone(half)
      i0 = half
      ans
    }
    else null
  }
}
