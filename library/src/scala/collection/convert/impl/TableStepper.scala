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
import scala.collection._

private[collection] abstract class TableStepperBase[A, I <: AnyRef, Sub, Semi <: Sub & TableStepperBase[A, I, ?, ?]](
  /** An upper bound on the number of elements remaining; `table` is the array of hash
   *  buckets, of which this stepper covers the indices from `i0` (inclusive) to `iN`
   *  (exclusive).
   */
  protected var maxLength: Int, protected val table: Array[I | Null], protected var i0: Int, protected val iN: Int
)
extends EfficientSplit {
  // Always holds table(i0); if `null` it is time to switch to the next element
  /** The entry currently being visited, or `null` when the current bucket's chain is
   *  exhausted and the next non-empty bucket must be found.
   */
  protected var myCurrent: I | Null = if (i0 < iN) table(i0) else null

  // Only call this when `myCurrent` is null (meaning we need to advance)
  /** Advances `i0` to the next non-empty bucket and sets `myCurrent` to its first entry.
   *
   *  @return `true` if a non-empty bucket was found before `iN`, else `false`
   */
  @annotation.tailrec
  protected final def findNextCurrent(): Boolean =
    if (i0 < iN) {
      i0 += 1
      if (i0 >= iN) false
      else {
        myCurrent = table(i0)
        if (myCurrent eq null) findNextCurrent()
        else true
      }
    }
    else false

  /** Creates a stepper of the concrete subtype covering the buckets from `i0` (inclusive)
   *  to `half` (exclusive) of the same table.  Called by `trySplit`, which afterwards
   *  fixes up the current entry and the `maxLength` bounds of both steppers.
   *
   *  @param half the ending bucket index (exclusive) of the new stepper's range
   *  @return a stepper over the buckets `[i0, half)`
   */
  protected def semiclone(half: Int): Semi

  /** Returns no Java `Spliterator` characteristics: hash-table order is not meaningful
   *  and the remaining count is only an upper bound.
   */
  def characteristics: Int = 0

  /** Returns `maxLength`, an upper bound on the number of elements remaining, first
   *  setting it to 0 if no elements remain.
   */
  def estimateSize: Long = if (!hasStep) { maxLength = 0; 0 } else maxLength

  /** Returns `true` if an entry is at hand or a further non-empty bucket exists before `iN`. */
  def hasStep: Boolean = (myCurrent ne null) || findNextCurrent()

  /** Splits the remaining buckets in half, if possible: returns a new stepper over the
   *  buckets `[i0, half)`, including the entry currently being visited, and advances this
   *  stepper to `[half, iN)`.  Each side's `maxLength` bound is reduced by a count of
   *  entries known to fall on the other side (for ranges of fewer than 32 buckets, a run
   *  of consecutive non-empty buckets on each side of the split point is counted).
   *
   *  @return the stepper over the bucket prefix, or `null` if fewer than two buckets
   *          remain or the element bound has reached 0
   */
  def trySplit(): Sub | Null = {
    if (iN-1 > i0 && maxLength > 0) {
      val half = (i0 + iN) >>> 1
      val ans = semiclone(half)
      ans.myCurrent = myCurrent
      myCurrent = table(half)
      var inLeft = if (ans.myCurrent ne null) 1 else 0
      var inRight = if (myCurrent ne null) 1 else 0
      if (iN - i0 < 32) {
        var i = i0+1
        while (i < half && (table(i) ne null)) { i += 1; inLeft += 1 }
        i = half+1
        while (i < iN && (table(i) ne null)) { i += 1; inRight += 1 }
      }
      maxLength -= inLeft
      ans.maxLength -= inRight
      i0 = half
      ans
    }
    else null
  }
}


private[collection] final class AnyTableStepper[A, I <: AnyRef](
  _maxLength: Int, _table: Array[I | Null], iterate: I => I | Null, extract: I => A, _i0: Int, _iN: Int
)
extends TableStepperBase[A, I, AnyStepper[A], AnyTableStepper[A, I]](_maxLength, _table, _i0, _iN)
with AnyStepper[A] {
  /** Returns the element extracted from the current entry and advances to the next entry
   *  in the bucket's chain via `iterate`.
   *
   *  @throws NoSuchElementException if no elements remain
   */
  def nextStep(): A =
    if (hasStep) {
      val ans = extract(myCurrent.nn)
      myCurrent = iterate(myCurrent.nn)
      ans
    }
    else Stepper.throwNSEE()

  /** Creates a new `AnyTableStepper` over the buckets `[i0, half)` of the same table,
   *  with the same `iterate` and `extract` functions and this stepper's current
   *  `maxLength` bound.
   *
   *  @param half the ending bucket index (exclusive) of the new stepper's range
   *  @return a stepper over the buckets `[i0, half)`
   */
  def semiclone(half: Int): AnyTableStepper[A, I] = new AnyTableStepper[A, I](maxLength, table, iterate, extract, i0, half)
}


private[collection] final class DoubleTableStepper[I <: AnyRef](
  _maxLength: Int, _table: Array[I | Null], iterate: I => I | Null, extract: I => Double, _i0: Int, _iN: Int
)
extends TableStepperBase[Double, I, DoubleStepper, DoubleTableStepper[I]](_maxLength, _table, _i0, _iN)
with DoubleStepper {
  /** Returns the element extracted from the current entry and advances to the next entry
   *  in the bucket's chain via `iterate`.
   *
   *  @throws NoSuchElementException if no elements remain
   */
  def nextStep(): Double =
    if (hasStep) {
      val ans = extract(myCurrent.nn)
      myCurrent = iterate(myCurrent.nn)
      ans
    }
    else Stepper.throwNSEE()

  /** Creates a new `DoubleTableStepper` over the buckets `[i0, half)` of the same table,
   *  with the same `iterate` and `extract` functions and this stepper's current
   *  `maxLength` bound.
   *
   *  @param half the ending bucket index (exclusive) of the new stepper's range
   *  @return a stepper over the buckets `[i0, half)`
   */
  def semiclone(half: Int): DoubleTableStepper[I] = new DoubleTableStepper[I](maxLength, table, iterate, extract, i0, half)
}


private[collection] final class IntTableStepper[I <: AnyRef](
  _maxLength: Int, _table: Array[I | Null], iterate: I => I | Null, extract: I => Int, _i0: Int, _iN: Int
)
extends TableStepperBase[Int, I, IntStepper, IntTableStepper[I]](_maxLength, _table, _i0, _iN)
with IntStepper {
  /** Returns the element extracted from the current entry and advances to the next entry
   *  in the bucket's chain via `iterate`.
   *
   *  @throws NoSuchElementException if no elements remain
   */
  def nextStep(): Int =
    if (hasStep) {
      val ans = extract(myCurrent.nn)
      myCurrent = iterate(myCurrent.nn)
      ans
    }
    else Stepper.throwNSEE()

  /** Creates a new `IntTableStepper` over the buckets `[i0, half)` of the same table,
   *  with the same `iterate` and `extract` functions and this stepper's current
   *  `maxLength` bound.
   *
   *  @param half the ending bucket index (exclusive) of the new stepper's range
   *  @return a stepper over the buckets `[i0, half)`
   */
  def semiclone(half: Int): IntTableStepper[I] = new IntTableStepper[I](maxLength, table, iterate, extract, i0, half)
}


private[collection] final class LongTableStepper[I <: AnyRef](
  _maxLength: Int, _table: Array[I | Null], iterate: I => I | Null, extract: I => Long, _i0: Int, _iN: Int
)
extends TableStepperBase[Long, I, LongStepper, LongTableStepper[I]](_maxLength, _table, _i0, _iN)
with LongStepper {
  /** Returns the element extracted from the current entry and advances to the next entry
   *  in the bucket's chain via `iterate`.
   *
   *  @throws NoSuchElementException if no elements remain
   */
  def nextStep(): Long =
    if (hasStep) {
      val ans = extract(myCurrent.nn)
      myCurrent = iterate(myCurrent.nn)
      ans
    }
    else Stepper.throwNSEE()

  /** Creates a new `LongTableStepper` over the buckets `[i0, half)` of the same table,
   *  with the same `iterate` and `extract` functions and this stepper's current
   *  `maxLength` bound.
   *
   *  @param half the ending bucket index (exclusive) of the new stepper's range
   *  @return a stepper over the buckets `[i0, half)`
   */
  def semiclone(half: Int): LongTableStepper[I] = new LongTableStepper[I](maxLength, table, iterate, extract, i0, half)
}

