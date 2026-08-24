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
import scala.collection.{AnyStepper, IntStepper, LongStepper, Stepper}
import scala.collection.immutable.NumericRange

private[collection] class AnyNumericRangeStepper[A](underlying: NumericRange[A], _i0: Int, _iN: Int)
extends IndexedStepperBase[AnyStepper[A], AnyNumericRangeStepper[A]](_i0, _iN)
with AnyStepper[A] {
  /** Returns the element at the current index of the underlying range and advances past it.
   *
   *  @throws NoSuchElementException if no elements remain
   */
  def nextStep(): A = if (hasStep) { val j = i0; i0 += 1; underlying(j) } else Stepper.throwNSEE()
  /** Creates a new `AnyNumericRangeStepper` over the indices from `i0` (inclusive) to
   *  `half` (exclusive) into the same underlying range.
   *
   *  @param half the ending index (exclusive) of the new stepper's range
   */
  def semiclone(half: Int) = new AnyNumericRangeStepper[A](underlying, i0, half)
}

private[collection] class IntNumericRangeStepper(underlying: NumericRange[Int], _i0: Int, _iN: Int)
extends IndexedStepperBase[IntStepper, IntNumericRangeStepper](_i0, _iN)
with IntStepper {
  /** Returns the element at the current index of the underlying range and advances past it.
   *
   *  @throws NoSuchElementException if no elements remain
   */
  def nextStep(): Int = if (hasStep) { val j = i0; i0 += 1; underlying(j) } else Stepper.throwNSEE()
  /** Creates a new `IntNumericRangeStepper` over the indices from `i0` (inclusive) to
   *  `half` (exclusive) into the same underlying range.
   *
   *  @param half the ending index (exclusive) of the new stepper's range
   */
  def semiclone(half: Int) = new IntNumericRangeStepper(underlying, i0, half)
}

private[collection] class LongNumericRangeStepper(underlying: NumericRange[Long], _i0: Int, _iN: Int)
extends IndexedStepperBase[LongStepper, LongNumericRangeStepper](_i0, _iN)
with LongStepper {
  /** Returns the element at the current index of the underlying range and advances past it.
   *
   *  @throws NoSuchElementException if no elements remain
   */
  def nextStep(): Long = if (hasStep) { val j = i0; i0 += 1; underlying(j) } else Stepper.throwNSEE()
  /** Creates a new `LongNumericRangeStepper` over the indices from `i0` (inclusive) to
   *  `half` (exclusive) into the same underlying range.
   *
   *  @param half the ending index (exclusive) of the new stepper's range
   */
  def semiclone(half: Int) = new LongNumericRangeStepper(underlying, i0, half)
}
