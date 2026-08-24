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
import scala.collection._

private[collection] class AnyIndexedSeqStepper[A](underlying: collection.IndexedSeqOps[A, AnyConstr, ?], _i0: Int, _iN: Int)
  extends IndexedStepperBase[AnyStepper[A], AnyIndexedSeqStepper[A]](_i0, _iN)
    with AnyStepper[A] {
  /** Returns the element at the current index of the underlying sequence and advances past it.
   *
   *  @throws NoSuchElementException if no elements remain
   */
  def nextStep(): A = if (hasStep) { val j = i0; i0 += 1; underlying(j) } else Stepper.throwNSEE()
  /** Creates a new `AnyIndexedSeqStepper` over the range from `i0` (inclusive) to `half`
   *  (exclusive) of the same underlying sequence.
   *
   *  @param half the ending index (exclusive) of the new stepper's range
   *  @return a stepper over the prefix `[i0, half)`
   */
  protected def semiclone(half: Int): AnyIndexedSeqStepper[A] = new AnyIndexedSeqStepper[A](underlying, i0, half)
}

private[collection] class DoubleIndexedSeqStepper[CC <: collection.IndexedSeqOps[Double, AnyConstr, ?]](underlying: CC, _i0: Int, _iN: Int)
  extends IndexedStepperBase[DoubleStepper, DoubleIndexedSeqStepper[CC]](_i0, _iN)
    with DoubleStepper {
  /** Returns the element at the current index of the underlying sequence and advances past it.
   *
   *  @throws NoSuchElementException if no elements remain
   */
  def nextStep(): Double = if (hasStep) { val j = i0; i0 += 1; underlying(j) } else Stepper.throwNSEE()
  /** Creates a new `DoubleIndexedSeqStepper` over the range from `i0` (inclusive) to `half`
   *  (exclusive) of the same underlying sequence.
   *
   *  @param half the ending index (exclusive) of the new stepper's range
   *  @return a stepper over the prefix `[i0, half)`
   */
  protected def semiclone(half: Int): DoubleIndexedSeqStepper[CC] = new DoubleIndexedSeqStepper[CC](underlying, i0, half)
}

private[collection] class IntIndexedSeqStepper[CC <: collection.IndexedSeqOps[Int, AnyConstr, ?]](underlying: CC, _i0: Int, _iN: Int)
  extends IndexedStepperBase[IntStepper, IntIndexedSeqStepper[CC]](_i0, _iN)
    with IntStepper {
  /** Returns the element at the current index of the underlying sequence and advances past it.
   *
   *  @throws NoSuchElementException if no elements remain
   */
  def nextStep(): Int = if (hasStep) { val j = i0; i0 += 1; underlying(j) } else Stepper.throwNSEE()
  /** Creates a new `IntIndexedSeqStepper` over the range from `i0` (inclusive) to `half`
   *  (exclusive) of the same underlying sequence.
   *
   *  @param half the ending index (exclusive) of the new stepper's range
   *  @return a stepper over the prefix `[i0, half)`
   */
  protected def semiclone(half: Int): IntIndexedSeqStepper[CC] = new IntIndexedSeqStepper[CC](underlying, i0, half)
}

private[collection] class LongIndexedSeqStepper[CC <: collection.IndexedSeqOps[Long, AnyConstr, ?]](underlying: CC, _i0: Int, _iN: Int)
  extends IndexedStepperBase[LongStepper, LongIndexedSeqStepper[CC]](_i0, _iN)
    with LongStepper {
  /** Returns the element at the current index of the underlying sequence and advances past it.
   *
   *  @throws NoSuchElementException if no elements remain
   */
  def nextStep(): Long = if (hasStep) { val j = i0; i0 += 1; underlying(j) } else Stepper.throwNSEE()
  /** Creates a new `LongIndexedSeqStepper` over the range from `i0` (inclusive) to `half`
   *  (exclusive) of the same underlying sequence.
   *
   *  @param half the ending index (exclusive) of the new stepper's range
   *  @return a stepper over the prefix `[i0, half)`
   */
  protected def semiclone(half: Int): LongIndexedSeqStepper[CC] = new LongIndexedSeqStepper[CC](underlying, i0, half)
}
