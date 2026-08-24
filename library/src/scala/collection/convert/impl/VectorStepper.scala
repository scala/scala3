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

private[convert] abstract class VectorStepperBase[Sub, Semi <: Sub](
  _i0: Int,
  _iN: Int,
  /** The depth of the vector's tree: the number of levels of internal arrays above the
   *  leaf arrays, so 0 means `trunk` is itself the leaf array of elements.
   */
  protected val displayN: Int,
  /** The root array of the vector's tree, holding the elements directly when `displayN == 0`. */
  protected val trunk: Array[AnyRef]
)
extends IndexedStepperBase[Sub, Semi](_i0, _iN) {
  /** The position in `leaves` of the most recently read element; 32 initially and after
   *  a split, forcing `advanceData` to reposition on the next read.
   */
  protected var index: Int = 32  // Force an advanceData on the first element
  /** The leaf array elements are currently being read from. */
  protected var leaves: Array[AnyRef] = compiletime.uninitialized
  /** The position in `twigs` of the current leaf array; 32 forces `advanceData` to
   *  re-descend from the trunk via `initTo`.
   */
  protected var index1: Int = 32 // Force advanceData to defer to initTo on the first element
  /** The array of leaf arrays currently being read from; unused when `displayN == 0`. */
  protected var twigs: Array[AnyRef] = compiletime.uninitialized

  /** Advances to the next leaf array: takes the next leaf from `twigs` if one remains,
   *  otherwise re-descends from the trunk to the position of logical index `iX`.
   *
   *  @param iX the logical index of the next element to read
   */
  protected final def advanceData(iX: Int): Unit = {
    index1 += 1
    if (index1 >= 32) initTo(iX)
    else {
      leaves = twigs(index1).asInstanceOf[Array[AnyRef]]
      index = 0
    }
  }
  /** Descends from `trunk` to set the cached arrays and indices so that `leaves(index)`
   *  is the element at logical index `iX`.
   *
   *  @param iX the logical index to position at
   */
  protected final def initTo(iX: Int): Unit = displayN match {
    case 0 =>
      leaves = trunk
      index = iX
    case 1 =>
      twigs = trunk
      index1 = iX >>> 5
      leaves = twigs(index1).asInstanceOf[Array[AnyRef]]
      index = iX & 0x1F
    case _ =>
      var n = displayN
      var dataN = trunk
      while (n > 2) {
        dataN = dataN((iX >> (5*n)) & 0x1F).asInstanceOf[Array[AnyRef]]
        n -= 1
      }
      twigs = dataN((iX >>> 10) & 0x1F).asInstanceOf[Array[AnyRef]]
      index1 = (iX >> 5) & 0x1F
      leaves = twigs(index1).asInstanceOf[Array[AnyRef]]
      index = iX & 0x1F
  }
}

private[collection] class AnyVectorStepper[A](_i0: Int, _iN: Int, _displayN: Int, _trunk: Array[AnyRef])
extends VectorStepperBase[AnyStepper[A], AnyVectorStepper[A]](_i0, _iN, _displayN, _trunk)
with AnyStepper[A] {
  /** Returns the element at the current index and advances past it, moving to the next
   *  leaf array as needed.
   *
   *  @throws NoSuchElementException if no elements remain
   */
  def nextStep(): A = if (hasStep) {
    index += 1
    if (index >= 32) advanceData(i0)
    i0 += 1
    leaves(index).asInstanceOf[A]
  } else Stepper.throwNSEE()
  /** Creates a new `AnyVectorStepper` over the range from `i0` (inclusive) to `half`
   *  (exclusive) of the same tree, and resets this stepper's cached position (`index`
   *  and `index1` to 32) so it re-descends at its new starting index on the next read.
   *
   *  @param half the ending index (exclusive) of the new stepper's range
   *  @return a stepper over the prefix `[i0, half)`
   */
  def semiclone(half: Int): AnyVectorStepper[A] = {
    val ans = new AnyVectorStepper[A](i0, half, displayN, trunk)
    index = 32
    index1 = 32
    i0 = half
    ans
  }
}

private[collection] class DoubleVectorStepper(_i0: Int, _iN: Int, _displayN: Int, _trunk: Array[AnyRef])
extends VectorStepperBase[DoubleStepper, DoubleVectorStepper](_i0, _iN, _displayN, _trunk)
with DoubleStepper {
  /** Returns the element at the current index and advances past it, moving to the next
   *  leaf array as needed.
   *
   *  @throws NoSuchElementException if no elements remain
   */
  def nextStep(): Double = if (hasStep) {
    index += 1
    if (index >= 32) advanceData(i0)
    i0 += 1
    leaves(index).asInstanceOf[Double]
  } else Stepper.throwNSEE()
  /** Creates a new `DoubleVectorStepper` over the range from `i0` (inclusive) to `half`
   *  (exclusive) of the same tree, and resets this stepper's cached position (`index`
   *  and `index1` to 32) so it re-descends at its new starting index on the next read.
   *
   *  @param half the ending index (exclusive) of the new stepper's range
   *  @return a stepper over the prefix `[i0, half)`
   */
  def semiclone(half: Int): DoubleVectorStepper = {
    val ans = new DoubleVectorStepper(i0, half, displayN, trunk)
    index = 32
    index1 = 32
    i0 = half
    ans
  }
}

private[collection] class IntVectorStepper(_i0: Int, _iN: Int, _displayN: Int, _trunk: Array[AnyRef])
extends VectorStepperBase[IntStepper, IntVectorStepper](_i0, _iN, _displayN, _trunk)
with IntStepper {
  /** Returns the element at the current index and advances past it, moving to the next
   *  leaf array as needed.
   *
   *  @throws NoSuchElementException if no elements remain
   */
  def nextStep(): Int = if (hasStep) {
    index += 1
    if (index >= 32) advanceData(i0)
    i0 += 1
    leaves(index).asInstanceOf[Int]
  } else Stepper.throwNSEE()
  /** Creates a new `IntVectorStepper` over the range from `i0` (inclusive) to `half`
   *  (exclusive) of the same tree, and resets this stepper's cached position (`index`
   *  and `index1` to 32) so it re-descends at its new starting index on the next read.
   *
   *  @param half the ending index (exclusive) of the new stepper's range
   *  @return a stepper over the prefix `[i0, half)`
   */
  def semiclone(half: Int): IntVectorStepper = {
    val ans = new IntVectorStepper(i0, half, displayN, trunk)
    index = 32
    index1 = 32
    i0 = half
    ans
  }
}

private[collection] class LongVectorStepper(_i0: Int, _iN: Int, _displayN: Int, _trunk: Array[AnyRef])
extends VectorStepperBase[LongStepper, LongVectorStepper](_i0, _iN, _displayN, _trunk)
with LongStepper {
  /** Returns the element at the current index and advances past it, moving to the next
   *  leaf array as needed.
   *
   *  @throws NoSuchElementException if no elements remain
   */
  def nextStep(): Long = if (hasStep) {
    index += 1
    if (index >= 32) advanceData(i0)
    i0 += 1
    leaves(index).asInstanceOf[Long]
  } else Stepper.throwNSEE()
  /** Creates a new `LongVectorStepper` over the range from `i0` (inclusive) to `half`
   *  (exclusive) of the same tree, and resets this stepper's cached position (`index`
   *  and `index1` to 32) so it re-descends at its new starting index on the next read.
   *
   *  @param half the ending index (exclusive) of the new stepper's range
   *  @return a stepper over the prefix `[i0, half)`
   */
  def semiclone(half: Int): LongVectorStepper = {
    val ans = new LongVectorStepper(i0, half, displayN, trunk)
    index = 32
    index1 = 32
    i0 = half
    ans
  }
}
