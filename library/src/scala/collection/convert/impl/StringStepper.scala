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
import java.lang.Character.{charCount, isLowSurrogate}
import java.util.Spliterator

import scala.collection.Stepper.EfficientSplit
import scala.collection.{IntStepper, Stepper}

/** Implements `Stepper` on a `String` where you step through chars packed into `Int`.
 *
 *  @param underlying the `String` to step through
 *  @param _i0 the starting char index (inclusive) into the string
 *  @param _iN the ending char index (exclusive) into the string
 */
private[collection] final class CharStringStepper(underlying: String, _i0: Int, _iN: Int)
extends IndexedStepperBase[IntStepper, CharStringStepper](_i0, _iN)
with IntStepper {
  /** Returns the char at the current index, widened to `Int`, and advances past it.
   *
   *  @throws NoSuchElementException if no chars remain
   */
  def nextStep(): Int =
    if (hasStep) { val j = i0; i0 += 1; underlying.charAt(j) }
    else Stepper.throwNSEE()

  /** Creates a new `CharStringStepper` over the range from `i0` (inclusive) to `half`
   *  (exclusive) of the same string.
   *
   *  @param half the ending index (exclusive) of the new stepper's range
   *  @return a stepper over the prefix `[i0, half)`
   */
  def semiclone(half: Int): CharStringStepper = new CharStringStepper(underlying, i0, half)
}

/** Implements `Stepper` on a `String` where you step through code points.
 *
 *  @param underlying the `String` to step through by code point
 *  @param i0 the starting char index (inclusive) into the string
 *  @param iN the ending char index (exclusive) into the string
 */
private[collection] final class CodePointStringStepper(underlying: String, private var i0: Int, private var iN: Int)
extends IntStepper with EfficientSplit {
  /** Returns the Java `Spliterator` characteristics: `IMMUTABLE`, `NONNULL` and `ORDERED`,
   *  but not `SIZED`, since surrogate pairs make the number of code points inexact.
   */
  def characteristics: Int = Spliterator.IMMUTABLE | Spliterator.NONNULL | Spliterator.ORDERED
  /** Returns `iN - i0`, the number of chars remaining, an upper bound on the number of
   *  code points remaining.
   */
  def estimateSize: Long = iN - i0
  /** Returns `true` if at least one char (and therefore at least one code point) remains. */
  def hasStep: Boolean = i0 < iN
  /** Returns the code point at the current position and advances past it: by two chars
   *  for a surrogate pair, by one char otherwise.
   *
   *  @throws NoSuchElementException if no chars remain
   */
  def nextStep(): Int = {
    if (hasStep) {
      val cp = underlying.codePointAt(i0)
      i0 += charCount(cp)
      cp
    }
    else Stepper.throwNSEE()
  }
  /** Splits this stepper near the midpoint of the remaining chars: returns a new stepper
   *  over the prefix and advances this stepper past it.  The split point backs up one
   *  char if it would fall on a low surrogate, so no surrogate pair is divided between
   *  the two steppers.
   *
   *  @return the stepper over the prefix, or `null` if fewer than four chars remain
   */
  def trySplit(): CodePointStringStepper | Null =
    if (iN - 3 > i0) {
      var half = (i0 + iN) >>> 1
      if (isLowSurrogate(underlying.charAt(half))) half -= 1
      val ans = new CodePointStringStepper(underlying, i0, half)
      i0 = half
      ans
    }
    else null
}
