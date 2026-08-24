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

package scala
package runtime

import scala.language.`2.13`

// Still need this one since the implicit class ArrayCharSequence only converts
// a single argument.
/** A `CharSequence` view of a slice of an `Array[Char]`.
 *
 *  The sequence consists of the characters of `xs` from index `start` until
 *  `end`. Characters are read from the array on demand, so later writes to
 *  the array are visible through this sequence. The bounds are not validated
 *  on construction: `end <= start` yields an empty sequence, and out-of-range
 *  bounds only surface when characters are accessed.
 *
 *  @param xs the underlying character array
 *  @param start the index in `xs` of the first character of the sequence
 *  @param end the index in `xs` one past the last character of the sequence
 */
final class ArrayCharSequence(val xs: Array[Char], start: Int, end: Int) extends CharSequence {
  // yikes
  // java.lang.VerifyError: (class: scala/runtime/ArrayCharSequence, method: <init> signature: ([C)V)
  //   Constructor must call super() or this()
  //
  // def this(xs: Array[Char]) = this(xs, 0, xs.length)

  /** Returns the number of characters in this sequence: `end - start`, or `0` if `end <= start`. */
  def length: Int = math.max(0, end - start)
  /** Returns the character at the given index of this sequence, that is, the
   *  character at index `start + index` of the underlying array.
   *
   *  @param index the index of the character to return, from `0` to `length - 1`
   *  @throws ArrayIndexOutOfBoundsException if `index` is negative or not less
   *          than `length`, or if the slice this sequence was constructed with
   *          falls outside the array, since those bounds are not validated (the
   *          exception message reports the bounds of the underlying array, not
   *          of this sequence)
   */
  def charAt(index: Int): Char = {
    if (0 <= index && index < length)
      xs(start + index)
    else throw new ArrayIndexOutOfBoundsException(s"$index is out of bounds (min 0, max ${xs.length - 1})")
  }
  /** Returns a new `ArrayCharSequence` over the characters of this sequence
   *  from index `start0` until `end0`.
   *
   *  The result is a view over the same underlying array; no characters are
   *  copied.
   *
   *  @param start0 the index in this sequence of the first character of the subsequence
   *  @param end0 the index in this sequence one past the last character of the subsequence
   *  @return the subsequence view; empty if `end0 <= start0` (no exception is
   *          thrown for an inverted range, unlike the `CharSequence` contract)
   *  @throws ArrayIndexOutOfBoundsException if `start0` is negative or `end0`
   *          is greater than `length`
   */
  def subSequence(start0: Int, end0: Int): CharSequence = {
    if (start0 < 0) throw new ArrayIndexOutOfBoundsException(s"$start0 is out of bounds (min 0, max ${length -1})")
    else if (end0 > length) throw new ArrayIndexOutOfBoundsException(s"$end0 is out of bounds (min 0, max ${xs.length -1})")
    else if (end0 <= start0) new ArrayCharSequence(xs, 0, 0)
    else {
      val newlen = end0 - start0
      val start1 = start + start0
      new ArrayCharSequence(xs, start1, start1 + newlen)
    }
  }
  /** Returns the characters of this sequence as a `String`.
   *
   *  The bounds are clamped to the underlying array before copying: a
   *  negative `start` is treated as `0` and the end is capped at the array's
   *  length, so a sequence constructed with out-of-range bounds yields
   *  characters rather than throwing. The count is taken from the declared
   *  bounds, so a negative `start` shifts the window: `start = -2, end = 5`
   *  copies seven characters from index `0`, not the five in range.
   */
  override def toString() = {
    val start = math.max(this.start, 0)
    val end = math.min(xs.length, start + length)

    if (start >= end) "" else new String(xs, start, end - start)
  }
}
