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
import scala.collection.immutable.Range

// Note that this does not implement IntegralProxy[Int] so that it can return
// the Int-specific Range class from until/to.
/** A wrapper providing the additional methods available on `Int` values, such
 *  as `max`, `abs`, `toHexString`, and the `to` and `until` range
 *  constructors.
 *
 *  @param self the wrapped `Int` value
 */
@deprecated("use the extension methods available on primitive types instead", since = "3.10.0")
final class RichInt(val self: Int) extends AnyVal with ScalaNumberProxy[Int] with RangedProxy[Int] {
  /** The `Numeric` evidence for `Int`, [[scala.math.Numeric.IntIsIntegral]]. */
  protected def num: scala.math.Numeric.IntIsIntegral.type = scala.math.Numeric.IntIsIntegral
  /** The `Ordering` evidence for `Int`, [[scala.math.Ordering.Int]]. */
  protected def ord: scala.math.Ordering.Int.type = scala.math.Ordering.Int

  /** Returns the wrapped `Int` converted to a `Double`; the conversion is exact. */
  override def doubleValue = self.toDouble
  /** Returns the wrapped `Int` converted to a `Float`; magnitudes above 2^24^
   *  may be rounded, since a `Float` has only 24 bits of precision.
   */
  override def floatValue  = self.toFloat
  /** Returns the wrapped `Int` converted to a `Long`; the conversion is exact. */
  override def longValue   = self.toLong
  /** Returns the wrapped `Int` itself. */
  override def intValue    = self
  /** Returns the low 8 bits of the wrapped `Int` as a `Byte`. */
  override def byteValue   = self.toByte
  /** Returns the low 16 bits of the wrapped `Int` as a `Short`. */
  override def shortValue  = self.toShort

  /** Returns `true` if this number has no decimal component.
   *  Always `true` for `RichInt`.
   */
  @deprecated("isWhole on an integer type is always true", "2.12.15")
  def isWhole = true

  /** Always `true`, since the wrapped value is already an `Int`. */
  override def isValidInt   = true
  /** Always `true`, since every `Int` value fits in a `Long`. */
  def isValidLong  = true

  // These method are all overridden and redefined to call out to scala.math to avoid 3 allocations:
  // the primitive boxing, the value class boxing and instantiation of the Numeric num.
  // We'd like to redefine signum and sign too but forwards binary compatibility doesn't allow us to.
  /** Returns the absolute value of the wrapped `Int`.
   *
   *  @note `Int.MinValue` has no positive counterpart of type `Int`, so
   *        `Int.MinValue.abs` overflows and returns `Int.MinValue` itself.
   */
  override def abs: Int            = math.abs(self)
  /** Returns the larger of the wrapped `Int` and `that`.
   *
   *  @param that the value to compare with
   */
  override def max(that: Int): Int = math.max(self, that)
  /** Returns the smaller of the wrapped `Int` and `that`.
   *
   *  @param that the value to compare with
   */
  override def min(that: Int): Int = math.min(self, that)

  /** There is no reason to round an `Int`, but this method is provided to avoid accidental loss of precision from a detour through `Float`. */
  @deprecated("this is an integer type; there is no reason to round it.  Perhaps you meant to call this on a floating-point value?", "2.11.0")
  def round: Int = self

  /** Returns the wrapped `Int`'s 32-bit two's-complement value as an unsigned
   *  base-2 string with no sign and no leading zeros: `255.toBinaryString` is
   *  `"11111111"`, and `(-1).toBinaryString` is a string of thirty-two `'1'`
   *  characters.
   */
  def toBinaryString: String = java.lang.Integer.toBinaryString(self)
  /** Returns the wrapped `Int`'s 32-bit two's-complement value as an unsigned
   *  base-16 string with no sign, no leading zeros, and lowercase digits
   *  `a` to `f`: `255.toHexString` is `"ff"`, and `(-1).toHexString` is
   *  `"ffffffff"`.
   */
  def toHexString: String    = java.lang.Integer.toHexString(self)
  /** Returns the wrapped `Int`'s 32-bit two's-complement value as an unsigned
   *  base-8 string with no sign and no leading zeros: `8.toOctalString` is
   *  `"10"`.
   */
  def toOctalString: String  = java.lang.Integer.toOctalString(self)

  type ResultWithoutStep = Range

  /**
   *  @param end the final bound of the range to make (exclusive)
   *  @return a [[scala.collection.immutable.Range]] from `this` up to but
   *         not including `end`
   */
  def until(end: Int): Range = Range(self, end)

  /**
   *  @param end the final bound of the range to make (exclusive)
   *  @param step the increment value of the range
   *  @return a [[scala.collection.immutable.Range]] from `this` up to but
   *         not including `end`
   */
  def until(end: Int, step: Int): Range = Range(self, end, step)

  /** Like `until`, but includes the last index.
   *
   *  @param end the final bound of the range to make (inclusive)
   *  @return a [[scala.collection.immutable.Range.Inclusive]] from `this` up to
   *         and including `end`
   */
  def to(end: Int): Range.Inclusive = Range.inclusive(self, end)

  /**
   *  @param end the final bound of the range to make (inclusive)
   *  @param step the increment value of the range
   *  @return a [[scala.collection.immutable.Range.Inclusive]] from `this` up to
   *         and including `end`
   */
  def to(end: Int, step: Int): Range.Inclusive = Range.inclusive(self, end, step)
}
