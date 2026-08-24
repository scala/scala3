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

/** A wrapper providing the additional methods available on `Long` values,
 *  such as `max`, `abs`, `toHexString`, and the `to` and `until` range
 *  constructors.
 *
 *  @param self the wrapped `Long` value
 */
@deprecated("use the extension methods available on primitive types instead", since = "3.10.0")
final class RichLong(val self: Long) extends AnyVal with IntegralProxy[Long] {
  /** The `Integral` evidence for `Long`, [[scala.math.Numeric.LongIsIntegral]]. */
  protected def num: scala.math.Numeric.LongIsIntegral.type = scala.math.Numeric.LongIsIntegral
  /** The `Ordering` evidence for `Long`, [[scala.math.Ordering.Long]]. */
  protected def ord: scala.math.Ordering.Long.type = scala.math.Ordering.Long

  /** Returns the wrapped `Long` converted to a `Double`; magnitudes above
   *  2^53^ may be rounded, since a `Double` has only 53 bits of precision.
   */
  override def doubleValue = self.toDouble
  /** Returns the wrapped `Long` converted to a `Float`; magnitudes above
   *  2^24^ may be rounded, since a `Float` has only 24 bits of precision.
   */
  override def floatValue  = self.toFloat
  /** Returns the wrapped `Long` itself. */
  override def longValue   = self
  /** Returns the low 32 bits of the wrapped `Long` as an `Int`. */
  override def intValue    = self.toInt
  /** Returns the low 8 bits of the wrapped `Long` as a `Byte`. */
  override def byteValue   = self.toByte
  /** Returns the low 16 bits of the wrapped `Long` as a `Short`. */
  override def shortValue  = self.toShort

  /** Returns `true` if the wrapped `Long` fits in a `Byte`, that is, lies
   *  between -128 and 127 inclusive.
   */
  override def isValidByte  = self.toByte.toLong == self
  /** Returns `true` if the wrapped `Long` fits in a `Short`, that is, lies
   *  between -32768 and 32767 inclusive.
   */
  override def isValidShort = self.toShort.toLong == self
  /** Returns `true` if the wrapped `Long` fits in a `Char`, that is, lies
   *  between 0 and 65535 inclusive.
   */
  override def isValidChar  = self.toChar.toLong == self
  /** Returns `true` if the wrapped `Long` fits in an `Int`, that is, lies
   *  between `Int.MinValue` and `Int.MaxValue` inclusive.
   */
  override def isValidInt   = self.toInt.toLong == self
           def isValidLong  = true
  // override def isValidFloat = self.toFloat.toLong == self && self != Long.MaxValue
  // override def isValidDouble = self.toDouble.toLong == self && self != Long.MaxValue

  // These method are all overridden and redefined to call out to scala.math to avoid 3 allocations:
  // the primitive boxing, the value class boxing and instantiation of the Numeric num.
  // We'd like to redefine signum and sign too but forwards binary compatibility doesn't allow us to.
  /** Returns the absolute value of the wrapped `Long`.
   *
   *  @note `Long.MinValue` has no positive counterpart of type `Long`, so
   *        `Long.MinValue.abs` overflows and returns `Long.MinValue` itself.
   */
  override def abs: Long             = math.abs(self)
  /** Returns the larger of the wrapped `Long` and `that`.
   *
   *  @param that the value to compare with
   */
  override def max(that: Long): Long = math.max(self, that)
  /** Returns the smaller of the wrapped `Long` and `that`.
   *
   *  @param that the value to compare with
   */
  override def min(that: Long): Long = math.min(self, that)

  /** There is no reason to round a `Long`, but this method is provided to avoid accidental conversion to `Int` through `Float`. */
  @deprecated("this is an integer type; there is no reason to round it.  Perhaps you meant to call this on a floating-point value?", "2.11.0")
  def round: Long = self

  /** Returns the wrapped `Long`'s 64-bit two's-complement value as an
   *  unsigned base-2 string with no sign and no leading zeros:
   *  `255L.toBinaryString` is `"11111111"`, and `(-1L).toBinaryString` is a
   *  string of sixty-four `'1'` characters.
   */
  def toBinaryString: String = java.lang.Long.toBinaryString(self)
  /** Returns the wrapped `Long`'s 64-bit two's-complement value as an
   *  unsigned base-16 string with no sign, no leading zeros, and lowercase
   *  digits `a` to `f`: `255L.toHexString` is `"ff"`, and `(-1L).toHexString`
   *  is a string of sixteen `'f'` characters.
   */
  def toHexString: String    = java.lang.Long.toHexString(self)
  /** Returns the wrapped `Long`'s 64-bit two's-complement value as an
   *  unsigned base-8 string with no sign and no leading zeros:
   *  `8L.toOctalString` is `"10"`.
   */
  def toOctalString: String  = java.lang.Long.toOctalString(self)
}
