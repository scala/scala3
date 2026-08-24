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

/** A wrapper providing the additional methods available on `Double` values,
 *  such as `isNaN`, `round`, `ceil`, and `floor`.
 *
 *  @param self the wrapped `Double` value
 */
@deprecated("use the extension methods available on primitive types instead", since = "3.10.0")
final class RichDouble(val self: Double) extends AnyVal with FractionalProxy[Double] {
  /** The `Fractional` evidence for `Double`, [[scala.math.Numeric.DoubleIsFractional]]. */
  protected def num: Fractional[Double] = scala.math.Numeric.DoubleIsFractional
  /** The `Ordering` evidence for `Double`:
   *  [[scala.math.Ordering.Double.TotalOrdering]], a total ordering in which
   *  `NaN` is greater than every other value and -0.0 is less than 0.0.
   *  The comparison operators `<`, `<=`, `>`, and `>=` inherited from
   *  [[scala.math.Ordered]] use this ordering.
   */
  protected def ord: Ordering[Double]   = scala.math.Ordering.Double.TotalOrdering

  /** Returns the wrapped `Double` itself. */
  override def doubleValue = self
  /** Returns the wrapped `Double` converted to the nearest `Float`; the
   *  conversion may round, since a `Float` has only 24 bits of precision,
   *  and magnitudes beyond the `Float` range convert to the infinities.
   */
  override def floatValue  = self.toFloat
  /** Returns the wrapped `Double` truncated toward zero to a `Long`; `NaN`
   *  converts to 0, and values outside the `Long` range to `Long.MinValue`
   *  or `Long.MaxValue`.
   */
  override def longValue   = self.toLong
  /** Returns the wrapped `Double` truncated toward zero to an `Int`; `NaN`
   *  converts to 0, and values outside the `Int` range to `Int.MinValue` or
   *  `Int.MaxValue`.
   */
  override def intValue    = self.toInt
  /** Returns the wrapped `Double` truncated to an `Int` as by `intValue` and
   *  then narrowed to its low 8 bits as a `Byte`.
   */
  override def byteValue   = self.toByte
  /** Returns the wrapped `Double` truncated to an `Int` as by `intValue` and
   *  then narrowed to its low 16 bits as a `Short`.
   */
  override def shortValue  = self.toShort

  /** Returns `true` if the wrapped `Double` is a whole number: finite with
   *  no fractional part. `NaN` and the infinities are not whole.
   */
  override def isWhole = {
    val l = self.toLong
    l.toDouble == self || l == Long.MaxValue && self < Double.PositiveInfinity || l == Long.MinValue && self > Double.NegativeInfinity
  }
  /** Returns `true` if the wrapped `Double` exactly represents an integer in
   *  the `Byte` range, -128 to 127.
   */
  override def isValidByte  = self.toByte.toDouble == self
  /** Returns `true` if the wrapped `Double` exactly represents an integer in
   *  the `Short` range, -32768 to 32767.
   */
  override def isValidShort = self.toShort.toDouble == self
  /** Returns `true` if the wrapped `Double` exactly represents an integer in
   *  the `Char` range, 0 to 65535.
   */
  override def isValidChar  = self.toChar.toDouble == self
  /** Returns `true` if the wrapped `Double` represents an integer in the
   *  `Int` range, `Int.MinValue` to `Int.MaxValue`; every such integer is
   *  exactly representable as a `Double`.
   */
  override def isValidInt   = self.toInt.toDouble == self
  // override def isValidLong = { val l = self.toLong; l.toDouble == self && l != Long.MaxValue }
  // override def isValidFloat = self.toFloat.toDouble == self
  // override def isValidDouble = !java.lang.Double.isNaN(self)

  /** Returns `true` if the wrapped `Double` is `NaN` (not a number).
   *
   *  `NaN` is not equal to itself, so `== Double.NaN` is always `false`; use
   *  this method to test for `NaN`.
   */
  def isNaN: Boolean         = java.lang.Double.isNaN(self)
  /** Returns `true` if the wrapped `Double` is `Double.PositiveInfinity` or
   *  `Double.NegativeInfinity`.
   */
  def isInfinity: Boolean    = java.lang.Double.isInfinite(self)
  /** Returns `true` if the wrapped `Double` is neither infinite nor `NaN`. */
  def isFinite: Boolean      = java.lang.Double.isFinite(self)
  /** Returns `true` if the wrapped `Double` is `Double.PositiveInfinity`. */
  def isPosInfinity: Boolean = Double.PositiveInfinity == self
  /** Returns `true` if the wrapped `Double` is `Double.NegativeInfinity`. */
  def isNegInfinity: Boolean = Double.NegativeInfinity == self

  // These method are all overridden and redefined to call out to scala.math to avoid 3 allocations:
  // the primitive boxing, the value class boxing and instantiation of the Numeric num.
  // We'd like to redefine sign too but forwards binary compatibility doesn't allow us to.
  /** Returns the absolute value of the wrapped `Double`: `-0.0.abs` is
   *  `0.0`, both infinities map to `Double.PositiveInfinity`, and `NaN` maps
   *  to `NaN`.
   */
  override def abs: Double               = math.abs(self)
  /** Returns the larger of the wrapped `Double` and `that`. If either value
   *  is `NaN`, the result is `NaN`; 0.0 is larger than -0.0.
   *
   *  @param that the value to compare with
   */
  override def max(that: Double): Double = math.max(self, that)
  /** Returns the smaller of the wrapped `Double` and `that`. If either value
   *  is `NaN`, the result is `NaN`; -0.0 is smaller than 0.0.
   *
   *  @param that the value to compare with
   */
  override def min(that: Double): Double = math.min(self, that)
  /** Returns the sign of the wrapped `Double` as an `Int`: -1 if it is
   *  negative, 1 if it is positive, and 0 if it is zero; `-0.0` and `NaN`
   *  also yield 0.
   */
  @deprecated("signum does not handle -0.0 or Double.NaN; use `sign` method instead", since = "2.13.0")
  override def signum: Int               = math.signum(self).toInt

  /** Returns the wrapped `Double` rounded to the nearest `Long`, with ties
   *  rounded upward: `0.5.round` is 1 and `(-0.5).round` is 0. `NaN` rounds
   *  to 0, and values beyond the `Long` range to `Long.MinValue` or
   *  `Long.MaxValue`.
   */
  def round: Long   = math.round(self)
  /** Returns the smallest `Double` that is greater than or equal to the
   *  wrapped value and equals a mathematical integer; `NaN`, the infinities,
   *  and values already equal to an integer return themselves.
   */
  def ceil: Double  = math.ceil(self)
  /** Returns the largest `Double` that is less than or equal to the wrapped
   *  value and equals a mathematical integer; `NaN`, the infinities, and
   *  values already equal to an integer return themselves.
   */
  def floor: Double = math.floor(self)

  /** Converts an angle measured in degrees to an approximately equivalent
   *  angle measured in radians.
   *
   *  @return the measurement of the angle x in radians.
   */
  def toRadians: Double = math.toRadians(self)

  /** Converts an angle measured in radians to an approximately equivalent
   *  angle measured in degrees.
   *  @return the measurement of the angle x in degrees.
   */
  def toDegrees: Double = math.toDegrees(self)
}
