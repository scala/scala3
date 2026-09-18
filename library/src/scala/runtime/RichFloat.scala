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

/** A wrapper providing the additional methods available on `Float` values,
 *  such as `isNaN`, `round`, `ceil`, and `floor`.
 *
 *  @param self the wrapped `Float` value
 */
@deprecated("use the extension methods available on primitive types instead", since = "3.10.0")
final class RichFloat(val self: Float) extends AnyVal with FractionalProxy[Float] {
  /** The `Fractional` evidence for `Float`, [[scala.math.Numeric.FloatIsFractional]]. */
  protected def num: Fractional[Float] = scala.math.Numeric.FloatIsFractional
  /** The `Ordering` evidence for `Float`:
   *  [[scala.math.Ordering.Float.TotalOrdering]], a total ordering in which
   *  `NaN` is greater than every other value and -0.0f is less than 0.0f.
   *  The comparison operators `<`, `<=`, `>`, and `>=` inherited from
   *  [[scala.math.Ordered]] use this ordering.
   */
  protected def ord: Ordering[Float]   = scala.math.Ordering.Float.TotalOrdering

  /** Returns the wrapped `Float` converted to a `Double`; the conversion is exact. */
  override def doubleValue = self.toDouble
  /** Returns the wrapped `Float` itself. */
  override def floatValue  = self
  /** Returns the wrapped `Float` truncated toward zero to a `Long`; `NaN`
   *  converts to 0, and values outside the `Long` range to `Long.MinValue`
   *  or `Long.MaxValue`.
   */
  override def longValue   = self.toLong
  /** Returns the wrapped `Float` truncated toward zero to an `Int`; `NaN`
   *  converts to 0, and values outside the `Int` range to `Int.MinValue` or
   *  `Int.MaxValue`.
   */
  override def intValue    = self.toInt
  /** Returns the wrapped `Float` truncated to an `Int` as by `intValue` and
   *  then narrowed to its low 8 bits as a `Byte`.
   */
  override def byteValue   = self.toByte
  /** Returns the wrapped `Float` truncated to an `Int` as by `intValue` and
   *  then narrowed to its low 16 bits as a `Short`.
   */
  override def shortValue  = self.toShort

  /** Returns `true` if the wrapped `Float` is a whole number: finite with no
   *  fractional part. `NaN` and the infinities are not whole.
   */
  override def isWhole = {
    val l = self.toLong
    l.toFloat == self || l == Long.MaxValue && self < Float.PositiveInfinity || l == Long.MinValue && self > Float.NegativeInfinity
  }
  /** Returns `true` if the wrapped `Float` exactly represents an integer in
   *  the `Byte` range, -128 to 127.
   */
  override def isValidByte  = self.toByte.toFloat == self
  /** Returns `true` if the wrapped `Float` exactly represents an integer in
   *  the `Short` range, -32768 to 32767.
   */
  override def isValidShort = self.toShort.toFloat == self
  /** Returns `true` if the wrapped `Float` exactly represents an integer in
   *  the `Char` range, 0 to 65535.
   */
  override def isValidChar  = self.toChar.toFloat == self
  /** Returns `true` if the wrapped `Float` exactly represents an integer in
   *  the `Int` range. Since a `Float` has only 24 bits of precision, no
   *  `Float` equals `Int.MaxValue`, and large in-range integers are only
   *  valid if they are exactly representable.
   */
  override def isValidInt   = { val i = self.toInt; i.toFloat == self && i != Int.MaxValue }
  // override def isValidLong = { val l = self.toLong; l.toFloat == self && l != Long.MaxValue }
  // override def isValidFloat = !java.lang.Float.isNaN(self)
  // override def isValidDouble = !java.lang.Float.isNaN(self)

  /** Returns `true` if the wrapped `Float` is `NaN` (not a number).
   *
   *  `NaN` is not equal to itself, so `== Float.NaN` is always `false`; use
   *  this method to test for `NaN`.
   */
  def isNaN: Boolean         = java.lang.Float.isNaN(self)
  /** Returns `true` if the wrapped `Float` is `Float.PositiveInfinity` or
   *  `Float.NegativeInfinity`.
   */
  def isInfinity: Boolean    = java.lang.Float.isInfinite(self)
  /** Returns `true` if the wrapped `Float` is neither infinite nor `NaN`. */
  def isFinite: Boolean      = java.lang.Float.isFinite(self)
  /** Returns `true` if the wrapped `Float` is `Float.PositiveInfinity`. */
  def isPosInfinity: Boolean = Float.PositiveInfinity == self
  /** Returns `true` if the wrapped `Float` is `Float.NegativeInfinity`. */
  def isNegInfinity: Boolean = Float.NegativeInfinity == self

  // These method are all overridden and redefined to call out to scala.math to avoid 3 allocations:
  // the primitive boxing, the value class boxing and instantiation of the Numeric num.
  // We'd like to redefine sign too but forwards binary compatibility doesn't allow us to.
  /** Returns the absolute value of the wrapped `Float`: `-0.0f.abs` is
   *  `0.0f`, both infinities map to `Float.PositiveInfinity`, and `NaN` maps
   *  to `NaN`.
   */
  override def abs: Float              = math.abs(self)
  /** Returns the larger of the wrapped `Float` and `that`. If either value
   *  is `NaN`, the result is `NaN`; 0.0f is larger than -0.0f.
   *
   *  @param that the value to compare with
   */
  override def max(that: Float): Float = math.max(self, that)
  /** Returns the smaller of the wrapped `Float` and `that`. If either value
   *  is `NaN`, the result is `NaN`; -0.0f is smaller than 0.0f.
   *
   *  @param that the value to compare with
   */
  override def min(that: Float): Float = math.min(self, that)
  /** Returns the sign of the wrapped `Float` as an `Int`: -1 if it is
   *  negative, 1 if it is positive, and 0 if it is zero; `-0.0f` and `NaN`
   *  also yield 0.
   */
  @deprecated("signum does not handle -0.0f or Float.NaN; use `sign` method instead", since = "2.13.0")
  override def signum: Int             = math.signum(self).toInt

  /** Returns the wrapped `Float` rounded to the nearest `Int`, with ties
   *  rounded upward: `0.5f.round` is 1 and `(-0.5f).round` is 0. `NaN`
   *  rounds to 0, and values beyond the `Int` range to `Int.MinValue` or
   *  `Int.MaxValue`.
   */
  def round: Int   = math.round(self)
  /** Returns the smallest `Float` that is greater than or equal to the
   *  wrapped value and equals a mathematical integer; `NaN`, the infinities,
   *  and values already equal to an integer return themselves.
   */
  def ceil: Float  = math.ceil(self.toDouble).toFloat
  /** Returns the largest `Float` that is less than or equal to the wrapped
   *  value and equals a mathematical integer; `NaN`, the infinities, and
   *  values already equal to an integer return themselves.
   */
  def floor: Float = math.floor(self.toDouble).toFloat

  /** Converts an angle measured in degrees to an approximately equivalent
   *  angle measured in radians.
   *
   *  @return the measurement of the angle `x` in radians.
   */
  def toRadians: Float = math.toRadians(self.toDouble).toFloat

  /** Converts an angle measured in radians to an approximately equivalent
   *  angle measured in degrees.
   *
   *  @return the measurement of the angle `x` in degrees.
   */
  def toDegrees: Float = math.toDegrees(self.toDouble).toFloat
}
