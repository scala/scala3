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

/** A wrapper providing the additional methods available on `Short` values,
 *  such as `max`, `min`, and `abs`.
 *
 *  @param self the wrapped `Short` value
 */
@deprecated("use the extension methods available on primitive types instead", since = "3.10.0")
final class RichShort(val self: Short) extends AnyVal with ScalaWholeNumberProxy[Short] {
  /** The `Numeric` evidence for `Short`, [[scala.math.Numeric.ShortIsIntegral]]. */
  protected def num: scala.math.Numeric.ShortIsIntegral.type = scala.math.Numeric.ShortIsIntegral
  /** The `Ordering` evidence for `Short`, [[scala.math.Ordering.Short]]. */
  protected def ord: scala.math.Ordering.Short.type = scala.math.Ordering.Short

  /** Returns the wrapped `Short` converted to a `Double`; the conversion is exact. */
  override def doubleValue = self.toDouble
  /** Returns the wrapped `Short` converted to a `Float`; the conversion is exact. */
  override def floatValue  = self.toFloat
  /** Returns the wrapped `Short` converted to a `Long`; the conversion is exact. */
  override def longValue   = self.toLong
  /** Returns the wrapped `Short` converted to an `Int`; the conversion is exact. */
  override def intValue    = self.toInt
  /** Returns the low 8 bits of the wrapped `Short` as a `Byte`. */
  override def byteValue   = self.toByte
  /** Returns the wrapped `Short` itself. */
  override def shortValue  = self

  /** Always `true`, since the wrapped value is already a `Short`. */
  override def isValidShort  = true

  // These method are all overridden and redefined to call out to scala.math to avoid 3 allocations:
  // the primitive boxing, the value class boxing and instantiation of the Numeric num.
  // We'd like to redefine signum and sign too but forwards binary compatibility doesn't allow us to.
  /** Returns the absolute value of the wrapped `Short`.
   *
   *  @note `Short.MinValue` has no positive counterpart of type `Short`, so
   *        `(-32768: Short).abs` overflows and returns `Short.MinValue`
   *        itself.
   */
  override def abs: Short              = math.abs(self.toInt).toShort
  /** Returns the larger of the wrapped `Short` and `that`.
   *
   *  @param that the value to compare with
   */
  override def max(that: Short): Short = math.max(self.toInt, that.toInt).toShort
  /** Returns the smaller of the wrapped `Short` and `that`.
   *
   *  @param that the value to compare with
   */
  override def min(that: Short): Short = math.min(self.toInt, that.toInt).toShort
}
