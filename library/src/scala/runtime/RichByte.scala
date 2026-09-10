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

/** A wrapper providing the additional methods available on `Byte` values,
 *  such as `max`, `min`, and `abs`.
 *
 *  @param self the wrapped `Byte` value
 */
@deprecated("use the extension methods available on primitive types instead", since = "3.10.0")
final class RichByte(val self: Byte) extends AnyVal with ScalaWholeNumberProxy[Byte] {
  /** The `Numeric` evidence for `Byte`, [[scala.math.Numeric.ByteIsIntegral]]. */
  protected def num: scala.math.Numeric.ByteIsIntegral.type = scala.math.Numeric.ByteIsIntegral
  /** The `Ordering` evidence for `Byte`, [[scala.math.Ordering.Byte]]. */
  protected def ord: scala.math.Ordering.Byte.type = scala.math.Ordering.Byte

  /** Returns the wrapped `Byte` converted to a `Double`; the conversion is exact. */
  override def doubleValue = self.toDouble
  /** Returns the wrapped `Byte` converted to a `Float`; the conversion is exact. */
  override def floatValue  = self.toFloat
  /** Returns the wrapped `Byte` converted to a `Long`; the conversion is exact. */
  override def longValue   = self.toLong
  /** Returns the wrapped `Byte` converted to an `Int`; the conversion is exact. */
  override def intValue    = self.toInt
  /** Returns the wrapped `Byte` itself. */
  override def byteValue   = self
  /** Returns the wrapped `Byte` converted to a `Short`; the conversion is exact. */
  override def shortValue  = self.toShort

  /** Always `true`, since the wrapped value is already a `Byte`. */
  override def isValidByte   = true

  // These method are all overridden and redefined to call out to scala.math to avoid 3 allocations:
  // the primitive boxing, the value class boxing and instantiation of the Numeric num.
  // We'd like to redefine signum and sign too but forwards binary compatibility doesn't allow us to.
  /** Returns the absolute value of the wrapped `Byte`.
   *
   *  @note `Byte.MinValue` has no positive counterpart of type `Byte`, so
   *        `(-128: Byte).abs` overflows and returns `Byte.MinValue` itself.
   */
  override def abs: Byte             = math.abs(self).toByte
  /** Returns the larger of the wrapped `Byte` and `that`.
   *
   *  @param that the value to compare with
   */
  override def max(that: Byte): Byte = math.max(self, that).toByte
  /** Returns the smaller of the wrapped `Byte` and `that`.
   *
   *  @param that the value to compare with
   */
  override def min(that: Byte): Byte = math.min(self, that).toByte
}
