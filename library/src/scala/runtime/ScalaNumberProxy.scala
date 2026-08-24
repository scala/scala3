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
import scala.collection.immutable
import scala.math.ScalaNumericAnyConversions
import immutable.NumericRange

/** Base classes for the Rich* wrappers of the primitive types.
 *  As with all classes in scala.runtime.*, this is not a supported API.
 */
@deprecated("use the extension methods available on primitive types instead", since = "3.10.0")
trait ScalaNumberProxy[T] extends Any with ScalaNumericAnyConversions with Proxy.Typed[T] with OrderedProxy[T] {
  /** The `Numeric` evidence for `T`, used to implement the arithmetic and
   *  comparison operations of this proxy.
   */
  protected implicit def num: Numeric[T]

  /** Returns the wrapped value converted to a `Double` by the `Numeric` evidence. */
  def doubleValue = num.toDouble(self)
  /** Returns the wrapped value converted to a `Float` by the `Numeric` evidence. */
  def floatValue  = num.toFloat(self)
  /** Returns the wrapped value converted to a `Long` by the `Numeric` evidence. */
  def longValue   = num.toLong(self)
  /** Returns the wrapped value converted to an `Int` by the `Numeric` evidence. */
  def intValue    = num.toInt(self)
  /** Returns the wrapped value converted to an `Int` and then narrowed to its low 8 bits as a `Byte`. */
  def byteValue   = intValue.toByte
  /** Returns the wrapped value converted to an `Int` and then narrowed to its low 16 bits as a `Short`. */
  def shortValue  = intValue.toShort

  /** Returns `this` if `this < that` or `that` otherwise.
   *
   *  @param that the value to compare against for finding the minimum
   *  @return the smaller of `this` and `that`
   */
  def min(that: T): T = num.min(self, that)
  /** Returns `this` if `this > that` or `that` otherwise.
   *
   *  @param that the value to compare against for finding the maximum
   *  @return the larger of `this` and `that`
   */
  def max(that: T): T = num.max(self, that)
  /** Returns the absolute value of `this`. */
  def abs             = num.abs(self)
  /** Returns the sign of `this`.
   *  zero if the argument is zero, -zero if the argument is -zero,
   *  one if the argument is greater than zero, -one if the argument is less than zero,
   *  and NaN if the argument is NaN where applicable.
   */
  def sign: T         = num.sign(self)
  /** Returns the signum of `this`. */
  @deprecated("use `sign` method instead", since = "2.13.0") def signum: Int = num.signum(self)
}

/** Base trait for the `Rich*` wrappers of the whole-number primitive types,
 *  such as [[RichByte]], [[RichShort]], and [[RichLong]].
 *
 *  @tparam T the wrapped primitive type
 */
@deprecated("use the extension methods available on primitive types instead", since = "3.10.0")
trait ScalaWholeNumberProxy[T] extends Any with ScalaNumberProxy[T] {
  /** Always `true`, since a whole number has no fractional part. */
  @deprecated("isWhole on an integer type is always true", "2.12.15")
  def isWhole = true
}

/** Base trait for the `Rich*` wrappers whose type has `Integral` evidence, such
 *  as [[RichChar]] and [[RichLong]], implementing the `until` and `to` range
 *  constructors with [[scala.collection.immutable.NumericRange]].
 *
 *  @tparam T the wrapped primitive type
 */
@deprecated("use the extension methods available on primitive types instead", since = "3.10.0")
trait IntegralProxy[T] extends Any with ScalaWholeNumberProxy[T] with RangedProxy[T] {
  /** The `Integral` evidence for `T`, also used as the source of the default
   *  range step, `num.one`.
   */
  protected implicit def num: Integral[T]
  type ResultWithoutStep = NumericRange[T]

  /** Returns a range from the wrapped value up to but not including `end`, in
   *  steps of `num.one`.
   *
   *  @param end the exclusive end point of the range
   *  @return an exclusive [[scala.collection.immutable.NumericRange]] from this
   *          value until `end` with step 1
   */
  def until(end: T): NumericRange.Exclusive[T]          = NumericRange(self, end, num.one)
  /** Returns a range from the wrapped value up to but not including `end`,
   *  advancing by `step`.
   *
   *  @param end the exclusive end point of the range
   *  @param step the increment between consecutive elements; may be negative
   *  @return an exclusive [[scala.collection.immutable.NumericRange]] from this
   *          value until `end` with the given step
   *  @throws IllegalArgumentException if `step` is zero, or if the range has more
   *          than `Int.MaxValue` elements; both are raised when the range's length
   *          is first computed, not when it is constructed
   */
  def until(end: T, step: T): NumericRange.Exclusive[T] = NumericRange(self, end, step)
  /** Returns a range from the wrapped value up to and including `end`, in
   *  steps of `num.one`.
   *
   *  @param end the inclusive end point of the range
   *  @return an inclusive [[scala.collection.immutable.NumericRange]] from this
   *          value to `end` with step 1
   */
  def to(end: T): NumericRange.Inclusive[T]             = NumericRange.inclusive(self, end, num.one)
  /** Returns a range from the wrapped value up to and including `end`,
   *  advancing by `step`.
   *
   *  @param end the inclusive end point of the range
   *  @param step the increment between consecutive elements; may be negative
   *  @return an inclusive [[scala.collection.immutable.NumericRange]] from this
   *          value to `end` with the given step
   *  @throws IllegalArgumentException if `step` is zero, or if the range has more
   *          than `Int.MaxValue` elements; both are raised when the range's length
   *          is first computed, not when it is constructed
   */
  def to(end: T, step: T): NumericRange.Inclusive[T]    = NumericRange.inclusive(self, end, step)
}

/** Base trait for the `Rich*` wrappers of the fractional primitive types,
 *  [[RichFloat]] and [[RichDouble]].
 *
 *  @tparam T the wrapped primitive type
 */
@deprecated("use the extension methods available on primitive types instead", since = "3.10.0")
trait FractionalProxy[T] extends Any with ScalaNumberProxy[T] {
  /** The `Fractional` evidence for `T`, used to implement the arithmetic and
   *  comparison operations of this proxy.
   */
  protected implicit def num: Fractional[T]

  /** Returns `false`. [[RichFloat]] and [[RichDouble]] override this to test
   *  whether the wrapped value has a fractional part.
   */
  def isWhole = false
}

/** Base trait for the `Rich*` wrappers that implements [[scala.math.Ordered]]
 *  in terms of an [[scala.math.Ordering]], giving the wrapped value the
 *  comparison operators `<`, `<=`, `>`, and `>=`.
 *
 *  @tparam T the wrapped primitive type
 */
@deprecated("use the extension methods available on primitive types instead", since = "3.10.0")
trait OrderedProxy[T] extends Any with Ordered[T] with Proxy.Typed[T] {
  /** The `Ordering` evidence for `T`, used to implement `compare`. */
  protected def ord: Ordering[T]

  /** Returns the result of comparing the wrapped value with `y` using `ord`:
   *  negative if this value is smaller than `y`, positive if it is larger,
   *  and 0 if the two are equal.
   *
   *  @param y the value to compare the wrapped value with
   */
  def compare(y: T) = ord.compare(self, y)
}

/** Base trait for the `Rich*` wrappers that provide the `until` and `to` range
 *  constructors. [[RichInt]] implements them with the `Int`-specific
 *  [[scala.collection.immutable.Range]]; the [[IntegralProxy]] wrappers use
 *  [[scala.collection.immutable.NumericRange]].
 *
 *  @tparam T the wrapped primitive type
 */
@deprecated("use the extension methods available on primitive types instead", since = "3.10.0")
trait RangedProxy[T] extends Any with Proxy.Typed[T] {
  type ResultWithoutStep

  /** Returns a range from the wrapped value up to but not including `end`,
   *  with step 1.
   *
   *  @param end the exclusive end point of the range
   *  @return a range from this value until `end`, excluding `end`
   */
  def until(end: T): ResultWithoutStep
  /** Returns a range from the wrapped value up to but not including `end`,
   *  advancing by `step`.
   *
   *  @param end the exclusive end point of the range
   *  @param step the increment between consecutive elements; may be negative
   *          but must not be zero
   *  @return a range from this value until `end`, excluding `end`, with the
   *          given step
   */
  def until(end: T, step: T): immutable.IndexedSeq[T]
  /** Returns a range from the wrapped value up to and including `end`, with
   *  step 1.
   *
   *  @param end the inclusive end point of the range
   *  @return a range from this value to `end`, including `end`
   */
  def to(end: T): ResultWithoutStep
  /** Returns a range from the wrapped value up to and including `end`,
   *  advancing by `step`.
   *
   *  @param end the inclusive end point of the range
   *  @param step the increment between consecutive elements; may be negative
   *          but must not be zero
   *  @return a range from this value to `end`, including `end`, with the
   *          given step
   */
  def to(end: T, step: T): immutable.IndexedSeq[T]
}
