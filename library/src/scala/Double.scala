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

import scala.language.`2.13`

/** `Double`, a 64-bit IEEE-754 floating point number (equivalent to Java's `double` primitive type) is a
 *  subtype of [[scala.AnyVal]]. Instances of `Double` are not
 *  represented by an object in the underlying runtime system.
 *
 *  There is an implicit conversion from [[scala.Double]] => [[scala.runtime.RichDouble]]
 *  which provides useful non-primitive operations.
 */
final abstract class Double private extends AnyVal {
  /** Returns this value first converted to an `Int` (NaN to `0`, a finite value outside the `Int` range to `Int.MinValue` or `Int.MaxValue`, otherwise rounded toward zero), then narrowed to a `Byte` by keeping only its low-order 8 bits. */
  def toByte: Byte
  /** Returns this value first converted to an `Int` (NaN to `0`, a finite value outside the `Int` range to `Int.MinValue` or `Int.MaxValue`, otherwise rounded toward zero), then narrowed to a `Short` by keeping only its low-order 16 bits. */
  def toShort: Short
  /** Returns this value first converted to an `Int` (NaN to `0`, a finite value outside the `Int` range to `Int.MinValue` or `Int.MaxValue`, otherwise rounded toward zero), then narrowed to a `Char` by keeping only its low-order 16 bits. */
  def toChar: Char
  /** Returns this value as an `Int`, rounded toward zero. A NaN value converts to `0`, and a finite value outside the `Int` range converts to `Int.MinValue` or `Int.MaxValue`. */
  def toInt: Int
  /** Returns this value as a `Long`, rounded toward zero. A NaN value converts to `0L`, and a finite value outside the `Long` range converts to `Long.MinValue` or `Long.MaxValue`. */
  def toLong: Long
  /** Returns this value as a `Float`, which may lose precision. */
  def toFloat: Float
  /** Returns this value, unmodified. */
  def toDouble: Double

  /** Returns this value, unmodified. */
  def unary_+ : Double
  /** Returns the negation of this value. */
  def unary_- : Double

  @deprecated("Adding a number and a String is deprecated. Use the string interpolation `s\"$num$str\"`", "2.13.0")
  def +(x: String): String

  /** Returns `true` if this value is equal to x, `false` otherwise. */
  def ==(x: Byte): Boolean
  /** Returns `true` if this value is equal to x, `false` otherwise. */
  def ==(x: Short): Boolean
  /** Returns `true` if this value is equal to x, `false` otherwise. */
  def ==(x: Char): Boolean
  /** Returns `true` if this value is equal to x, `false` otherwise. */
  def ==(x: Int): Boolean
  /** Returns `true` if this value is equal to x, `false` otherwise. */
  def ==(x: Long): Boolean
  /** Returns `true` if this value is equal to x, `false` otherwise. */
  def ==(x: Float): Boolean
  /** Returns `true` if this value is equal to x, `false` otherwise. */
  def ==(x: Double): Boolean

  /** Returns `true` if this value is not equal to x, `false` otherwise. */
  def !=(x: Byte): Boolean
  /** Returns `true` if this value is not equal to x, `false` otherwise. */
  def !=(x: Short): Boolean
  /** Returns `true` if this value is not equal to x, `false` otherwise. */
  def !=(x: Char): Boolean
  /** Returns `true` if this value is not equal to x, `false` otherwise. */
  def !=(x: Int): Boolean
  /** Returns `true` if this value is not equal to x, `false` otherwise. */
  def !=(x: Long): Boolean
  /** Returns `true` if this value is not equal to x, `false` otherwise. */
  def !=(x: Float): Boolean
  /** Returns `true` if this value is not equal to x, `false` otherwise. */
  def !=(x: Double): Boolean

  /** Returns `true` if this value is less than x, `false` otherwise.
   *
   *  @param x the value to compare against this one
   */
  def <(x: Byte): Boolean
  /** Returns `true` if this value is less than x, `false` otherwise.
   *
   *  @param x the value to compare against this one
   */
  def <(x: Short): Boolean
  /** Returns `true` if this value is less than x, `false` otherwise.
   *
   *  @param x the value to compare against this one
   */
  def <(x: Char): Boolean
  /** Returns `true` if this value is less than x, `false` otherwise.
   *
   *  @param x the value to compare against this one
   */
  def <(x: Int): Boolean
  /** Returns `true` if this value is less than x, `false` otherwise.
   *
   *  @param x the value to compare against this one
   */
  def <(x: Long): Boolean
  /** Returns `true` if this value is less than x, `false` otherwise.
   *
   *  @param x the value to compare against this one
   */
  def <(x: Float): Boolean
  /** Returns `true` if this value is less than x, `false` otherwise.
   *
   *  @param x the value to compare against this one
   */
  def <(x: Double): Boolean

  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Byte): Boolean
  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Short): Boolean
  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Char): Boolean
  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Int): Boolean
  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Long): Boolean
  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Float): Boolean
  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Double): Boolean

  /** Returns `true` if this value is greater than x, `false` otherwise.
   *
   *  @param x the value to compare against this one
   */
  def >(x: Byte): Boolean
  /** Returns `true` if this value is greater than x, `false` otherwise.
   *
   *  @param x the value to compare against this one
   */
  def >(x: Short): Boolean
  /** Returns `true` if this value is greater than x, `false` otherwise.
   *
   *  @param x the value to compare against this one
   */
  def >(x: Char): Boolean
  /** Returns `true` if this value is greater than x, `false` otherwise.
   *
   *  @param x the value to compare against this one
   */
  def >(x: Int): Boolean
  /** Returns `true` if this value is greater than x, `false` otherwise.
   *
   *  @param x the value to compare against this one
   */
  def >(x: Long): Boolean
  /** Returns `true` if this value is greater than x, `false` otherwise.
   *
   *  @param x the value to compare against this one
   */
  def >(x: Float): Boolean
  /** Returns `true` if this value is greater than x, `false` otherwise.
   *
   *  @param x the value to compare against this one
   */
  def >(x: Double): Boolean

  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Byte): Boolean
  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Short): Boolean
  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Char): Boolean
  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Int): Boolean
  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Long): Boolean
  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Float): Boolean
  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Double): Boolean

  /** Returns the sum of this value and `x`.
   *
   *  @param x the value to add to this value
   */
  def +(x: Byte): Double
  /** Returns the sum of this value and `x`.
   *
   *  @param x the value to add to this value
   */
  def +(x: Short): Double
  /** Returns the sum of this value and `x`.
   *
   *  @param x the value to add to this value
   */
  def +(x: Char): Double
  /** Returns the sum of this value and `x`.
   *
   *  @param x the value to add to this value
   */
  def +(x: Int): Double
  /** Returns the sum of this value and `x`.
   *
   *  @param x the value to add to this value
   */
  def +(x: Long): Double
  /** Returns the sum of this value and `x`.
   *
   *  @param x the value to add to this value
   */
  def +(x: Float): Double
  /** Returns the sum of this value and `x`.
   *
   *  @param x the value to add to this value
   */
  def +(x: Double): Double

  /** Returns the difference of this value and `x`.
   *
   *  @param x the value to subtract from this value
   */
  def -(x: Byte): Double
  /** Returns the difference of this value and `x`.
   *
   *  @param x the value to subtract from this value
   */
  def -(x: Short): Double
  /** Returns the difference of this value and `x`.
   *
   *  @param x the value to subtract from this value
   */
  def -(x: Char): Double
  /** Returns the difference of this value and `x`.
   *
   *  @param x the value to subtract from this value
   */
  def -(x: Int): Double
  /** Returns the difference of this value and `x`.
   *
   *  @param x the value to subtract from this value
   */
  def -(x: Long): Double
  /** Returns the difference of this value and `x`.
   *
   *  @param x the value to subtract from this value
   */
  def -(x: Float): Double
  /** Returns the difference of this value and `x`.
   *
   *  @param x the value to subtract from this value
   */
  def -(x: Double): Double

  /** Returns the product of this value and `x`.
   *
   *  @param x the value to multiply this value by
   */
  def *(x: Byte): Double
  /** Returns the product of this value and `x`.
   *
   *  @param x the value to multiply this value by
   */
  def *(x: Short): Double
  /** Returns the product of this value and `x`.
   *
   *  @param x the value to multiply this value by
   */
  def *(x: Char): Double
  /** Returns the product of this value and `x`.
   *
   *  @param x the value to multiply this value by
   */
  def *(x: Int): Double
  /** Returns the product of this value and `x`.
   *
   *  @param x the value to multiply this value by
   */
  def *(x: Long): Double
  /** Returns the product of this value and `x`.
   *
   *  @param x the value to multiply this value by
   */
  def *(x: Float): Double
  /** Returns the product of this value and `x`.
   *
   *  @param x the value to multiply this value by
   */
  def *(x: Double): Double

  /** Returns the quotient of this value and `x`.
   *
   *  @param x the value to divide this value by
   */
  def /(x: Byte): Double
  /** Returns the quotient of this value and `x`.
   *
   *  @param x the value to divide this value by
   */
  def /(x: Short): Double
  /** Returns the quotient of this value and `x`.
   *
   *  @param x the value to divide this value by
   */
  def /(x: Char): Double
  /** Returns the quotient of this value and `x`.
   *
   *  @param x the value to divide this value by
   */
  def /(x: Int): Double
  /** Returns the quotient of this value and `x`.
   *
   *  @param x the value to divide this value by
   */
  def /(x: Long): Double
  /** Returns the quotient of this value and `x`.
   *
   *  @param x the value to divide this value by
   */
  def /(x: Float): Double
  /** Returns the quotient of this value and `x`.
   *
   *  @param x the value to divide this value by
   */
  def /(x: Double): Double

  /** Returns the remainder of the division of this value by `x`.
   *
   *  @param x the divisor
   */
  def %(x: Byte): Double
  /** Returns the remainder of the division of this value by `x`.
   *
   *  @param x the divisor
   */
  def %(x: Short): Double
  /** Returns the remainder of the division of this value by `x`.
   *
   *  @param x the divisor
   */
  def %(x: Char): Double
  /** Returns the remainder of the division of this value by `x`.
   *
   *  @param x the divisor
   */
  def %(x: Int): Double
  /** Returns the remainder of the division of this value by `x`.
   *
   *  @param x the divisor
   */
  def %(x: Long): Double
  /** Returns the remainder of the division of this value by `x`.
   *
   *  @param x the divisor
   */
  def %(x: Float): Double
  /** Returns the remainder of the division of this value by `x`.
   *
   *  @param x the divisor
   */
  def %(x: Double): Double

}

object Double extends AnyValCompanion {
  /** The smallest positive value greater than 0.0d which is
   *  representable as a Double.
   */
  final val MinPositiveValue = java.lang.Double.MIN_VALUE
  final val NaN              = java.lang.Double.NaN
  final val PositiveInfinity = java.lang.Double.POSITIVE_INFINITY
  final val NegativeInfinity = java.lang.Double.NEGATIVE_INFINITY

  /** The negative number with the greatest (finite) absolute value which is representable
   *  by a Double.  Note that it differs from [[java.lang.Double.MIN_VALUE]], which
   *  is the smallest positive value representable by a Double.  In Scala that number
   *  is called Double.MinPositiveValue.
   */
  final val MinValue = -java.lang.Double.MAX_VALUE

  /** The largest finite positive number representable as a Double. */
  final val MaxValue = java.lang.Double.MAX_VALUE

  /** Transforms a value type into a boxed reference type.
   *
   *  Runtime implementation determined by `scala.runtime.BoxesRunTime.boxToDouble`. See [src/library/scala/runtime/BoxesRunTime.java](https://github.com/scala/scala).
   *
   *  @param  x   the Double to be boxed
   *  @return     a java.lang.Double offering `x` as its underlying value.
   */
  def box(x: Double): java.lang.Double = ???

  /** Transforms a boxed type into a value type.  Note that this
   *  method is not typesafe: it accepts any Object, but will throw
   *  an exception if the argument is not a java.lang.Double.
   *
   *  Runtime implementation determined by `scala.runtime.BoxesRunTime.unboxToDouble`. See [src/library/scala/runtime/BoxesRunTime.java](https://github.com/scala/scala).
   *
   *  @param  x   the java.lang.Double to be unboxed.
   *  @return     the Double resulting from calling doubleValue() on `x`
   *  @throws     ClassCastException  if the argument is not a java.lang.Double
   */
  def unbox(x: java.lang.Object): Double = ???

  /** The `String` representation of the `scala.Double` companion object. */
  override def toString() = "object scala.Double"
}

