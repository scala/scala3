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

// DO NOT EDIT, CHANGES WILL BE LOST
// This auto-generated code can be modified in "project/GenerateAnyVals.scala".
// Afterwards, running "sbt generateSources" regenerates this source file.

package scala

import scala.language.`2.13`

import scala.collection.immutable.NumericRange

/** `Double`, a 64-bit IEEE-754 floating point number (equivalent to Java's `double` primitive type) is a
 *  subtype of [[scala.AnyVal]]. Instances of `Double` are not
 *  represented by an object in the underlying runtime system.
 *
 *  There is an implicit conversion from [[scala.Double]] => [[scala.runtime.RichDouble]]
 *  which provides useful non-primitive operations.
 */
final abstract class Double private extends AnyVal {
  def toByte: Byte
  def toShort: Short
  def toChar: Char
  def toInt: Int
  def toLong: Long
  def toFloat: Float
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

  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Byte): Boolean
  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Short): Boolean
  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Char): Boolean
  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Int): Boolean
  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Long): Boolean
  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Float): Boolean
  /** Returns `true` if this value is less than x, `false` otherwise. */
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

  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Byte): Boolean
  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Short): Boolean
  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Char): Boolean
  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Int): Boolean
  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Long): Boolean
  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Float): Boolean
  /** Returns `true` if this value is greater than x, `false` otherwise. */
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

  /** Returns the sum of this value and `x`. */
  def +(x: Byte): Double
  /** Returns the sum of this value and `x`. */
  def +(x: Short): Double
  /** Returns the sum of this value and `x`. */
  def +(x: Char): Double
  /** Returns the sum of this value and `x`. */
  def +(x: Int): Double
  /** Returns the sum of this value and `x`. */
  def +(x: Long): Double
  /** Returns the sum of this value and `x`. */
  def +(x: Float): Double
  /** Returns the sum of this value and `x`. */
  def +(x: Double): Double

  /** Returns the difference of this value and `x`. */
  def -(x: Byte): Double
  /** Returns the difference of this value and `x`. */
  def -(x: Short): Double
  /** Returns the difference of this value and `x`. */
  def -(x: Char): Double
  /** Returns the difference of this value and `x`. */
  def -(x: Int): Double
  /** Returns the difference of this value and `x`. */
  def -(x: Long): Double
  /** Returns the difference of this value and `x`. */
  def -(x: Float): Double
  /** Returns the difference of this value and `x`. */
  def -(x: Double): Double

  /** Returns the product of this value and `x`. */
  def *(x: Byte): Double
  /** Returns the product of this value and `x`. */
  def *(x: Short): Double
  /** Returns the product of this value and `x`. */
  def *(x: Char): Double
  /** Returns the product of this value and `x`. */
  def *(x: Int): Double
  /** Returns the product of this value and `x`. */
  def *(x: Long): Double
  /** Returns the product of this value and `x`. */
  def *(x: Float): Double
  /** Returns the product of this value and `x`. */
  def *(x: Double): Double

  /** Returns the quotient of this value and `x`. */
  def /(x: Byte): Double
  /** Returns the quotient of this value and `x`. */
  def /(x: Short): Double
  /** Returns the quotient of this value and `x`. */
  def /(x: Char): Double
  /** Returns the quotient of this value and `x`. */
  def /(x: Int): Double
  /** Returns the quotient of this value and `x`. */
  def /(x: Long): Double
  /** Returns the quotient of this value and `x`. */
  def /(x: Float): Double
  /** Returns the quotient of this value and `x`. */
  def /(x: Double): Double

  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Byte): Double
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Short): Double
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Char): Double
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Int): Double
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Long): Double
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Float): Double
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Double): Double

  // Provide a more specific return type for Scaladoc
  override def getClass(): Class[Double] = ???
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
   *  Runtime implementation determined by `scala.runtime.BoxesRunTime.boxToDouble`. See [[https://github.com/scala/scala src/library/scala/runtime/BoxesRunTime.java]].
   *
   *  @param  x   the Double to be boxed
   *  @return     a java.lang.Double offering `x` as its underlying value.
   */
  def box(x: Double): java.lang.Double = ???

  /** Transforms a boxed type into a value type.  Note that this
   *  method is not typesafe: it accepts any Object, but will throw
   *  an exception if the argument is not a java.lang.Double.
   *
   *  Runtime implementation determined by `scala.runtime.BoxesRunTime.unboxToDouble`. See [[https://github.com/scala/scala src/library/scala/runtime/BoxesRunTime.java]].
   *
   *  @param  x   the java.lang.Double to be unboxed.
   *  @throws     ClassCastException  if the argument is not a java.lang.Double
   *  @return     the Double resulting from calling doubleValue() on `x`
   */
  def unbox(x: java.lang.Object): Double = ???

  /** The String representation of the scala.Double companion object. */
  override def toString = "object scala.Double"

  extension (self: Double) {
    /** Returns `'''true'''` if this number is finite and has no decimal component. */
    def isWhole: Boolean = {
      val l = self.toLong
      l.toDouble == self || l == Long.MaxValue && self < Double.PositiveInfinity || l == Long.MinValue && self > Double.NegativeInfinity
    }

    /** Returns `true` iff this has a zero fractional part, and is within the
      * range of [[scala.Char]] MinValue and MaxValue; otherwise returns `false`.
      */
    def isValidChar: Boolean = self.toChar.toDouble == self

    /** Returns `true` iff this has a zero fractional part, and is within the
      * range of [[scala.Byte]] MinValue and MaxValue; otherwise returns `false`.
      */
    def isValidByte: Boolean = self.toByte.toDouble == self

    /** Returns `true` iff this has a zero fractional part, and is within the
      * range of [[scala.Short]] MinValue and MaxValue; otherwise returns `false`.
      */
    def isValidShort: Boolean = self.toShort.toDouble == self

    /** Returns `true` iff this has a zero fractional part, and is within the
      * range of [[scala.Int]] MinValue and MaxValue; otherwise returns `false`.
      */
    def isValidInt: Boolean = self.toInt.toDouble == self

    /** Returns `true` iff `this` is a `NaN` value. */
    def isNaN: Boolean = java.lang.Double.isNaN(self)

    /** Returns `true` iff `this` is `PositiveInfinity` or `NegativeInfinity`. */
    def isInfinity: Boolean = java.lang.Double.isInfinite(self)

    /** Returns `true` iff `this` is a finite value, i.e., not an infinity nor `NaN`. */
    def isFinite: Boolean = java.lang.Double.isFinite(self)

    /** Returns `true` iff `this` is `PositiveInfinity`. */
    def isPosInfinity: Boolean = Double.PositiveInfinity == self

    /** Returns `true` iff `this` is `NegativeInfinity`. */
    def isNegInfinity: Boolean = Double.NegativeInfinity == self

    /** Returns the absolute value of `this`. */
    def abs: Double = java.lang.Math.abs(self)

    /** Returns `this` if `this > that` or `that` otherwise. */
    def max(that: Double): Double = java.lang.Math.max(self, that)

    /** Returns `this` if `this < that` or `that` otherwise. */
    def min(that: Double): Double = java.lang.Math.min(self, that)

    /** Returns the sign of `this`.
      *
      * - `1.0` if `this > 0.0`;
      * - `-1.0` if `this < 0.0`;
      * - `this` otherwise (for zeros and `NaN`).
      */
    def sign: Double = java.lang.Math.signum(self)

    /** Returns the signum of `this`. */
    @deprecated("signum does not handle -0.0 or Double.NaN; use `sign` method instead", since = "2.13.0")
    def signum: Int = self.sign.toInt

    /** Returns the closest `Long` to `this`. */
    def round: Long = java.lang.Math.round(self)

    /** Returns the smallest integer greater or equal to `this`. */
    def ceil: Double = java.lang.Math.ceil(self)

    /** Returns the largest integer smaller or equal to `this`. */
    def floor: Double = java.lang.Math.floor(self)

    /** Converts an angle measured in degrees to an approximately equivalent
     *  angle measured in radians.
     *
     *  @return the measurement of the angle x in radians.
     */
    def toRadians: Double = java.lang.Math.toRadians(self)

    /** Converts an angle measured in radians to an approximately equivalent
     *  angle measured in degrees.
     *  @return the measurement of the angle x in degrees.
     */
    def toDegrees: Double = java.lang.Math.toDegrees(self)

    /** Compares `this` to `that` according to the standard total ordering.
      *
      * Returns:
      * - a positive value if `this > that`
      * - a negative value if `this < that`
      * - `0` if `this == that`
      *
      * Special cases for this method:
      * - `0.0` is considered greater than `-0.0`
      * - `NaN` is considered greater than all other values, but equal to itself
      */
    def compare(that: Double): Int = java.lang.Double.compare(self, that)
  }
}
