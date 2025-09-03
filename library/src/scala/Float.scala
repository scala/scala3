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

/** `Float`, a 32-bit IEEE-754 floating point number (equivalent to Java's `float` primitive type) is a
 *  subtype of [[scala.AnyVal]]. Instances of `Float` are not
 *  represented by an object in the underlying runtime system.
 *
 *  There is an implicit conversion from [[scala.Float]] => [[scala.runtime.RichFloat]]
 *  which provides useful non-primitive operations.
 */
final abstract class Float private extends AnyVal {
  def toByte: Byte
  def toShort: Short
  def toChar: Char
  def toInt: Int
  def toLong: Long
  def toFloat: Float
  def toDouble: Double

  /** Returns this value, unmodified. */
  def unary_+ : Float
  /** Returns the negation of this value. */
  def unary_- : Float

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
  def +(x: Byte): Float
  /** Returns the sum of this value and `x`. */
  def +(x: Short): Float
  /** Returns the sum of this value and `x`. */
  def +(x: Char): Float
  /** Returns the sum of this value and `x`. */
  def +(x: Int): Float
  /** Returns the sum of this value and `x`. */
  def +(x: Long): Float
  /** Returns the sum of this value and `x`. */
  def +(x: Float): Float
  /** Returns the sum of this value and `x`. */
  def +(x: Double): Double

  /** Returns the difference of this value and `x`. */
  def -(x: Byte): Float
  /** Returns the difference of this value and `x`. */
  def -(x: Short): Float
  /** Returns the difference of this value and `x`. */
  def -(x: Char): Float
  /** Returns the difference of this value and `x`. */
  def -(x: Int): Float
  /** Returns the difference of this value and `x`. */
  def -(x: Long): Float
  /** Returns the difference of this value and `x`. */
  def -(x: Float): Float
  /** Returns the difference of this value and `x`. */
  def -(x: Double): Double

  /** Returns the product of this value and `x`. */
  def *(x: Byte): Float
  /** Returns the product of this value and `x`. */
  def *(x: Short): Float
  /** Returns the product of this value and `x`. */
  def *(x: Char): Float
  /** Returns the product of this value and `x`. */
  def *(x: Int): Float
  /** Returns the product of this value and `x`. */
  def *(x: Long): Float
  /** Returns the product of this value and `x`. */
  def *(x: Float): Float
  /** Returns the product of this value and `x`. */
  def *(x: Double): Double

  /** Returns the quotient of this value and `x`. */
  def /(x: Byte): Float
  /** Returns the quotient of this value and `x`. */
  def /(x: Short): Float
  /** Returns the quotient of this value and `x`. */
  def /(x: Char): Float
  /** Returns the quotient of this value and `x`. */
  def /(x: Int): Float
  /** Returns the quotient of this value and `x`. */
  def /(x: Long): Float
  /** Returns the quotient of this value and `x`. */
  def /(x: Float): Float
  /** Returns the quotient of this value and `x`. */
  def /(x: Double): Double

  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Byte): Float
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Short): Float
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Char): Float
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Int): Float
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Long): Float
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Float): Float
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Double): Double

  // Provide a more specific return type for Scaladoc
  override def getClass(): Class[Float] = ???
}

object Float extends AnyValCompanion {
  /** The smallest positive value greater than 0.0f which is
   *  representable as a Float.
   */
  final val MinPositiveValue = java.lang.Float.MIN_VALUE
  final val NaN              = java.lang.Float.NaN
  final val PositiveInfinity = java.lang.Float.POSITIVE_INFINITY
  final val NegativeInfinity = java.lang.Float.NEGATIVE_INFINITY

  /** The negative number with the greatest (finite) absolute value which is representable
   *  by a Float.  Note that it differs from [[java.lang.Float.MIN_VALUE]], which
   *  is the smallest positive value representable by a Float.  In Scala that number
   *  is called Float.MinPositiveValue.
   */
  final val MinValue = -java.lang.Float.MAX_VALUE

  /** The largest finite positive number representable as a Float. */
  final val MaxValue = java.lang.Float.MAX_VALUE

  /** Transforms a value type into a boxed reference type.
   *
   *  Runtime implementation determined by `scala.runtime.BoxesRunTime.boxToFloat`. See [[https://github.com/scala/scala src/library/scala/runtime/BoxesRunTime.java]].
   *
   *  @param  x   the Float to be boxed
   *  @return     a java.lang.Float offering `x` as its underlying value.
   */
  def box(x: Float): java.lang.Float = ???

  /** Transforms a boxed type into a value type.  Note that this
   *  method is not typesafe: it accepts any Object, but will throw
   *  an exception if the argument is not a java.lang.Float.
   *
   *  Runtime implementation determined by `scala.runtime.BoxesRunTime.unboxToFloat`. See [[https://github.com/scala/scala src/library/scala/runtime/BoxesRunTime.java]].
   *
   *  @param  x   the java.lang.Float to be unboxed.
   *  @throws     ClassCastException  if the argument is not a java.lang.Float
   *  @return     the Float resulting from calling floatValue() on `x`
   */
  def unbox(x: java.lang.Object): Float = ???

  /** The String representation of the scala.Float companion object. */
  override def toString = "object scala.Float"
  /** Language mandated coercions from Float to "wider" types. */
  import scala.language.implicitConversions
  implicit def float2double(x: Float): Double = x.toDouble

  extension (self: Float) {
    /** Returns `'''true'''` if this number is finite and has no decimal component. */
    def isWhole: Boolean = {
      val i = self.toInt
      i.toFloat == self || i == Int.MaxValue && self < Float.PositiveInfinity || i == Int.MinValue && self > Float.NegativeInfinity
    }

    /** Returns `true` iff this has a zero fractional part, and is within the
      * range of [[scala.Char]] MinValue and MaxValue; otherwise returns `false`.
      */
    def isValidChar: Boolean = self.toChar.toFloat == self

    /** Returns `true` iff this has a zero fractional part, and is within the
      * range of [[scala.Byte]] MinValue and MaxValue; otherwise returns `false`.
      */
    def isValidByte: Boolean = self.toByte.toFloat == self

    /** Returns `true` iff this has a zero fractional part, and is within the
      * range of [[scala.Short]] MinValue and MaxValue; otherwise returns `false`.
      */
    def isValidShort: Boolean = self.toShort.toFloat == self

    /** Returns `true` iff this has a zero fractional part, and is within the
      * range of [[scala.Int]] MinValue and MaxValue; otherwise returns `false`.
      */
    def isValidInt: Boolean = self.toInt.toDouble == self.toDouble

    /** Returns `true` iff `this` is a `NaN` value. */
    def isNaN: Boolean = java.lang.Float.isNaN(self)

    /** Returns `true` iff `this` is `PositiveInfinity` or `NegativeInfinity`. */
    def isInfinity: Boolean = java.lang.Float.isInfinite(self)

    /** Returns `true` iff `this` is a finite value, i.e., not an infinity nor `NaN`. */
    def isFinite: Boolean = java.lang.Float.isFinite(self)

    /** Returns `true` iff `this` is `PositiveInfinity`. */
    def isPosInfinity: Boolean = Float.PositiveInfinity == self

    /** Returns `true` iff `this` is `NegativeInfinity`. */
    def isNegInfinity: Boolean = Float.NegativeInfinity == self

    /** Returns the absolute value of `this`. */
    def abs: Float = java.lang.Math.abs(self)

    /** Returns `this` if `this > that` or `that` otherwise. */
    def max(that: Float): Float = java.lang.Math.max(self, that)

    /** Returns `this` if `this < that` or `that` otherwise. */
    def min(that: Float): Float = java.lang.Math.min(self, that)

    /** Returns the sign of `this`.
      *
      * - `1.0f` if `this > 0.0f`;
      * - `-1.0f` if `this < 0.0f`;
      * - `this` otherwise (for zeros and `NaN`).
      */
    def sign: Float = java.lang.Math.signum(self)

    /** Returns the signum of `this`. */
    @deprecated("signum does not handle -0.0f or Double.NaN; use `sign` method instead", since = "2.13.0")
    def signum: Int = self.sign.toInt

    /** Returns the closest `Int` to `this`. */
    def round: Int = java.lang.Math.round(self)

    /** Returns the smallest integer greater or equal to `this`. */
    def ceil: Float = java.lang.Math.ceil(self.toDouble).toFloat

    /** Returns the largest integer smaller or equal to `this`. */
    def floor: Float = java.lang.Math.floor(self.toDouble).toFloat

    /** Converts an angle measured in degrees to an approximately equivalent
     *  angle measured in radians.
     *
     *  @return the measurement of the angle x in radians.
     */
    def toRadians: Float = java.lang.Math.toRadians(self.toDouble).toFloat

    /** Converts an angle measured in radians to an approximately equivalent
     *  angle measured in degrees.
     *  @return the measurement of the angle x in degrees.
     */
    def toDegrees: Float = java.lang.Math.toDegrees(self.toDouble).toFloat

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
    def compare(that: Float): Int = java.lang.Float.compare(self, that)
  }
}
