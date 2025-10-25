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

/** `Byte`, a 8-bit signed integer (equivalent to Java's `byte` primitive type) is a
 *  subtype of [[scala.AnyVal]]. Instances of `Byte` are not
 *  represented by an object in the underlying runtime system.
 *
 *  There is an implicit conversion from [[scala.Byte]] => [[scala.runtime.RichByte]]
 *  which provides useful non-primitive operations.
 */
final abstract class Byte private extends AnyVal {
  def toByte: Byte
  def toShort: Short
  def toChar: Char
  def toInt: Int
  def toLong: Long
  def toFloat: Float
  def toDouble: Double

  /**
 * Returns the bitwise negation of this value.
 * @example {{{
 * ~5 == -6
 * // in binary: ~00000101 ==
 * //             11111010
 * }}}
 */
  def unary_~ : Int
  /** Returns this value, unmodified. */
  def unary_+ : Int
  /** Returns the negation of this value. */
  def unary_- : Int

  @deprecated("Adding a number and a String is deprecated. Use the string interpolation `s\"$num$str\"`", "2.13.0")
  def +(x: String): String

  /**
  * Returns this value bit-shifted left by the specified number of bits,
  *         filling in the new right bits with zeroes.
  * @example {{{ 6 << 3 == 48 // in binary: 0110 << 3 == 0110000 }}}
  */
  def <<(x: Int): Int
  /**
  * Returns this value bit-shifted left by the specified number of bits,
  *         filling in the new right bits with zeroes.
  * @example {{{ 6 << 3 == 48 // in binary: 0110 << 3 == 0110000 }}}
  */
  @deprecated("shifting a value by a `Long` argument is deprecated (except when the value is a `Long`).\nCall `toInt` on the argument to maintain the current behavior and avoid the deprecation warning.", "2.12.7")
  def <<(x: Long): Int
  /**
  * Returns this value bit-shifted right by the specified number of bits,
  *         filling the new left bits with zeroes.
  * @example {{{ 21 >>> 3 == 2 // in binary: 010101 >>> 3 == 010 }}}
  * @example {{{
  * -21 >>> 3 == 536870909
  * // in binary: 11111111 11111111 11111111 11101011 >>> 3 ==
  * //            00011111 11111111 11111111 11111101
  * }}}
  */
  def >>>(x: Int): Int
  /**
  * Returns this value bit-shifted right by the specified number of bits,
  *         filling the new left bits with zeroes.
  * @example {{{ 21 >>> 3 == 2 // in binary: 010101 >>> 3 == 010 }}}
  * @example {{{
  * -21 >>> 3 == 536870909
  * // in binary: 11111111 11111111 11111111 11101011 >>> 3 ==
  * //            00011111 11111111 11111111 11111101
  * }}}
  */
  @deprecated("shifting a value by a `Long` argument is deprecated (except when the value is a `Long`).\nCall `toInt` on the argument to maintain the current behavior and avoid the deprecation warning.", "2.12.7")
  def >>>(x: Long): Int
  /**
  * Returns this value bit-shifted right by the specified number of bits,
  *         filling in the left bits with the same value as the left-most bit of this.
  *         The effect of this is to retain the sign of the value.
  * @example {{{
  * -21 >> 3 == -3
  * // in binary: 11111111 11111111 11111111 11101011 >> 3 ==
  * //            11111111 11111111 11111111 11111101
  * }}}
  */
  def >>(x: Int): Int
  /**
  * Returns this value bit-shifted right by the specified number of bits,
  *         filling in the left bits with the same value as the left-most bit of this.
  *         The effect of this is to retain the sign of the value.
  * @example {{{
  * -21 >> 3 == -3
  * // in binary: 11111111 11111111 11111111 11101011 >> 3 ==
  * //            11111111 11111111 11111111 11111101
  * }}}
  */
  @deprecated("shifting a value by a `Long` argument is deprecated (except when the value is a `Long`).\nCall `toInt` on the argument to maintain the current behavior and avoid the deprecation warning.", "2.12.7")
  def >>(x: Long): Int

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

  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary:   11110000
  * //            | 10101010
  * //              --------
  * //              11111010
  * }}}
  */
  def |(x: Byte): Int
  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary:   11110000
  * //            | 10101010
  * //              --------
  * //              11111010
  * }}}
  */
  def |(x: Short): Int
  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary:   11110000
  * //            | 10101010
  * //              --------
  * //              11111010
  * }}}
  */
  def |(x: Char): Int
  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary:   11110000
  * //            | 10101010
  * //              --------
  * //              11111010
  * }}}
  */
  def |(x: Int): Int
  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary:   11110000
  * //            | 10101010
  * //              --------
  * //              11111010
  * }}}
  */
  def |(x: Long): Long

  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary:   11110000
  * //            & 10101010
  * //              --------
  * //              10100000
  * }}}
  */
  def &(x: Byte): Int
  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary:   11110000
  * //            & 10101010
  * //              --------
  * //              10100000
  * }}}
  */
  def &(x: Short): Int
  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary:   11110000
  * //            & 10101010
  * //              --------
  * //              10100000
  * }}}
  */
  def &(x: Char): Int
  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary:   11110000
  * //            & 10101010
  * //              --------
  * //              10100000
  * }}}
  */
  def &(x: Int): Int
  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary:   11110000
  * //            & 10101010
  * //              --------
  * //              10100000
  * }}}
  */
  def &(x: Long): Long

  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary:   11110000
  * //            ^ 10101010
  * //              --------
  * //              01011010
  * }}}
  */
  def ^(x: Byte): Int
  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary:   11110000
  * //            ^ 10101010
  * //              --------
  * //              01011010
  * }}}
  */
  def ^(x: Short): Int
  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary:   11110000
  * //            ^ 10101010
  * //              --------
  * //              01011010
  * }}}
  */
  def ^(x: Char): Int
  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary:   11110000
  * //            ^ 10101010
  * //              --------
  * //              01011010
  * }}}
  */
  def ^(x: Int): Int
  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary:   11110000
  * //            ^ 10101010
  * //              --------
  * //              01011010
  * }}}
  */
  def ^(x: Long): Long

  /** Returns the sum of this value and `x`. */
  def +(x: Byte): Int
  /** Returns the sum of this value and `x`. */
  def +(x: Short): Int
  /** Returns the sum of this value and `x`. */
  def +(x: Char): Int
  /** Returns the sum of this value and `x`. */
  def +(x: Int): Int
  /** Returns the sum of this value and `x`. */
  def +(x: Long): Long
  /** Returns the sum of this value and `x`. */
  def +(x: Float): Float
  /** Returns the sum of this value and `x`. */
  def +(x: Double): Double

  /** Returns the difference of this value and `x`. */
  def -(x: Byte): Int
  /** Returns the difference of this value and `x`. */
  def -(x: Short): Int
  /** Returns the difference of this value and `x`. */
  def -(x: Char): Int
  /** Returns the difference of this value and `x`. */
  def -(x: Int): Int
  /** Returns the difference of this value and `x`. */
  def -(x: Long): Long
  /** Returns the difference of this value and `x`. */
  def -(x: Float): Float
  /** Returns the difference of this value and `x`. */
  def -(x: Double): Double

  /** Returns the product of this value and `x`. */
  def *(x: Byte): Int
  /** Returns the product of this value and `x`. */
  def *(x: Short): Int
  /** Returns the product of this value and `x`. */
  def *(x: Char): Int
  /** Returns the product of this value and `x`. */
  def *(x: Int): Int
  /** Returns the product of this value and `x`. */
  def *(x: Long): Long
  /** Returns the product of this value and `x`. */
  def *(x: Float): Float
  /** Returns the product of this value and `x`. */
  def *(x: Double): Double

  /** Returns the quotient of this value and `x`. */
  def /(x: Byte): Int
  /** Returns the quotient of this value and `x`. */
  def /(x: Short): Int
  /** Returns the quotient of this value and `x`. */
  def /(x: Char): Int
  /** Returns the quotient of this value and `x`. */
  def /(x: Int): Int
  /** Returns the quotient of this value and `x`. */
  def /(x: Long): Long
  /** Returns the quotient of this value and `x`. */
  def /(x: Float): Float
  /** Returns the quotient of this value and `x`. */
  def /(x: Double): Double

  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Byte): Int
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Short): Int
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Char): Int
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Int): Int
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Long): Long
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Float): Float
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Double): Double

  // Provide a more specific return type for Scaladoc
  override def getClass(): Class[Byte] = ???
}

object Byte extends AnyValCompanion {
  /** The smallest value representable as a Byte. */
  final val MinValue = java.lang.Byte.MIN_VALUE

  /** The largest value representable as a Byte. */
  final val MaxValue = java.lang.Byte.MAX_VALUE

  /** Transforms a value type into a boxed reference type.
   *
   *  Runtime implementation determined by `scala.runtime.BoxesRunTime.boxToByte`. See [[https://github.com/scala/scala src/library/scala/runtime/BoxesRunTime.java]].
   *
   *  @param  x   the Byte to be boxed
   *  @return     a java.lang.Byte offering `x` as its underlying value.
   */
  def box(x: Byte): java.lang.Byte = ???

  /** Transforms a boxed type into a value type.  Note that this
   *  method is not typesafe: it accepts any Object, but will throw
   *  an exception if the argument is not a java.lang.Byte.
   *
   *  Runtime implementation determined by `scala.runtime.BoxesRunTime.unboxToByte`. See [[https://github.com/scala/scala src/library/scala/runtime/BoxesRunTime.java]].
   *
   *  @param  x   the java.lang.Byte to be unboxed.
   *  @throws     ClassCastException  if the argument is not a java.lang.Byte
   *  @return     the Byte resulting from calling byteValue() on `x`
   */
  def unbox(x: java.lang.Object): Byte = ???

  /** The String representation of the scala.Byte companion object. */
  override def toString = "object scala.Byte"
  /** Language mandated coercions from Byte to "wider" types. */
  import scala.language.implicitConversions
  implicit def byte2short(x: Byte): Short = x.toShort
  implicit def byte2int(x: Byte): Int = x.toInt
  implicit def byte2long(x: Byte): Long = x.toLong
  implicit def byte2float(x: Byte): Float = x.toFloat
  implicit def byte2double(x: Byte): Double = x.toDouble

  extension (self: Byte) {
    /** Returns `'''true'''` if this number has no decimal component.
      * Always `'''true'''` for `Byte`.
      */
    @deprecated("isWhole on Byte is always true", "2.12.15")
    def isWhole: Boolean = true

    /** Returns `true` iff this is within the
      * range of [[scala.Char]] MinValue and MaxValue; otherwise returns `false`.
      */
    def isValidChar: Boolean = self >= 0

    /** Returns `true` iff this is within the
      * range of [[scala.Byte]] MinValue and MaxValue; otherwise returns `false`.
      */
    @deprecated("isValidByte on Byte is always true", "3.8.0")
    def isValidByte: Boolean = true

    /** Returns `true` iff this is within the
      * range of [[scala.Short]] MinValue and MaxValue; otherwise returns `false`.
      */
    @deprecated("isValidShort on Byte is always true", "3.8.0")
    def isValidShort: Boolean = true

    /** Returns `true` iff this is within the
      * range of [[scala.Int]] MinValue and MaxValue; otherwise returns `false`.
      */
    @deprecated("isValidInt on Byte is always true", "3.8.0")
    def isValidInt: Boolean = true

    /** Returns the absolute value of `this`. */
    def abs: Byte = java.lang.Math.abs(self.toInt).toByte

    /** Returns `this` if `this > that` or `that` otherwise. */
    def max(that: Byte): Byte = java.lang.Math.max(self.toInt, that.toInt).toByte

    /** Returns `this` if `this < that` or `that` otherwise. */
    def min(that: Byte): Byte = java.lang.Math.min(self.toInt, that.toInt).toByte

    /** Returns the sign of `this`.
      *
      * `0` if `this == 0`, `-1` if `this < 0` and `1` if `this > 0`.
      */
    def sign: Byte = java.lang.Integer.signum(self.toInt).toByte

    /** Returns the signum of `this`. */
    @deprecated("use `sign` method instead", since = "2.13.0")
    def signum: Int = self.sign.toInt

    /** Compares `this` to `that` according to the standard total ordering.
      *
      * Returns:
      * - a positive value if `this > that`
      * - a negative value if `this < that`
      * - `0` if `this == that`
      */
    def compare(that: Byte): Int = java.lang.Byte.compare(self, that)

    /** A [[scala.collection.immutable.NumericRange]] from `this` up to but not including `end`.
      *
      * @param end The final bound of the range to make.
      */
    def until(end: Byte): NumericRange.Exclusive[Byte] = NumericRange(self, end, 1)

    /** A [[scala.collection.immutable.NumericRange]] from `this` up to but not including `end`.
      *
      * @param end The final bound of the range to make.
      * @param step The number to increase by for each step of the range.
      */
    def until(end: Byte, step: Byte): NumericRange.Exclusive[Byte] = NumericRange(self, end, step)

    /** A [[scala.collection.immutable.NumericRange]] from `this` up to and including `end`.
      *
      * @param end The final bound of the range to make.
      */
    def to(end: Byte): NumericRange.Inclusive[Byte] = NumericRange.inclusive(self, end, 1)

    /** A [[scala.collection.immutable.NumericRange]] from `this` up to and including `end`.
      *
      * @param end The final bound of the range to make.
      * @param step The number to increase by for each step of the range.
      */
    def to(end: Byte, step: Byte): NumericRange.Inclusive[Byte] = NumericRange.inclusive(self, end, step)

    // ------------------------------------
    // For source compatibility with the previous API, when the rhs is an Int, we want an int Range

    /** A [[scala.collection.immutable.Range]] from `this` up to but not including `end`.
      *
      * @param end The final bound of the range to make.
      */
    def until(end: Int): Range = Range(self.toInt, end, 1)

    /** A [[scala.collection.immutable.Range]] from `this` up to but not including `end`.
      *
      * @param end The final bound of the range to make.
      * @param step The number to increase by for each step of the range.
      */
    def until(end: Int, step: Int): Range = Range(self.toInt, end, step)

    /** A [[scala.collection.immutable.Range]] from `this` up to and including `end`.
      *
      * @param end The final bound of the range to make.
      */
    def to(end: Int): Range.Inclusive = Range.inclusive(self.toInt, end, 1)

    /** A [[scala.collection.immutable.Range]] from `this` up to and including `end`.
      *
      * @param end The final bound of the range to make.
      * @param step The number to increase by for each step of the range.
      */
    def to(end: Int, step: Int): Range.Inclusive = Range.inclusive(self.toInt, end, step)
  }
}
