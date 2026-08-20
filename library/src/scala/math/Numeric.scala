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
package math

import scala.language.`2.13`
import scala.collection.StringParsers
import scala.language.implicitConversions
import scala.util.Try

object Numeric {
  /** Returns the implicit `Numeric` instance for type `T`.
   *
   *  @tparam T the numeric type for which an implicit `Numeric` instance is requested
   *  @param num the implicit `Numeric` instance for type `T`
   *  @return the `Numeric` instance for type `T`
   */
  @inline def apply[T](implicit num: Numeric[T]): Numeric[T] = num

  /** Provides additional implicit conversions for numeric types. */
  trait ExtraImplicits {
    /** These implicits create conversions from a value for which an implicit Numeric
     *  exists to the inner class which creates infix operations.  Once imported, you
     *  can write methods as follows:
     *  ```scala sc:compile
     *  import scala.math.Numeric.Implicits.*
     *  def plus[T: Numeric](x: T, y: T) = x + y
     *  ```
     *
     *  @tparam T the numeric type for which a `Numeric` instance exists
     *  @param x the value to wrap with numeric infix operations
     *  @param num the implicit `Numeric` instance that provides the arithmetic operations
     *
     *  @return a `NumericOps` wrapper around `x` that exposes infix arithmetic operators and numeric conversion methods
     */
    implicit def infixNumericOps[T](x: T)(implicit num: Numeric[T]): Numeric[T]#NumericOps = new num.NumericOps(x)
  }
  object Implicits extends ExtraImplicits { }

  /** Provides arithmetic operations for `BigInt` values. */
  trait BigIntIsIntegral extends Integral[BigInt] {
    /** Returns the sum of two `BigInt` values.
     *
     *  @param x the first `BigInt` value
     *  @param y the second `BigInt` value
     *  @return the sum of `x` and `y`
     */
    def plus(x: BigInt, y: BigInt): BigInt = x + y
    /** Returns the difference between two `BigInt` values.
     *
     *  @param x the first `BigInt` value
     *  @param y the second `BigInt` value
     *  @return the difference between `x` and `y`
     */
    def minus(x: BigInt, y: BigInt): BigInt = x - y
    /** Returns the product of two `BigInt` values.
     *
     *  @param x the first `BigInt` value
     *  @param y the second `BigInt` value
     *  @return the product of `x` and `y`
     */
    def times(x: BigInt, y: BigInt): BigInt = x * y
    /** Returns the quotient of two `BigInt` values.
     *
     *  @param x the first `BigInt` value
     *  @param y the second `BigInt` value
     *  @return the quotient of `x` divided by `y`
     *  @throws java.lang.ArithmeticException if `y` is zero
     */
    def quot(x: BigInt, y: BigInt): BigInt = x / y
    /** Returns the remainder of two `BigInt` values.
     *
     *  @param x the first `BigInt` value
     *  @param y the second `BigInt` value
     *  @return the remainder of `x` divided by `y`
     *  @throws java.lang.ArithmeticException if `y` is zero
     */
    def rem(x: BigInt, y: BigInt): BigInt = x % y
    /** Returns the negation of a `BigInt` value.
     *
     *  @param x the `BigInt` value to negate
     *  @return the negation of `x`
     */
    def negate(x: BigInt): BigInt = -x
    /** Converts an `Int` to a `BigInt`.
     *
     *  @param x the `Int` value to convert
     *  @return the `BigInt` representation of `x`
     */
    def fromInt(x: Int): BigInt = BigInt(x)
    /** Parses a `String` into a `BigInt`.
     *
     *  @param str the `String` to parse
     *  @return `Some(BigInt)` if the string is a valid `BigInt`, `None` otherwise
     */
    def parseString(str: String): Option[BigInt] = Try(BigInt(str)).toOption
    /** Converts a `BigInt` to an `Int`.
     *
     *  @param x the `BigInt` value to convert
     *  @return the `Int` representation of `x`
     */
    def toInt(x: BigInt): Int = x.intValue
    /** Converts a `BigInt` to a `Long`.
     *
     *  @param x the `BigInt` value to convert
     *  @return the `Long` representation of `x`
     */
    def toLong(x: BigInt): Long = x.longValue
    /** Converts a `BigInt` to a `Float`.
     *
     *  @param x the `BigInt` value to convert
     *  @return the `Float` representation of `x`
     */
    def toFloat(x: BigInt): Float = x.floatValue
    /** Converts a `BigInt` to a `Double`.
     *
     *  @param x the `BigInt` value to convert
     *  @return the `Double` representation of `x`
     */
    def toDouble(x: BigInt): Double = x.doubleValue
  }
  implicit object BigIntIsIntegral extends BigIntIsIntegral with Ordering.BigIntOrdering

  /** Provides arithmetic operations for `Int` values. */
  trait IntIsIntegral extends Integral[Int] {
    /** Returns the sum of two `Int` values.
     *
     *  @param x the first `Int` value
     *  @param y the second `Int` value
     *  @return the sum of `x` and `y`
     */
    def plus(x: Int, y: Int): Int = x + y
    /** Returns the difference between two `Int` values.
     *
     *  @param x the first `Int` value
     *  @param y the second `Int` value
     *  @return the difference between `x` and `y`
     */
    def minus(x: Int, y: Int): Int = x - y
    /** Returns the product of two `Int` values.
     *
     *  @param x the first `Int` value
     *  @param y the second `Int` value
     *  @return the product of `x` and `y`
     */
    def times(x: Int, y: Int): Int = x * y
    /** Returns the quotient of two `Int` values.
     *
     *  @param x the first `Int` value
     *  @param y the second `Int` value
     *  @return the quotient of `x` divided by `y`
     *  @throws java.lang.ArithmeticException if `y` is zero
     */
    def quot(x: Int, y: Int): Int = x / y
    /** Returns the remainder of two `Int` values.
     *
     *  @param x the first `Int` value
     *  @param y the second `Int` value
     *  @return the remainder of `x` divided by `y`
     *  @throws java.lang.ArithmeticException if `y` is zero
     */
    def rem(x: Int, y: Int): Int = x % y
    /** Returns the negation of an `Int` value.
     *
     *  @param x the `Int` value to negate
     *  @return the negation of `x`
     */
    def negate(x: Int): Int = -x
    /** Returns the given `Int` value, as no conversion is needed.
     *
     *  @param x the `Int` value to convert
     *  @return the given `Int` value unchanged
     */
    def fromInt(x: Int): Int = x
    /** Parses a `String` into an `Int`.
     *
     *  @param str the `String` to parse
     *  @return `Some(Int)` if the string is a valid `Int`, `None` otherwise
     */
    def parseString(str: String): Option[Int] = StringParsers.parseInt(str)
    /** Returns the given `Int` value, as no conversion is needed.
     *
     *  @param x the `Int` value to convert
     *  @return the given `Int` value unchanged
     */
    def toInt(x: Int): Int = x
    /** Converts an `Int` to a `Long`.
     *
     *  @param x the `Int` value to convert
     *  @return the `Long` representation of `x`
     */
    def toLong(x: Int): Long = x.toLong
    /** Converts an `Int` to a `Float`.
     *
     *  @param x the `Int` value to convert
     *  @return the `Float` representation of `x`
     */
    def toFloat(x: Int): Float = x.toFloat
    /** Converts an `Int` to a `Double`.
     *
     *  @param x the `Int` value to convert
     *  @return the `Double` representation of `x`
     */
    def toDouble(x: Int): Double = x.toDouble
    /** Returns the signum of an `Int` value.
     *
     *  @param x the `Int` value
     *  @return -1 if `x` is negative, 1 if `x` is positive, 0 if `x` is zero
     */
    override def signum(x: Int): Int = math.signum(x)
    /** Returns the sign of an `Int` value.
     *
     *  @param x the `Int` value
     *  @return -1 if `x` is negative, 1 if `x` is positive, 0 if `x` is zero
     */
    override def sign(x: Int): Int = math.signum(x)
  }
  implicit object IntIsIntegral extends IntIsIntegral with Ordering.IntOrdering

  /** Provides arithmetic operations for `Short` values. */
  trait ShortIsIntegral extends Integral[Short] {
    /** Returns the sum of two `Short` values.
     *
     *  @param x the first `Short` value
     *  @param y the second `Short` value
     *  @return the sum of `x` and `y`
     */
    def plus(x: Short, y: Short): Short = (x + y).toShort
    /** Returns the difference between two `Short` values.
     *
     *  @param x the first `Short` value
     *  @param y the second `Short` value
     *  @return the difference between `x` and `y`
     */
    def minus(x: Short, y: Short): Short = (x - y).toShort
    /** Returns the product of two `Short` values.
     *
     *  @param x the first `Short` value
     *  @param y the second `Short` value
     *  @return the product of `x` and `y`
     */
    def times(x: Short, y: Short): Short = (x * y).toShort
    /** Returns the quotient of two `Short` values.
     *
     *  @param x the first `Short` value
     *  @param y the second `Short` value
     *  @return the quotient of `x` divided by `y`
     *  @throws java.lang.ArithmeticException if `y` is zero
     */
    def quot(x: Short, y: Short): Short = (x / y).toShort
    /** Returns the remainder of two `Short` values.
     *
     *  @param x the first `Short` value
     *  @param y the second `Short` value
     *  @return the remainder of `x` divided by `y`
     *  @throws java.lang.ArithmeticException if `y` is zero
     */
    def rem(x: Short, y: Short): Short = (x % y).toShort
    /** Returns the negation of a `Short` value.
     *
     *  @param x the `Short` value to negate
     *  @return the negation of `x`
     */
    def negate(x: Short): Short = (-x).toShort
    /** Converts an `Int` to a `Short`.
     *
     *  @param x the `Int` value to convert
     *  @return the `Short` representation of `x`
     */
    def fromInt(x: Int): Short = x.toShort
    /** Parses a `String` into a `Short`.
     *
     *  @param str the `String` to parse
     *  @return `Some(Short)` if the string is a valid `Short`, `None` otherwise
     */
    def parseString(str: String): Option[Short] = StringParsers.parseShort(str)
    /** Converts a `Short` to an `Int`.
     *
     *  @param x the `Short` value to convert
     *  @return the `Int` representation of `x`
     */
    def toInt(x: Short): Int = x.toInt
    /** Converts a `Short` to a `Long`.
     *
     *  @param x the `Short` value to convert
     *  @return the `Long` representation of `x`
     */
    def toLong(x: Short): Long = x.toLong
    /** Converts a `Short` to a `Float`.
     *
     *  @param x the `Short` value to convert
     *  @return the `Float` representation of `x`
     */
    def toFloat(x: Short): Float = x.toFloat
    /** Converts a `Short` to a `Double`.
     *
     *  @param x the `Short` value to convert
     *  @return the `Double` representation of `x`
     */
    def toDouble(x: Short): Double = x.toDouble
    /** Returns the signum of a `Short` value.
     *
     *  @param x the `Short` value
     *  @return -1 if `x` is negative, 1 if `x` is positive, 0 if `x` is zero
     */
    override def signum(x: Short): Int = math.signum(x.toInt)
    /** Returns the sign of a `Short` value.
     *
     *  @param x the `Short` value
     *  @return -1 if `x` is negative, 1 if `x` is positive, 0 if `x` is zero
     */
    override def sign(x: Short): Short = math.signum(x.toInt).toShort
  }
  implicit object ShortIsIntegral extends ShortIsIntegral with Ordering.ShortOrdering

  /** Provides arithmetic operations for `Byte` values. */
  trait ByteIsIntegral extends Integral[Byte] {
    /** Returns the sum of two `Byte` values.
     *
     *  @param x the first `Byte` value
     *  @param y the second `Byte` value
     *  @return the sum of `x` and `y`
     */
    def plus(x: Byte, y: Byte): Byte = (x + y).toByte
    /** Returns the difference between two `Byte` values.
     *
     *  @param x the first `Byte` value
     *  @param y the second `Byte` value
     *  @return the difference between `x` and `y`
     */
    def minus(x: Byte, y: Byte): Byte = (x - y).toByte
    /** Returns the product of two `Byte` values.
     *
     *  @param x the first `Byte` value
     *  @param y the second `Byte` value
     *  @return the product of `x` and `y`
     */
    def times(x: Byte, y: Byte): Byte = (x * y).toByte
    /** Returns the quotient of two `Byte` values.
     *
     *  @param x the first `Byte` value
     *  @param y the second `Byte` value
     *  @return the quotient of `x` divided by `y`
     *  @throws java.lang.ArithmeticException if `y` is zero
     */
    def quot(x: Byte, y: Byte): Byte = (x / y).toByte
    /** Returns the remainder of two `Byte` values.
     *
     *  @param x the first `Byte` value
     *  @param y the second `Byte` value
     *  @return the remainder of `x` divided by `y`
     *  @throws java.lang.ArithmeticException if `y` is zero
     */
    def rem(x: Byte, y: Byte): Byte = (x % y).toByte
    /** Returns the negation of a `Byte` value.
     *
     *  @param x the `Byte` value to negate
     *  @return the negation of `x`
     */
    def negate(x: Byte): Byte = (-x).toByte
    /** Converts an `Int` to a `Byte`.
     *
     *  @param x the `Int` value to convert
     *  @return the `Byte` representation of `x`
     */
    def fromInt(x: Int): Byte = x.toByte
    /** Parses a `String` into a `Byte`.
     *
     *  @param str the `String` to parse
     *  @return `Some(Byte)` if the string is a valid `Byte`, `None` otherwise
     */
    def parseString(str: String): Option[Byte] = StringParsers.parseByte(str)
    /** Converts a `Byte` to an `Int`.
     *
     *  @param x the `Byte` value to convert
     *  @return the `Int` representation of `x`
     */
    def toInt(x: Byte): Int = x.toInt
    /** Converts a `Byte` to a `Long`.
     *
     *  @param x the `Byte` value to convert
     *  @return the `Long` representation of `x`
     */
    def toLong(x: Byte): Long = x.toLong
    /** Converts a `Byte` to a `Float`.
     *
     *  @param x the `Byte` value to convert
     *  @return the `Float` representation of `x`
     */
    def toFloat(x: Byte): Float = x.toFloat
    /** Converts a `Byte` to a `Double`.
     *
     *  @param x the `Byte` value to convert
     *  @return the `Double` representation of `x`
     */
    def toDouble(x: Byte): Double = x.toDouble
    /** Returns the signum of a `Byte` value.
     *
     *  @param x the `Byte` value
     *  @return -1 if `x` is negative, 1 if `x` is positive, 0 if `x` is zero
     */
    override def signum(x: Byte): Int = math.signum(x.toInt)
    /** Returns the sign of a `Byte` value.
     *
     *  @param x the `Byte` value
     *  @return -1 if `x` is negative, 1 if `x` is positive, 0 if `x` is zero
     */
    override def sign(x: Byte): Byte = math.signum(x.toInt).toByte
  }
  implicit object ByteIsIntegral extends ByteIsIntegral with Ordering.ByteOrdering

  /** Provides arithmetic operations for `Char` values. */
  trait CharIsIntegral extends Integral[Char] {
    /** Returns the sum of two `Char` values.
     *
     *  @param x the first `Char` value
     *  @param y the second `Char` value
     *  @return the sum of `x` and `y`
     */
    def plus(x: Char, y: Char): Char = (x + y).toChar
    /** Returns the difference between two `Char` values.
     *
     *  @param x the first `Char` value
     *  @param y the second `Char` value
     *  @return the difference between `x` and `y`
     */
    def minus(x: Char, y: Char): Char = (x - y).toChar
    /** Returns the product of two `Char` values.
     *
     *  @param x the first `Char` value
     *  @param y the second `Char` value
     *  @return the product of `x` and `y`
     */
    def times(x: Char, y: Char): Char = (x * y).toChar
    /** Returns the quotient of two `Char` values.
     *
     *  @param x the first `Char` value
     *  @param y the second `Char` value
     *  @return the quotient of `x` divided by `y`
     *  @throws java.lang.ArithmeticException if `y` is zero
     */
    def quot(x: Char, y: Char): Char = (x / y).toChar
    /** Returns the remainder of two `Char` values.
     *
     *  @param x the first `Char` value
     *  @param y the second `Char` value
     *  @return the remainder of `x` divided by `y`
     *  @throws java.lang.ArithmeticException if `y` is zero
     */
    def rem(x: Char, y: Char): Char = (x % y).toChar
    /** Returns the negation of a `Char` value.
     *
     *  @param x the `Char` value to negate
     *  @return the negation of `x`
     */
    def negate(x: Char): Char = (-x).toChar
    /** Converts an `Int` to a `Char`.
     *
     *  @param x the `Int` value to convert
     *  @return the `Char` representation of `x`
     */
    def fromInt(x: Int): Char = x.toChar
    /** Parses a `String` into a `Char` by interpreting it as a decimal integer and converting that integer to a `Char`.
     *
     *  @param str the `String` to parse
     *  @return `Some(Char)` if the string is a valid integer (any valid `Int` is truncated to a `Char` via narrowing conversion, so out-of-range integers wrap rather than failing), `None` otherwise
     */
    def parseString(str: String): Option[Char] = Try(str.toInt.toChar).toOption
    /** Converts a `Char` to an `Int`.
     *
     *  @param x the `Char` value to convert
     *  @return the `Int` representation of `x`
     */
    def toInt(x: Char): Int = x.toInt
    /** Converts a `Char` to a `Long`.
     *
     *  @param x the `Char` value to convert
     *  @return the `Long` representation of `x`
     */
    def toLong(x: Char): Long = x.toLong
    /** Converts a `Char` to a `Float`.
     *
     *  @param x the `Char` value to convert
     *  @return the `Float` representation of `x`
     */
    def toFloat(x: Char): Float = x.toFloat
    /** Converts a `Char` to a `Double`.
     *
     *  @param x the `Char` value to convert
     *  @return the `Double` representation of `x`
     */
    def toDouble(x: Char): Double = x.toDouble
    /** Returns the signum of a `Char` value.
     *
     *  @param x the `Char` value
     *  @return 1 if `x` is nonzero, 0 if `x` is zero (\\u0000) — `Char` values are unsigned.
     */
    override def signum(x: Char): Int = math.signum(x.toInt)
    /** Returns the sign of a `Char` value.
     *
     *  @param x the `Char` value
     *  @return \\u0001 if `x` is nonzero, \\u0000 if `x` is zero (\\u0000) — `Char` values are unsigned.
     */
    override def sign(x: Char): Char = math.signum(x.toInt).toChar
  }
  implicit object CharIsIntegral extends CharIsIntegral with Ordering.CharOrdering

  /** Provides arithmetic operations for `Long` values. */
  trait LongIsIntegral extends Integral[Long] {
    /** Returns the sum of two `Long` values.
     *
     *  @param x the first `Long` value
     *  @param y the second `Long` value
     *  @return the sum of `x` and `y`
     */
    def plus(x: Long, y: Long): Long = x + y
    /** Returns the difference between two `Long` values.
     *
     *  @param x the first `Long` value
     *  @param y the second `Long` value
     *  @return the difference between `x` and `y`
     */
    def minus(x: Long, y: Long): Long = x - y
    /** Returns the product of two `Long` values.
     *
     *  @param x the first `Long` value
     *  @param y the second `Long` value
     *  @return the product of `x` and `y`
     */
    def times(x: Long, y: Long): Long = x * y
    /** Returns the quotient of two `Long` values.
     *
     *  @param x the first `Long` value
     *  @param y the second `Long` value
     *  @return the quotient of `x` divided by `y`
     *  @throws java.lang.ArithmeticException if `y` is zero
     */
    def quot(x: Long, y: Long): Long = x / y
    /** Returns the remainder of two `Long` values.
     *
     *  @param x the first `Long` value
     *  @param y the second `Long` value
     *  @return the remainder of `x` divided by `y`
     *  @throws java.lang.ArithmeticException if `y` is zero
     */
    def rem(x: Long, y: Long): Long = x % y
    /** Returns the negation of a `Long` value.
     *
     *  @param x the `Long` value to negate
     *  @return the negation of `x`
     */
    def negate(x: Long): Long = -x
    /** Converts an `Int` to a `Long`.
     *
     *  @param x the `Int` value to convert
     *  @return the `Long` representation of `x`
     */
    def fromInt(x: Int): Long = x.toLong
    /** Parses a `String` into a `Long`.
     *
     *  @param str the `String` to parse
     *  @return `Some(Long)` if the string is a valid `Long`, `None` otherwise
     */
    def parseString(str: String): Option[Long] = StringParsers.parseLong(str)
    /** Converts a `Long` to an `Int`.
     *
     *  @param x the `Long` value to convert
     *  @return the `Int` representation of `x`
     */
    def toInt(x: Long): Int = x.toInt
    /** Returns the given `Long` value, as no conversion is needed.
     *
     *  @param x the `Long` value to convert
     *  @return the given `Long` value unchanged
     */
    def toLong(x: Long): Long = x
    /** Converts a `Long` to a `Float`.
     *
     *  @param x the `Long` value to convert
     *  @return the `Float` representation of `x`
     */
    def toFloat(x: Long): Float = x.toFloat
    /** Converts a `Long` to a `Double`.
     *
     *  @param x the `Long` value to convert
     *  @return the `Double` representation of `x`
     */
    def toDouble(x: Long): Double = x.toDouble
    /** Returns the signum of a `Long` value.
     *
     *  @param x the `Long` value
     *  @return -1 if `x` is negative, 1 if `x` is positive, 0 if `x` is zero
     */
    override def signum(x: Long): Int = math.signum(x).toInt
    /** Returns the sign of a `Long` value.
     *
     *  @param x the `Long` value
     *  @return -1 if `x` is negative, 1 if `x` is positive, 0 if `x` is zero
     */
    override def sign(x: Long): Long = math.signum(x)
  }
  implicit object LongIsIntegral extends LongIsIntegral with Ordering.LongOrdering

  /** Provides arithmetic operations for `Float` values. */
  trait FloatIsFractional extends Fractional[Float] {
    /** Returns the sum of two `Float` values.
     *
     *  @param x the first `Float` value
     *  @param y the second `Float` value
     *  @return the sum of `x` and `y`
     */
    def plus(x: Float, y: Float): Float = x + y
    /** Returns the difference between two `Float` values.
     *
     *  @param x the first `Float` value
     *  @param y the second `Float` value
     *  @return the difference between `x` and `y`
     */
    def minus(x: Float, y: Float): Float = x - y
    /** Returns the product of two `Float` values.
     *
     *  @param x the first `Float` value
     *  @param y the second `Float` value
     *  @return the product of `x` and `y`
     */
    def times(x: Float, y: Float): Float = x * y
    /** Returns the negation of a `Float` value.
     *
     *  @param x the `Float` value to negate
     *  @return the negation of `x`
     */
    def negate(x: Float): Float = -x
    /** Converts an `Int` to a `Float`.
     *
     *  @param x the `Int` value to convert
     *  @return the `Float` representation of `x`
     */
    def fromInt(x: Int): Float = x.toFloat
    /** Parses a `String` into a `Float`.
     *
     *  @param str the `String` to parse
     *  @return `Some(Float)` if the string is a valid `Float`, `None` otherwise
     */
    def parseString(str: String): Option[Float] = StringParsers.parseFloat(str)
    /** Converts a `Float` to an `Int`.
     *
     *  @param x the `Float` value to convert
     *  @return the `Int` representation of `x`
     */
    def toInt(x: Float): Int = x.toInt
    /** Converts a `Float` to a `Long`.
     *
     *  @param x the `Float` value to convert
     *  @return the `Long` representation of `x`
     */
    def toLong(x: Float): Long = x.toLong
    /** Returns the given `Float` value, as no conversion is needed.
     *
     *  @param x the `Float` value (returned unchanged)
     *  @return the given `Float` value unchanged
     */
    def toFloat(x: Float): Float = x
    /** Converts a `Float` to a `Double`.
     *
     *  @param x the `Float` value to convert
     *  @return the `Double` representation of `x`
     */
    def toDouble(x: Float): Double = x.toDouble
    /** Returns the quotient of two `Float` values.
     *
     *  @param x the first `Float` value
     *  @param y the second `Float` value
     *  @return the quotient of `x` divided by `y`
     */
    def div(x: Float, y: Float): Float = x / y
    // logic in Numeric base trait mishandles abs(-0.0f)
    /** Returns the absolute value of a `Float`.
     *
     *  @param x the `Float` value
     *  @return the absolute value of `x`
     */
    override def abs(x: Float): Float = math.abs(x)
    // logic in Numeric base trait mishandles sign(-0.0f) and sign(Float.NaN)
    /** Returns the sign of a `Float` value.
     *
     *  @param x the `Float` value
     *  @return -1.0 if `x` is negative, 1.0 if `x` is positive, 0.0 if `x` is zero, or `NaN` if `x` is `NaN`
     */
    override def sign(x: Float): Float = math.signum(x)
  }
  implicit object FloatIsFractional extends FloatIsFractional with Ordering.Float.IeeeOrdering

  /** Provides arithmetic operations for `Double` values. */
  trait DoubleIsFractional extends Fractional[Double] {
    /** Returns the sum of two `Double` values.
     *
     *  @param x the first `Double` value
     *  @param y the second `Double` value
     *  @return the sum of `x` and `y`
     */
    def plus(x: Double, y: Double): Double = x + y
    /** Returns the difference between two `Double` values.
     *
     *  @param x the first `Double` value
     *  @param y the second `Double` value
     *  @return the difference between `x` and `y`
     */
    def minus(x: Double, y: Double): Double = x - y
    /** Returns the product of two `Double` values.
     *
     *  @param x the first `Double` value
     *  @param y the second `Double` value
     *  @return the product of `x` and `y`
     */
    def times(x: Double, y: Double): Double = x * y
    /** Returns the negation of a `Double` value.
     *
     *  @param x the `Double` value to negate
     *  @return the negation of `x`
     */
    def negate(x: Double): Double = -x
    /** Converts an `Int` to a `Double`.
     *
     *  @param x the `Int` value to convert
     *  @return the `Double` representation of `x`
     */
    def fromInt(x: Int): Double = x.toDouble
    /** Parses a `String` into a `Double`.
     *
     *  @param str the `String` to parse
     *  @return `Some(Double)` if the string is a valid `Double`, `None` otherwise
     */
    def parseString(str: String): Option[Double] = StringParsers.parseDouble(str)
    /** Converts a `Double` to an `Int`.
     *
     *  @param x the `Double` value to convert
     *  @return the `Int` representation of `x`
     */
    def toInt(x: Double): Int = x.toInt
    /** Converts a `Double` to a `Long`.
     *
     *  @param x the `Double` value to convert
     *  @return the `Long` representation of `x`
     */
    def toLong(x: Double): Long = x.toLong
    /** Converts a `Double` to a `Float`.
     *
     *  @param x the `Double` value to convert
     *  @return the `Float` representation of `x`
     */
    def toFloat(x: Double): Float = x.toFloat
    /** Returns the given `Double` value, as no conversion is needed.
     *
     *  @param x the `Double` value (returned unchanged)
     *  @return the given `Double` value unchanged
     */
    def toDouble(x: Double): Double = x
    /** Returns the quotient of two `Double` values.
     *
     *  @param x the first `Double` value
     *  @param y the second `Double` value
     *  @return the quotient of `x` divided by `y`
     */
    def div(x: Double, y: Double): Double = x / y
    // logic in Numeric base trait mishandles abs(-0.0)
    /** Returns the absolute value of a `Double`.
     *
     *  @param x the `Double` value
     *  @return the absolute value of `x`
     */
    override def abs(x: Double): Double = math.abs(x)
    // logic in Numeric base trait mishandles sign(-0.0) and sign(Double.NaN)
    /** Returns the sign of a `Double` value.
     *
     *  @param x the `Double` value
     *  @return -1.0 if `x` is negative, 1.0 if `x` is positive, 0.0 if `x` is zero, or `NaN` if `x` is `NaN`
     */
    override def sign(x: Double): Double = math.signum(x)
  }
  implicit object DoubleIsFractional extends DoubleIsFractional with Ordering.Double.IeeeOrdering

  /** Provides arithmetic operations for `BigDecimal` values. */
  trait BigDecimalIsConflicted extends Numeric[BigDecimal] {
    // works around pollution of math context by ignoring identity element
    /** Returns the sum of two `BigDecimal` values.
     *
     *  @param x the first `BigDecimal` value
     *  @param y the second `BigDecimal` value
     *  @return the sum of `x` and `y`
     *  @note Returns the second value directly if the first value is the cached zero instance (BigDecimal(0) under the default MathContext) to avoid math context pollution.
     */
    def plus(x: BigDecimal, y: BigDecimal): BigDecimal = {
      import BigDecimalIsConflicted._0
      if (x eq _0) y else x + y
    }
    /** Returns the difference between two `BigDecimal` values.
     *
     *  @param x the first `BigDecimal` value
     *  @param y the second `BigDecimal` value
     *  @return the difference between `x` and `y`
     *  @note Returns the negation of the second value directly if the first value is the cached zero instance (BigDecimal(0) under the default MathContext) to avoid math context pollution.
     */
    def minus(x: BigDecimal, y: BigDecimal): BigDecimal = {
      import BigDecimalIsConflicted._0
      if (x eq _0) -y else x - y
    }
    // works around pollution of math context by ignoring identity element
    /** Returns the product of two `BigDecimal` values.
     *
     *  @param x the first `BigDecimal` value
     *  @param y the second `BigDecimal` value
     *  @return the product of `x` and `y`
     *  @note Returns the second value directly if the first value is the cached one instance (BigDecimal(1) under the default MathContext) to avoid math context pollution.
     */
    def times(x: BigDecimal, y: BigDecimal): BigDecimal = {
      import BigDecimalIsConflicted._1
      if (x eq _1) y else x * y
    }
    /** Returns the negation of a `BigDecimal` value.
     *
     *  @param x the `BigDecimal` value to negate
     *  @return the negation of `x`
     */
    def negate(x: BigDecimal): BigDecimal = -x
    /** Converts an `Int` to a `BigDecimal`.
     *
     *  @param x the `Int` value to convert
     *  @return the `BigDecimal` representation of `x`
     */
    def fromInt(x: Int): BigDecimal = BigDecimal(x)
    /** Parses a `String` into a `BigDecimal`.
     *
     *  @param str the `String` to parse
     *  @return `Some(BigDecimal)` if the string is a valid `BigDecimal`, `None` otherwise
     */
    def parseString(str: String): Option[BigDecimal] = Try(BigDecimal(str)).toOption
    /** Converts a `BigDecimal` to an `Int`.
     *
     *  @param x the `BigDecimal` value to convert
     *  @return the `Int` representation of `x`
     */
    def toInt(x: BigDecimal): Int = x.intValue
    /** Converts a `BigDecimal` to a `Long`.
     *
     *  @param x the `BigDecimal` value to convert
     *  @return the `Long` representation of `x`
     */
    def toLong(x: BigDecimal): Long = x.longValue
    /** Converts a `BigDecimal` to a `Float`.
     *
     *  @param x the `BigDecimal` value to convert
     *  @return the `Float` representation of `x`
     */
    def toFloat(x: BigDecimal): Float = x.floatValue
    /** Converts a `BigDecimal` to a `Double`.
     *
     *  @param x the `BigDecimal` value to convert
     *  @return the `Double` representation of `x`
     */
    def toDouble(x: BigDecimal): Double = x.doubleValue
  }
  private object BigDecimalIsConflicted {
    private val _0 = BigDecimal(0)   // cached zero is ordinarily cached for default math context
    private val _1 = BigDecimal(1)   // cached one is ordinarily cached for default math context
  }

  /** Provides fractional arithmetic operations for `BigDecimal` values. */
  trait BigDecimalIsFractional extends BigDecimalIsConflicted with Fractional[BigDecimal] {
    /** Returns the quotient of two `BigDecimal` values.
     *
     *  @param x the first `BigDecimal` value
     *  @param y the second `BigDecimal` value
     *  @return the quotient of `x` divided by `y`
     *  @throws java.lang.ArithmeticException if `y` is zero
     */
    def div(x: BigDecimal, y: BigDecimal): BigDecimal = x / y
  }
  /** Provides integral arithmetic operations for `BigDecimal` values. */
  trait BigDecimalAsIfIntegral extends BigDecimalIsConflicted with Integral[BigDecimal] {
    /** Returns the quotient of two `BigDecimal` values.
     *
     *  @param x the first `BigDecimal` value
     *  @param y the second `BigDecimal` value
     *  @return the quotient of `x` divided by `y`
     *  @throws java.lang.ArithmeticException if `y` is zero
     */
    def quot(x: BigDecimal, y: BigDecimal): BigDecimal = x quot y
    /** Returns the remainder of two `BigDecimal` values.
     *
     *  @param x the first `BigDecimal` value
     *  @param y the second `BigDecimal` value
     *  @return the remainder of `x` divided by `y`
     *  @throws java.lang.ArithmeticException if `y` is zero
     */
    def rem(x: BigDecimal, y: BigDecimal): BigDecimal = x remainder y
  }

  // For BigDecimal we offer an implicit Fractional object, but also one
  // which acts like an Integral type, which is useful in NumericRange.
  implicit object BigDecimalIsFractional extends BigDecimalIsFractional with Ordering.BigDecimalOrdering
  object BigDecimalAsIfIntegral extends BigDecimalAsIfIntegral with Ordering.BigDecimalOrdering
}

/** Provides arithmetic operations for a numeric type `T`.
 *
 *  @tparam T the numeric type
 */
trait Numeric[T] extends Ordering[T] {
  /** Returns the sum of two values of type `T`.
   *
   *  @param x the first value
   *  @param y the second value
   *  @return the sum of `x` and `y`
   */
  def plus(x: T, y: T): T
  /** Returns the difference between two values of type `T`.
   *
   *  @param x the first value
   *  @param y the second value
   *  @return the difference between `x` and `y`
   */
  def minus(x: T, y: T): T
  /** Returns the product of two values of type `T`.
   *
   *  @param x the first value
   *  @param y the second value
   *  @return the product of `x` and `y`
   */
  def times(x: T, y: T): T
  /** Returns the negation of a value of type `T`.
   *
   *  @param x the value to negate
   *  @return the negation of `x`
   */
  def negate(x: T): T
  /** Converts an `Int` to a value of type `T`.
   *
   *  @param x the `Int` value to convert
   *  @return the `T` representation of `x`
   */
  def fromInt(x: Int): T
  /** Parses a `String` into a value of type `T`.
   *
   *  @param str the `String` to parse
   *  @return `Some(T)` if the string is a valid `T`, `None` otherwise
   */
  def parseString(str: String): Option[T]
  /** Converts a value of type `T` to an `Int`.
   *
   *  @param x the value to convert
   *  @return the `Int` representation of `x`
   */
  def toInt(x: T): Int
  /** Converts a value of type `T` to a `Long`.
   *
   *  @param x the value to convert
   *  @return the `Long` representation of `x`
   */
  def toLong(x: T): Long
  /** Converts a value of type `T` to a `Float`.
   *
   *  @param x the value to convert
   *  @return the `Float` representation of `x`
   */
  def toFloat(x: T): Float
  /** Converts a value of type `T` to a `Double`.
   *
   *  @param x the value to convert
   *  @return the `Double` representation of `x`
   */
  def toDouble(x: T): Double

  /** The additive identity element for type `T`. */
  def zero = fromInt(0)
  /** The multiplicative identity element for type `T`. */
  def one = fromInt(1)

  /** Returns the absolute value of a value of type `T`.
   *
   *  @param x the value
   *  @return the absolute value of `x`
   */
  def abs(x: T): T = if (lt(x, zero)) negate(x) else x

  /** Returns the signum of a value of type `T`.
   *  @deprecated Use `sign` instead.
   *
   *  @param x the value
   *  @return -1 if `x` is negative, 1 if `x` is positive, 0 if `x` is zero
   */
  @deprecated("use `sign` method instead", since = "2.13.0") def signum(x: T): Int =
    if (lt(x, zero)) -1
    else if (gt(x, zero)) 1
    else 0
  /** Returns the sign of a value of type `T`.
   *
   *  @param x the value
   *  @return -1 if `x` is negative, 1 if `x` is positive, 0 if `x` is zero
   */
  def sign(x: T): T =
    if (lt(x, zero)) negate(one)
    else if (gt(x, zero)) one
    else zero

  /** Provides infix arithmetic operations for a value of type `T`.
   *
   *  @param lhs the value to wrap with infix arithmetic operations
   */
  class NumericOps(lhs: T) {
    /** Returns the sum of this value and another value of type `T`.
     *
     *  @param rhs the other value
     *  @return the sum of this value and `rhs`
     */
    def +(rhs: T) = plus(lhs, rhs)
    /** Returns the difference between this value and another value of type `T`.
     *
     *  @param rhs the other value
     *  @return the difference between this value and `rhs`
     */
    def -(rhs: T) = minus(lhs, rhs)
    /** Returns the product of this value and another value of type `T`.
     *
     *  @param rhs the other value
     *  @return the product of this value and `rhs`
     */
    def *(rhs: T) = times(lhs, rhs)
    /** Returns the negation of this value. */
    def unary_- = negate(lhs)
    /** Returns the absolute value of this value. */
    def abs: T = Numeric.this.abs(lhs)
    /** Returns the signum of this value.
     *  @deprecated("use `sign` method instead", since = "2.13.0")
     */
    @deprecated("use `sign` method instead", since = "2.13.0") def signum: Int = Numeric.this.signum(lhs)
    /** Returns the sign of this value. */
    def sign: T = Numeric.this.sign(lhs)
    /** Converts this value to an `Int`. */
    def toInt: Int = Numeric.this.toInt(lhs)
    /** Converts this value to a `Long`. */
    def toLong: Long = Numeric.this.toLong(lhs)
    /** Converts this value to a `Float`. */
    def toFloat: Float = Numeric.this.toFloat(lhs)
    /** Converts this value to a `Double`. */
    def toDouble: Double = Numeric.this.toDouble(lhs)
  }
  /** Creates a `NumericOps` wrapper for a value of type `T`.
   *
   *  @param lhs the value to wrap
   *  @return a `NumericOps` wrapper for `lhs`
   */
  implicit def mkNumericOps(lhs: T): NumericOps = new NumericOps(lhs)
}
