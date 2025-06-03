package scala.math
package inline

import scala.util.Try

trait Integral[T] extends Numeric[T]:
  inline def quot(inline x: T, inline y: T): T
  inline def rem(inline x: T, inline y: T): T

  extension (inline x: T)
    transparent inline def abs: T =
      if lt(x, zero) then negate(x) else x
    transparent inline def sign: T =
      if lt(x, zero) then negate(one)
      else if gt(x, zero) then one
      else zero
    transparent inline def /(inline y: T) = quot(x, y)
    transparent inline def %(inline y: T) = rem(x, y)
    transparent inline def /%(inline y: T) = (quot(x, y), rem(x, y))

object Integral:
  given BigDecimalAsIfIntegral: Integral[BigDecimal], BigDecimalIsConflicted:
    transparent inline def quot(inline x: BigDecimal, inline y: BigDecimal): BigDecimal = x quot y
    transparent inline def rem(inline x: BigDecimal, inline y: BigDecimal): BigDecimal = x remainder y

  given BigIntIsIntegral: Integral[BigInt], Ordering.BigIntOrdering:
    transparent inline def plus(inline x: BigInt, inline y: BigInt): BigInt = x + y
    transparent inline def minus(inline x: BigInt, inline y: BigInt): BigInt = x - y
    transparent inline def times(inline x: BigInt, inline y: BigInt): BigInt = x * y
    transparent inline def negate(inline x: BigInt): BigInt = -x

    extension (inline x: BigInt)
      transparent inline def toInt: Int = x.intValue
      transparent inline def toLong: Long = x.longValue
      transparent inline def toFloat: Float = x.floatValue
      transparent inline def toDouble: Double = x.doubleValue

    transparent inline def fromInt(x: Int): BigInt = BigInt(x)
    def parseString(str: String): Option[BigInt] = Try(BigInt(str)).toOption

    transparent inline def quot(inline x: BigInt, inline y: BigInt): BigInt = x / y
    transparent inline def rem(inline x: BigInt, inline y: BigInt): BigInt = x % y

  given ByteIsIntegral: Integral[Byte], Ordering.ByteOrdering:
    transparent inline def plus(inline x: Byte, inline y: Byte): Byte = (x + y).toByte
    transparent inline def minus(inline x: Byte, inline y: Byte): Byte = (x - y).toByte
    transparent inline def times(inline x: Byte, inline y: Byte): Byte = (x * y).toByte
    transparent inline def negate(inline x: Byte): Byte = (-x).toByte

    transparent inline def fromInt(x: Int): Byte = x.toByte
    def parseString(str: String): Option[Byte] = str.toByteOption

    transparent inline def quot(inline x: Byte, inline y: Byte): Byte = (x / y).toByte
    transparent inline def rem(inline x: Byte, inline y: Byte): Byte = (x % y).toByte

    extension (inline x: Byte)
      transparent inline def toInt: Int = x.toInt
      transparent inline def toLong: Long = x.toLong
      transparent inline def toFloat: Float = x.toFloat
      transparent inline def toDouble: Double = x.toDouble

  given CharIsIntegral: Integral[Char], Ordering.CharOrdering:
    transparent inline def plus(inline x: Char, inline y: Char): Char = (x + y).toChar
    transparent inline def minus(inline x: Char, inline y: Char): Char = (x - y).toChar
    transparent inline def times(inline x: Char, inline y: Char): Char = (x * y).toChar
    transparent inline def negate(inline x: Char): Char = (-x).toChar

    transparent inline def fromInt(x: Int): Char = x.toChar
    def parseString(str: String): Option[Char] = Try(str.toInt.toChar).toOption

    transparent inline def quot(inline x: Char, inline y: Char): Char = (x / y).toChar
    transparent inline def rem(inline x: Char, inline y: Char): Char = (x % y).toChar

    extension (inline x: Char)
      transparent inline def toInt: Int = x.toInt
      transparent inline def toLong: Long = x.toLong
      transparent inline def toFloat: Float = x.toFloat
      transparent inline def toDouble: Double = x.toDouble

  given IntIsIntegral: Integral[Int], Ordering.IntOrdering:
    transparent inline def plus(inline x: Int, inline y: Int): Int = x + y
    transparent inline def minus(inline x: Int, inline y: Int): Int = x - y
    transparent inline def times(inline x: Int, inline y: Int): Int = x * y
    transparent inline def negate(inline x: Int): Int = -x

    transparent inline def fromInt(x: Int): Int = x
    def parseString(str: String): Option[Int] = str.toIntOption

    transparent inline def quot(inline x: Int, inline y: Int): Int = x / y
    transparent inline def rem(inline x: Int, inline y: Int): Int = x % y

    extension (inline x: Int)
      transparent inline def toInt: Int = x
      transparent inline def toLong: Long = x.toLong
      transparent inline def toFloat: Float = x.toFloat
      transparent inline def toDouble: Double = x.toDouble

  given LongIsIntegral: Integral[Long], Ordering.LongOrdering:
    transparent inline def plus(inline x: Long, inline y: Long): Long = x + y
    transparent inline def minus(inline x: Long, inline y: Long): Long = x - y
    transparent inline def times(inline x: Long, inline y: Long): Long = x * y
    transparent inline def negate(inline x: Long): Long = -x

    transparent inline def fromInt(x: Int): Long = x.toLong
    def parseString(str: String): Option[Long] = str.toLongOption

    transparent inline def quot(inline x: Long, inline y: Long): Long = (x / y).toLong
    transparent inline def rem(inline x: Long, inline y: Long): Long = (x % y).toLong

    extension (inline x: Long)
      transparent inline def toInt: Int = x.toInt
      transparent inline def toLong: Long = x
      transparent inline def toFloat: Float = x.toFloat
      transparent inline def toDouble: Double = x.toDouble

  given ShortIsIntegral: Integral[Short], Ordering.ShortOrdering:
    transparent inline def plus(inline x: Short, inline y: Short): Short = (x + y).toShort
    transparent inline def minus(inline x: Short, inline y: Short): Short = (x - y).toShort
    transparent inline def times(inline x: Short, inline y: Short): Short = (x * y).toShort
    transparent inline def negate(inline x: Short): Short = (-x).toShort

    transparent inline def fromInt(x: Int): Short = x.toShort
    def parseString(str: String): Option[Short] = str.toShortOption

    transparent inline def quot(inline x: Short, inline y: Short): Short = (x / y).toShort
    transparent inline def rem(inline x: Short, inline y: Short): Short = (x % y).toShort

    extension (inline x: Short)
      transparent inline def toInt: Int = x.toInt
      transparent inline def toLong: Long = x.toLong
      transparent inline def toFloat: Float = x.toFloat
      transparent inline def toDouble: Double = x.toDouble
