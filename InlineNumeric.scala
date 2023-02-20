// community-build/community-projects/stdLib213/src/library/scala/math/Numeric.scala
// community-build/community-projects/stdLib213/src/library/scala/math/Integral.scala
// community-build/community-projects/stdLib213/src/library/scala/math/Fractional.scala

import scala.util.Try

trait InlineNumeric[T] extends Ordering[T]: // extends Numeric[T] // TODO can we do this?
  transparent inline def plus(inline x: T, inline y: T): T
  transparent inline def minus(inline x: T, inline y: T): T
  transparent inline def times(inline x: T, inline y: T): T
  transparent inline def negate(inline x: T): T
  transparent inline def fromInt(inline x: Int): T
  transparent inline def parseString(inline str: String): Option[T]
  transparent inline def toInt(inline x: T): Int
  transparent inline def toLong(inline x: T): Long
  transparent inline def toFloat(inline x: T): Float
  transparent inline def toDouble(inline x: T): Double

  transparent inline def zero = fromInt(0)
  transparent inline def one = fromInt(1)

  transparent inline def abs(inline x: T): T = if lt(x, zero) then negate(x) else x

  transparent inline def sign(inline x: T): T =
    if lt(x, zero) then negate(one)
    else if gt(x, zero) then one
    else zero

object InlineNumeric:
  extension [T](inline x: T)(using inline num: InlineNumeric[T])
    transparent inline def +(inline y: T): T = num.plus(x, y)
    transparent inline def -(inline y: T) = num.minus(x, y)
    transparent inline def *(inline y: T): T = num.times(x, y)
    transparent inline def unary_- = num.negate(x)
    transparent inline def toInt: Int = num.toInt(x)
    transparent inline def toLong: Long = num.toLong(x)
    transparent inline def toFloat: Float = num.toFloat(x)
    transparent inline def toDouble: Double = num.toDouble(x)

    transparent inline def abs: T = num.abs(x)

    transparent inline def sign: T = num.sign(x)

trait InlineIntegral[T] extends InlineNumeric[T]:
  transparent inline def quot(inline x: T, inline y: T): T
  transparent inline def rem(inline x: T, inline y: T): T

object InlineIntegral:
  // TODO: how are these imported/composed with Numeric/Integral. Should the extension methods be defined in trait InlineIntegral?
  extension [T](inline lhs: T)(using inline int: InlineIntegral[T])
    transparent inline def /(inline rhs: T) = int.quot(lhs, rhs)
    transparent inline def %(inline rhs: T) = int.rem(lhs, rhs)
    transparent inline def /%(inline rhs: T) = (int.quot(lhs, rhs), int.rem(lhs, rhs))

trait InlineFractional[T] extends InlineNumeric[T]:
  transparent inline def div(inline x: T, inline y: T): T

object InlineFractional:
  // TODO: how are these imported/composed with Numeric/Fractional. Should the extension methods be defined in trait InlineFractional?
  extension [T](inline lhs: T)(using inline frac: InlineFractional[T])
    transparent inline def /(inline rhs: T) = frac.div(lhs, rhs)

given IntIsInlineIntegral: InlineIntegral[Int] with Ordering.IntOrdering with
  transparent inline def plus(inline x: Int, inline y: Int): Int = x + y
  transparent inline def minus(inline x: Int, inline y: Int): Int = x - y
  transparent inline def times(inline x: Int, inline y: Int): Int = x * y
  transparent inline def negate(inline x: Int): Int = -x
  transparent inline def fromInt(inline x: Int): Int = x
  transparent inline def parseString(inline str: String): Option[Int] = str.toIntOption
  transparent inline def toInt(inline x: Int): Int = x
  transparent inline def toLong(inline x: Int): Long = x.toLong
  transparent inline def toFloat(inline x: Int): Float = x.toFloat
  transparent inline def toDouble(inline x: Int): Double = x.toDouble

  transparent inline def quot(inline x: Int, inline y: Int): Int = x / y
  transparent inline def rem(inline x: Int, inline y: Int): Int = x % y

given BigIntIsInlineIntegral: InlineIntegral[BigInt] with Ordering.BigIntOrdering with
  transparent inline def plus(inline x: BigInt, inline y: BigInt): BigInt = x + y
  transparent inline def minus(inline x: BigInt, inline y: BigInt): BigInt = x - y
  transparent inline def times(inline x: BigInt, inline y: BigInt): BigInt = x * y
  transparent inline def negate(inline x: BigInt): BigInt = -x
  transparent inline def fromInt(inline x: Int): BigInt = BigInt(x)
  transparent inline def parseString(inline str: String): Option[BigInt] = Try(BigInt(str)).toOption
  transparent inline def toInt(inline x: BigInt): Int = x.intValue
  transparent inline def toLong(inline x: BigInt): Long = x.longValue
  transparent inline def toFloat(inline x: BigInt): Float = x.floatValue
  transparent inline def toDouble(inline x: BigInt): Double = x.doubleValue

  transparent inline def quot(inline x: BigInt, inline y: BigInt): BigInt = x / y
  transparent inline def rem(inline x: BigInt, inline y: BigInt): BigInt = x % y

given ShortIsInlineIntegral: InlineIntegral[Short] with Ordering.ShortOrdering with
  transparent inline def plus(inline x: Short, inline y: Short): Short = (x + y).toShort
  transparent inline def minus(inline x: Short, inline y: Short): Short = (x - y).toShort
  transparent inline def times(inline x: Short, inline y: Short): Short = (x * y).toShort
  transparent inline def negate(inline x: Short): Short = (-x).toShort
  transparent inline def fromInt(inline x: Int): Short = x.toShort
  transparent inline def parseString(inline str: String): Option[Short] = str.toShortOption
  transparent inline def toInt(inline x: Short): Int = x.toInt
  transparent inline def toLong(inline x: Short): Long = x.toLong
  transparent inline def toFloat(inline x: Short): Float = x.toFloat
  transparent inline def toDouble(inline x: Short): Double = x.toDouble

  transparent inline def quot(inline x: Short, inline y: Short): Short = (x / y).toShort
  transparent inline def rem(inline x: Short, inline y: Short): Short = (x % y).toShort

given ByteIsInlineIntegral: InlineIntegral[Byte] with Ordering.ByteOrdering with
  transparent inline def plus(inline x: Byte, inline y: Byte): Byte = (x + y).toByte
  transparent inline def minus(inline x: Byte, inline y: Byte): Byte = (x - y).toByte
  transparent inline def times(inline x: Byte, inline y: Byte): Byte = (x * y).toByte
  transparent inline def negate(inline x: Byte): Byte = (-x).toByte
  transparent inline def fromInt(inline x: Int): Byte = x.toByte
  transparent inline def parseString(inline str: String): Option[Byte] = str.toByteOption
  transparent inline def toInt(inline x: Byte): Int = x.toInt
  transparent inline def toLong(inline x: Byte): Long = x.toLong
  transparent inline def toFloat(inline x: Byte): Float = x.toFloat
  transparent inline def toDouble(inline x: Byte): Double = x.toDouble

  transparent inline def quot(inline x: Byte, inline y: Byte): Byte = (x / y).toByte
  transparent inline def rem(inline x: Byte, inline y: Byte): Byte = (x % y).toByte

given CharIsInlineIntegral: InlineIntegral[Char] with Ordering.CharOrdering with
  transparent inline def plus(inline x: Char, inline y: Char): Char = (x + y).toChar
  transparent inline def minus(inline x: Char, inline y: Char): Char = (x - y).toChar
  transparent inline def times(inline x: Char, inline y: Char): Char = (x * y).toChar
  transparent inline def negate(inline x: Char): Char = (-x).toChar
  transparent inline def fromInt(inline x: Int): Char = x.toChar
  transparent inline def parseString(inline str: String): Option[Char] = Try(str.toInt.toChar).toOption
  transparent inline def toInt(inline x: Char): Int = x.toInt
  transparent inline def toLong(inline x: Char): Long = x.toLong
  transparent inline def toFloat(inline x: Char): Float = x.toFloat
  transparent inline def toDouble(inline x: Char): Double = x.toDouble

  transparent inline def quot(inline x: Char, inline y: Char): Char = (x / y).toChar
  transparent inline def rem(inline x: Char, inline y: Char): Char = (x % y).toChar

given LongIsInlineIntegral: InlineIntegral[Long] with Ordering.LongOrdering with
  transparent inline def plus(inline x: Long, inline y: Long): Long = x + y
  transparent inline def minus(inline x: Long, inline y: Long): Long = x - y
  transparent inline def times(inline x: Long, inline y: Long): Long = x * y
  transparent inline def negate(inline x: Long): Long = -x
  transparent inline def fromInt(inline x: Int): Long = x.toLong
  transparent inline def parseString(inline str: String): Option[Long] = str.toLongOption
  transparent inline def toInt(inline x: Long): Int = x.toInt
  transparent inline def toLong(inline x: Long): Long = x
  transparent inline def toFloat(inline x: Long): Float = x.toFloat
  transparent inline def toDouble(inline x: Long): Double = x.toDouble

  transparent inline def quot(inline x: Long, inline y: Long): Long = (x / y).toLong
  transparent inline def rem(inline x: Long, inline y: Long): Long = (x % y).toLong

given FloatIsInlineFractional: InlineFractional[Float] with Ordering.Float.IeeeOrdering with
  transparent inline def plus(inline x: Float, inline y: Float): Float = x + y
  transparent inline def minus(inline x: Float, inline y: Float): Float = x - y
  transparent inline def times(inline x: Float, inline y: Float): Float = x * y
  transparent inline def negate(inline x: Float): Float = -x
  transparent inline def fromInt(inline x: Int): Float = x.toFloat
  transparent inline def parseString(inline str: String): Option[Float] = str.toFloatOption
  transparent inline def toInt(inline x: Float): Int = x.toInt
  transparent inline def toLong(inline x: Float): Long = x.toLong
  transparent inline def toFloat(inline x: Float): Float = x
  transparent inline def toDouble(inline x: Float): Double = x.toDouble

  transparent inline def div(inline x: Float, inline y: Float): Float = x / y

given DoubleIsInlineFractional: InlineFractional[Double] with Ordering.Double.IeeeOrdering with
  transparent inline def plus(inline x: Double, inline y: Double): Double = x + y
  transparent inline def minus(inline x: Double, inline y: Double): Double = x - y
  transparent inline def times(inline x: Double, inline y: Double): Double = x * y
  transparent inline def negate(inline x: Double): Double = -x
  transparent inline def fromInt(inline x: Int): Double = x.toDouble
  transparent inline def parseString(inline str: String): Option[Double] = str.toDoubleOption
  transparent inline def toInt(inline x: Double): Int = x.toInt
  transparent inline def toLong(inline x: Double): Long = x.toLong
  transparent inline def toFloat(inline x: Double): Float = x.toFloat
  transparent inline def toDouble(inline x: Double): Double = x

  transparent inline def div(inline x: Double, inline y: Double): Double = x / y

trait BigDecimalIsConflicted extends InlineNumeric[BigDecimal] with Ordering.BigDecimalOrdering:
  transparent inline def plus(inline x: BigDecimal, inline y: BigDecimal): BigDecimal = x + y
  transparent inline def minus(inline x: BigDecimal, inline y: BigDecimal): BigDecimal = x - y
  transparent inline def times(inline x: BigDecimal, inline y: BigDecimal): BigDecimal = x * y
  transparent inline def negate(inline x: BigDecimal): BigDecimal = -x
  transparent inline def fromInt(inline x: Int): BigDecimal = BigDecimal(x)
  transparent inline def parseString(inline str: String): Option[BigDecimal] = Try(BigDecimal(str)).toOption
  transparent inline def toInt(inline x: BigDecimal): Int = x.intValue
  transparent inline def toLong(inline x: BigDecimal): Long = x.longValue
  transparent inline def toFloat(inline x: BigDecimal): Float = x.floatValue
  transparent inline def toDouble(inline x: BigDecimal): Double = x.doubleValue

given BigDecimalIsInlineFractional: BigDecimalIsConflicted with InlineFractional[BigDecimal] with
  transparent inline def div(inline x: BigDecimal, inline y: BigDecimal): BigDecimal = x / y

given BigDecimalAsIfInlineIntegral: BigDecimalIsConflicted with InlineIntegral[BigDecimal] with
  transparent inline def quot(inline x: BigDecimal, inline y: BigDecimal): BigDecimal = x quot y
  transparent inline def rem(inline x: BigDecimal, inline y: BigDecimal): BigDecimal = x remainder y

object tests:
  import InlineNumeric.*

  // A generic inline operation that inlines/specializes primitive operations
  inline def foo[T: InlineNumeric](a: T, b: T) =
    a + b * b

  inline def integDiv[T: InlineIntegral](a: T, b: T) =
    import InlineIntegral.{/, %}
    a / b % b

  inline def fracDiv[T: InlineFractional](a: T, b: T) =
    import InlineFractional.{/}
    a / b + a

  inline def bar[T: InlineNumeric](a: T) = a.toInt

  inline def sign[T: InlineNumeric](a: T) = a.sign

  def test(a: Int, b: Int) =
    foo(a, b) // should be a + b * b // can check with -Xprint:inlining
    foo(a.toShort, b.toShort) // should be a + b * b

    integDiv(BigDecimal(a), BigDecimal(b)) // should be BigDecimal(a) quot BigDecimal(b) remainder BigDecimal(b)
    fracDiv(BigDecimal(a), BigDecimal(b)) // should be BigDecimal(a) / BigDecimal(b) + BigDecimal(a)

    bar(a.toFloat) // should be a.toFloat.toInt
    bar(a) // should be a

    sign(a)
    sign(a.toChar)
    sign(-7F)
