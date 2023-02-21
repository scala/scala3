import scala.util.Try

trait InlineNumeric[T] extends Ordering[T]: // extends Numeric[T] // TODO can we do this?
  transparent inline def plus(inline x: T, inline y: T): T
  transparent inline def minus(inline x: T, inline y: T): T
  transparent inline def times(inline x: T, inline y: T): T
  transparent inline def negate(inline x: T): T
  transparent inline def fromInt(inline x: Int): T
  transparent inline def parseString(inline str: String): Option[T]

  transparent inline def zero = fromInt(0)
  transparent inline def one = fromInt(1)

  extension (inline x: T)
    transparent inline def +(inline y: T): T = plus(x, y)
    transparent inline def -(inline y: T) = minus(x, y)
    transparent inline def *(inline y: T): T = times(x, y)
    transparent inline def unary_- = negate(x)
    transparent inline def toInt: Int
    transparent inline def toLong: Long
    transparent inline def toFloat: Float
    transparent inline def toDouble: Double
    transparent inline def abs: T
    transparent inline def sign: T

trait InlineIntegral[T] extends InlineNumeric[T]:
  transparent inline def quot(inline x: T, inline y: T): T
  transparent inline def rem(inline x: T, inline y: T): T

  extension (inline lhs: T)
    transparent inline def /(inline rhs: T) = quot(lhs, rhs)
    transparent inline def %(inline rhs: T) = rem(lhs, rhs)
    transparent inline def /%(inline rhs: T) = (quot(lhs, rhs), rem(lhs, rhs))

    transparent inline def abs: T = if lt(lhs, zero) then negate(lhs) else lhs
    transparent inline def sign: T =
      if lt(lhs, zero) then negate(one)
      else if gt(lhs, zero) then one
      else zero

trait InlineFractional[T] extends InlineNumeric[T]:
  transparent inline def div(inline x: T, inline y: T): T
  protected transparent inline def isNaN(inline x: T): Boolean
  protected transparent inline def isNegZero(inline x: T): Boolean

  extension (inline lhs: T)
    transparent inline def /(inline rhs: T) = div(lhs, rhs)
    transparent inline def abs: T = if lt(lhs, zero) || isNegZero(lhs) then negate(lhs) else lhs
    transparent inline def sign: T =
      if isNaN(lhs) || isNegZero(lhs) then lhs
      else if lt(lhs, zero) then negate(one)
      else if gt(lhs, zero) then one
      else zero

given IntIsInlineIntegral: InlineIntegral[Int] with Ordering.IntOrdering with
  transparent inline def plus(inline x: Int, inline y: Int): Int = x + y
  transparent inline def minus(inline x: Int, inline y: Int): Int = x - y
  transparent inline def times(inline x: Int, inline y: Int): Int = x * y
  transparent inline def negate(inline x: Int): Int = -x
  transparent inline def fromInt(inline x: Int): Int = x
  transparent inline def parseString(inline str: String): Option[Int] = str.toIntOption

  transparent inline def quot(inline x: Int, inline y: Int): Int = x / y
  transparent inline def rem(inline x: Int, inline y: Int): Int = x % y

  extension (inline x: Int)
    transparent inline def toInt: Int = x
    transparent inline def toLong: Long = x.toLong
    transparent inline def toFloat: Float = x.toFloat
    transparent inline def toDouble: Double = x.toDouble

given BigIntIsInlineIntegral: InlineIntegral[BigInt] with Ordering.BigIntOrdering with
  transparent inline def plus(inline x: BigInt, inline y: BigInt): BigInt = x + y
  transparent inline def minus(inline x: BigInt, inline y: BigInt): BigInt = x - y
  transparent inline def times(inline x: BigInt, inline y: BigInt): BigInt = x * y
  transparent inline def negate(inline x: BigInt): BigInt = -x
  transparent inline def fromInt(inline x: Int): BigInt = BigInt(x)
  transparent inline def parseString(inline str: String): Option[BigInt] = Try(BigInt(str)).toOption

  transparent inline def quot(inline x: BigInt, inline y: BigInt): BigInt = x / y
  transparent inline def rem(inline x: BigInt, inline y: BigInt): BigInt = x % y

  extension (inline x: BigInt)
    transparent inline def toInt: Int = x.intValue
    transparent inline def toLong: Long = x.longValue
    transparent inline def toFloat: Float = x.floatValue
    transparent inline def toDouble: Double = x.doubleValue

given ShortIsInlineIntegral: InlineIntegral[Short] with Ordering.ShortOrdering with
  transparent inline def plus(inline x: Short, inline y: Short): Short = (x + y).toShort
  transparent inline def minus(inline x: Short, inline y: Short): Short = (x - y).toShort
  transparent inline def times(inline x: Short, inline y: Short): Short = (x * y).toShort
  transparent inline def negate(inline x: Short): Short = (-x).toShort
  transparent inline def fromInt(inline x: Int): Short = x.toShort
  transparent inline def parseString(inline str: String): Option[Short] = str.toShortOption

  transparent inline def quot(inline x: Short, inline y: Short): Short = (x / y).toShort
  transparent inline def rem(inline x: Short, inline y: Short): Short = (x % y).toShort

  extension (inline x: Short)
    transparent inline def toInt: Int = x.toInt
    transparent inline def toLong: Long = x.toLong
    transparent inline def toFloat: Float = x.toFloat
    transparent inline def toDouble: Double = x.toDouble

given ByteIsInlineIntegral: InlineIntegral[Byte] with Ordering.ByteOrdering with
  transparent inline def plus(inline x: Byte, inline y: Byte): Byte = (x + y).toByte
  transparent inline def minus(inline x: Byte, inline y: Byte): Byte = (x - y).toByte
  transparent inline def times(inline x: Byte, inline y: Byte): Byte = (x * y).toByte
  transparent inline def negate(inline x: Byte): Byte = (-x).toByte
  transparent inline def fromInt(inline x: Int): Byte = x.toByte
  transparent inline def parseString(inline str: String): Option[Byte] = str.toByteOption

  transparent inline def quot(inline x: Byte, inline y: Byte): Byte = (x / y).toByte
  transparent inline def rem(inline x: Byte, inline y: Byte): Byte = (x % y).toByte

  extension (inline x: Byte)
    transparent inline def toInt: Int = x.toInt
    transparent inline def toLong: Long = x.toLong
    transparent inline def toFloat: Float = x.toFloat
    transparent inline def toDouble: Double = x.toDouble

given CharIsInlineIntegral: InlineIntegral[Char] with Ordering.CharOrdering with
  transparent inline def plus(inline x: Char, inline y: Char): Char = (x + y).toChar
  transparent inline def minus(inline x: Char, inline y: Char): Char = (x - y).toChar
  transparent inline def times(inline x: Char, inline y: Char): Char = (x * y).toChar
  transparent inline def negate(inline x: Char): Char = (-x).toChar
  transparent inline def fromInt(inline x: Int): Char = x.toChar
  transparent inline def parseString(inline str: String): Option[Char] = Try(str.toInt.toChar).toOption

  transparent inline def quot(inline x: Char, inline y: Char): Char = (x / y).toChar
  transparent inline def rem(inline x: Char, inline y: Char): Char = (x % y).toChar

  extension (inline x: Char)
    transparent inline def toInt: Int = x.toInt
    transparent inline def toLong: Long = x.toLong
    transparent inline def toFloat: Float = x.toFloat
    transparent inline def toDouble: Double = x.toDouble

given LongIsInlineIntegral: InlineIntegral[Long] with Ordering.LongOrdering with
  transparent inline def plus(inline x: Long, inline y: Long): Long = x + y
  transparent inline def minus(inline x: Long, inline y: Long): Long = x - y
  transparent inline def times(inline x: Long, inline y: Long): Long = x * y
  transparent inline def negate(inline x: Long): Long = -x
  transparent inline def fromInt(inline x: Int): Long = x.toLong
  transparent inline def parseString(inline str: String): Option[Long] = str.toLongOption

  transparent inline def quot(inline x: Long, inline y: Long): Long = (x / y).toLong
  transparent inline def rem(inline x: Long, inline y: Long): Long = (x % y).toLong

  extension (inline x: Long)
    transparent inline def toInt: Int = x.toInt
    transparent inline def toLong: Long = x
    transparent inline def toFloat: Float = x.toFloat
    transparent inline def toDouble: Double = x.toDouble

given FloatIsInlineFractional: InlineFractional[Float] with Ordering.Float.IeeeOrdering with
  transparent inline def plus(inline x: Float, inline y: Float): Float = x + y
  transparent inline def minus(inline x: Float, inline y: Float): Float = x - y
  transparent inline def times(inline x: Float, inline y: Float): Float = x * y
  transparent inline def negate(inline x: Float): Float = -x
  transparent inline def fromInt(inline x: Int): Float = x.toFloat
  transparent inline def parseString(inline str: String): Option[Float] = str.toFloatOption

  transparent inline def div(inline x: Float, inline y: Float): Float = x / y
  protected transparent inline def isNaN(inline x: Float): Boolean = x.isNaN
  protected transparent inline def isNegZero(inline x: Float): Boolean = x.equals(-0f)

  extension (inline x: Float)
    transparent inline def toInt: Int = x.toInt
    transparent inline def toLong: Long = x.toLong
    transparent inline def toFloat: Float = x
    transparent inline def toDouble: Double = x.toDouble

given DoubleIsInlineFractional: InlineFractional[Double] with Ordering.Double.IeeeOrdering with
  transparent inline def plus(inline x: Double, inline y: Double): Double = x + y
  transparent inline def minus(inline x: Double, inline y: Double): Double = x - y
  transparent inline def times(inline x: Double, inline y: Double): Double = x * y
  transparent inline def negate(inline x: Double): Double = -x
  transparent inline def fromInt(inline x: Int): Double = x.toDouble
  transparent inline def parseString(inline str: String): Option[Double] = str.toDoubleOption

  transparent inline def div(inline x: Double, inline y: Double): Double = x / y
  protected transparent inline def isNaN(inline x: Double): Boolean = x.isNaN
  protected transparent inline def isNegZero(inline x: Double): Boolean = x.equals(-0.0)

  extension (inline x: Double)
    transparent inline def toInt: Int = x.toInt
    transparent inline def toLong: Long = x.toLong
    transparent inline def toFloat: Float = x.toFloat
    transparent inline def toDouble: Double = x.toDouble

trait BigDecimalIsConflicted extends InlineNumeric[BigDecimal] with Ordering.BigDecimalOrdering:
  transparent inline def plus(inline x: BigDecimal, inline y: BigDecimal): BigDecimal = x + y
  transparent inline def minus(inline x: BigDecimal, inline y: BigDecimal): BigDecimal = x - y
  transparent inline def times(inline x: BigDecimal, inline y: BigDecimal): BigDecimal = x * y
  transparent inline def negate(inline x: BigDecimal): BigDecimal = -x
  transparent inline def fromInt(inline x: Int): BigDecimal = BigDecimal(x)
  transparent inline def parseString(inline str: String): Option[BigDecimal] = Try(BigDecimal(str)).toOption

  extension (inline x: BigDecimal)
    transparent inline def toInt: Int = x.intValue
    transparent inline def toLong: Long = x.longValue
    transparent inline def toFloat: Float = x.floatValue
    transparent inline def toDouble: Double = x.doubleValue

given BigDecimalIsInlineFractional: BigDecimalIsConflicted with InlineFractional[BigDecimal] with
  transparent inline def div(inline x: BigDecimal, inline y: BigDecimal): BigDecimal = x / y
  protected transparent inline def isNaN(inline x: BigDecimal): Boolean = false
  protected transparent inline def isNegZero(inline x: BigDecimal): Boolean = false


given BigDecimalAsIfInlineIntegral: BigDecimalIsConflicted with InlineIntegral[BigDecimal] with
  transparent inline def quot(inline x: BigDecimal, inline y: BigDecimal): BigDecimal = x quot y
  transparent inline def rem(inline x: BigDecimal, inline y: BigDecimal): BigDecimal = x remainder y

object tests:
  // A generic inline operation that inlines/specializes primitive operations
  inline def foo[T: InlineNumeric](inline a: T, inline b: T) =
    a + b * b

  inline def div[T: InlineIntegral](inline a: T, inline b: T) =
    a / b % b

  inline def div[T: InlineFractional](inline a: T, inline b: T) =
    a / b + a

  inline def toInt[T: InlineNumeric](inline a: T) =
    a.toInt

  inline def explicitToInt[T](inline a: T)(using n: InlineNumeric[T]) =
    n.toInt(a)

  inline def sign[T: InlineNumeric](inline a: T) =
    a.sign

  inline def explicitPlus[T](inline a: T, inline b: T)(using n: InlineNumeric[T]) =
    n.plus(a, b)

  def test(a: Int, b: Int) =
    val v1 = foo(a, b) // should be a + b * b // can check with -Xprint:inlining
    val v2 = foo(a.toShort, b.toShort) // should be a + b * b

    val v3 = div(BigDecimal(a), BigDecimal(b))(using BigDecimalAsIfInlineIntegral) // should be BigDecimal(a) quot BigDecimal(b) remainder BigDecimal(b)
    val v4 = div(BigDecimal(a), BigDecimal(b))(using BigDecimalIsInlineFractional) // should be BigDecimal(a) / BigDecimal(b) + BigDecimal(a)

    val v5 = toInt(a.toFloat) // should be a.toFloat.toInt
    val v6 = toInt(a) // should be a

    val v7 = sign(a)
    val v8 = sign(a.toChar)
    val v9 = sign(-7F)

    val v10 = sign(BigDecimal(a))(using BigDecimalAsIfInlineIntegral)
    val v11 = sign(BigDecimal(a))(using BigDecimalIsInlineFractional) // the condition with isNan() should be removed, i.e. it should be equivalent to v10

    val v12 = explicitPlus(3, 5) // should be 8
    val v13 = explicitPlus(a, b) // should be a + b

    val v14 = explicitToInt(3.2) // should be (3.2).toInt
    val v15 = explicitToInt(3) // should be 3
    val v16 = explicitToInt(a) // should be a
    val v17 = explicitToInt(a.toShort) // should be a.toShort.toInt