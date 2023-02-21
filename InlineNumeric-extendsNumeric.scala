import scala.util.Try

trait InlineNumeric[T] extends Numeric[T]:
  transparent inline def plus(x: T, y: T): T
  transparent inline def minus(x: T, y: T): T
  transparent inline def times(x: T, y: T): T
  transparent inline def negate(x: T): T
  transparent inline def fromInt(x: Int): T
  transparent inline def parseString(str: String): Option[T]
  transparent inline def toInt(x: T): Int
  transparent inline def toLong(x: T): Long
  transparent inline def toFloat(x: T): Float
  transparent inline def toDouble(x: T): Double

  // error overriding method abs in trait InlineNumeric of type (x: T): T;
  //   method abs in trait Numeric of type (x: T): T is not inline, cannot implement an inline method
  //transparent inline def abs(x: T): T
  //transparent inline def sign(x: T): T

  // Deferred inline method fromInt in trait InlineNumeric cannot be invoked
  //transparent inline def zero = fromInt(0)
  //transparent inline def one = fromInt(1)

object InlineNumeric:
  extension [T](x: T)(using num: InlineNumeric[T])
    transparent inline def +(y: T): T = num.plus(x, y)
    transparent inline def -(y: T) = num.minus(x, y)
    transparent inline def *(y: T): T = num.times(x, y)
    transparent inline def unary_- = num.negate(x)
    transparent inline def toInt: Int = num.toInt(x)
    transparent inline def toLong: Long = num.toLong(x)
    transparent inline def toFloat: Float = num.toFloat(x)
    transparent inline def toDouble: Double = num.toDouble(x)
    transparent inline def abs: T = num.abs(x)
    transparent inline def sign: T = num.sign(x)

trait InlineIntegral[T] extends InlineNumeric[T]:
  transparent inline def quot(x: T, y: T): T
  transparent inline def rem(x: T, y: T): T

object InlineIntegral:
  extension [T](lhs: T)(using int: InlineIntegral[T])
    transparent inline def /(rhs: T) = int.quot(lhs, rhs)
    transparent inline def %(rhs: T) = int.rem(lhs, rhs)
    transparent inline def /%(rhs: T) = (int.quot(lhs, rhs), int.rem(lhs, rhs))

trait InlineFractional[T] extends InlineNumeric[T]:
  transparent inline def div(x: T, y: T): T

object InlineFractional:
  extension [T](lhs: T)(using frac: InlineFractional[T])
    transparent inline def /(rhs: T) = frac.div(lhs, rhs)

given IntIsInlineIntegral: InlineIntegral[Int] with Ordering.IntOrdering with
  transparent inline def plus(x: Int, y: Int): Int = x + y
  transparent inline def minus(x: Int, y: Int): Int = x - y
  transparent inline def times(x: Int, y: Int): Int = x * y
  transparent inline def negate(x: Int): Int = -x
  transparent inline def fromInt(x: Int): Int = x
  transparent inline def parseString(str: String): Option[Int] = str.toIntOption
  transparent inline def toInt(x: Int): Int = x
  transparent inline def toLong(x: Int): Long = x.toLong
  transparent inline def toFloat(x: Int): Float = x.toFloat
  transparent inline def toDouble(x: Int): Double = x.toDouble
  transparent override inline def abs(x: Int): Int = math.abs(x)
  transparent override inline def sign(x: Int): Int = math.signum(x)

  transparent inline def quot(x: Int, y: Int): Int = x / y
  transparent inline def rem(x: Int, y: Int): Int = x % y

given BigIntIsInlineIntegral: InlineIntegral[BigInt] with Ordering.BigIntOrdering with
  transparent inline def plus(x: BigInt, y: BigInt): BigInt = x + y
  transparent inline def minus(x: BigInt, y: BigInt): BigInt = x - y
  transparent inline def times(x: BigInt, y: BigInt): BigInt = x * y
  transparent inline def negate(x: BigInt): BigInt = -x
  transparent inline def fromInt(x: Int): BigInt = BigInt(x)
  transparent inline def parseString(str: String): Option[BigInt] = Try(BigInt(str)).toOption
  transparent inline def toInt(x: BigInt): Int = x.intValue
  transparent inline def toLong(x: BigInt): Long = x.longValue
  transparent inline def toFloat(x: BigInt): Float = x.floatValue
  transparent inline def toDouble(x: BigInt): Double = x.doubleValue
  transparent override inline def abs(x: BigInt): BigInt = x.abs
  transparent override inline def sign(x: BigInt): BigInt = x.sign

  transparent inline def quot(x: BigInt, y: BigInt): BigInt = x / y
  transparent inline def rem(x: BigInt, y: BigInt): BigInt = x % y

given ShortIsInlineIntegral: InlineIntegral[Short] with Ordering.ShortOrdering with
  transparent inline def plus(x: Short, y: Short): Short = (x + y).toShort
  transparent inline def minus(x: Short, y: Short): Short = (x - y).toShort
  transparent inline def times(x: Short, y: Short): Short = (x * y).toShort
  transparent inline def negate(x: Short): Short = (-x).toShort
  transparent inline def fromInt(x: Int): Short = x.toShort
  transparent inline def parseString(str: String): Option[Short] = str.toShortOption
  transparent inline def toInt(x: Short): Int = x.toInt
  transparent inline def toLong(x: Short): Long = x.toLong
  transparent inline def toFloat(x: Short): Float = x.toFloat
  transparent inline def toDouble(x: Short): Double = x.toDouble
  transparent override inline def abs(x: Short): Short = math.abs(x).toShort
  transparent override inline def sign(x: Short): Short = math.signum(x).toShort

  transparent inline def quot(x: Short, y: Short): Short = (x / y).toShort
  transparent inline def rem(x: Short, y: Short): Short = (x % y).toShort

given ByteIsInlineIntegral: InlineIntegral[Byte] with Ordering.ByteOrdering with
  transparent inline def plus(x: Byte, y: Byte): Byte = (x + y).toByte
  transparent inline def minus(x: Byte, y: Byte): Byte = (x - y).toByte
  transparent inline def times(x: Byte, y: Byte): Byte = (x * y).toByte
  transparent inline def negate(x: Byte): Byte = (-x).toByte
  transparent inline def fromInt(x: Int): Byte = x.toByte
  transparent inline def parseString(str: String): Option[Byte] = str.toByteOption
  transparent inline def toInt(x: Byte): Int = x.toInt
  transparent inline def toLong(x: Byte): Long = x.toLong
  transparent inline def toFloat(x: Byte): Float = x.toFloat
  transparent inline def toDouble(x: Byte): Double = x.toDouble
  transparent override inline def abs(x: Byte): Byte = math.abs(x).toByte
  transparent override inline def sign(x: Byte): Byte = math.signum(x).toByte

  transparent inline def quot(x: Byte, y: Byte): Byte = (x / y).toByte
  transparent inline def rem(x: Byte, y: Byte): Byte = (x % y).toByte

given CharIsInlineIntegral: InlineIntegral[Char] with Ordering.CharOrdering with
  transparent inline def plus(x: Char, y: Char): Char = (x + y).toChar
  transparent inline def minus(x: Char, y: Char): Char = (x - y).toChar
  transparent inline def times(x: Char, y: Char): Char = (x * y).toChar
  transparent inline def negate(x: Char): Char = (-x).toChar
  transparent inline def fromInt(x: Int): Char = x.toChar
  transparent inline def parseString(str: String): Option[Char] = Try(str.toInt.toChar).toOption
  transparent inline def toInt(x: Char): Int = x.toInt
  transparent inline def toLong(x: Char): Long = x.toLong
  transparent inline def toFloat(x: Char): Float = x.toFloat
  transparent inline def toDouble(x: Char): Double = x.toDouble
  transparent override inline def abs(x: Char): Char = math.abs(x).toChar
  transparent override inline def sign(x: Char): Char = math.signum(x).toChar

  transparent inline def quot(x: Char, y: Char): Char = (x / y).toChar
  transparent inline def rem(x: Char, y: Char): Char = (x % y).toChar

given LongIsInlineIntegral: InlineIntegral[Long] with Ordering.LongOrdering with
  transparent inline def plus(x: Long, y: Long): Long = x + y
  transparent inline def minus(x: Long, y: Long): Long = x - y
  transparent inline def times(x: Long, y: Long): Long = x * y
  transparent inline def negate(x: Long): Long = -x
  transparent inline def fromInt(x: Int): Long = x.toLong
  transparent inline def parseString(str: String): Option[Long] = str.toLongOption
  transparent inline def toInt(x: Long): Int = x.toInt
  transparent inline def toLong(x: Long): Long = x
  transparent inline def toFloat(x: Long): Float = x.toFloat
  transparent inline def toDouble(x: Long): Double = x.toDouble
  transparent override inline def abs(x: Long): Long = math.abs(x)
  transparent override inline def sign(x: Long): Long = math.signum(x)

  transparent inline def quot(x: Long, y: Long): Long = (x / y).toLong
  transparent inline def rem(x: Long, y: Long): Long = (x % y).toLong

given FloatIsInlineFractional: InlineFractional[Float] with Ordering.Float.IeeeOrdering with
  transparent inline def plus(x: Float, y: Float): Float = x + y
  transparent inline def minus(x: Float, y: Float): Float = x - y
  transparent inline def times(x: Float, y: Float): Float = x * y
  transparent inline def negate(x: Float): Float = -x
  transparent inline def fromInt(x: Int): Float = x.toFloat
  transparent inline def parseString(str: String): Option[Float] = str.toFloatOption
  transparent inline def toInt(x: Float): Int = x.toInt
  transparent inline def toLong(x: Float): Long = x.toLong
  transparent inline def toFloat(x: Float): Float = x
  transparent inline def toDouble(x: Float): Double = x.toDouble
  transparent override inline def abs(x: Float): Float = math.abs(x)
  transparent override inline def sign(x: Float): Float = math.signum(x)

  transparent inline def div(x: Float, y: Float): Float = x / y

given DoubleIsInlineFractional: InlineFractional[Double] with Ordering.Double.IeeeOrdering with
  transparent inline def plus(x: Double, y: Double): Double = x + y
  transparent inline def minus(x: Double, y: Double): Double = x - y
  transparent inline def times(x: Double, y: Double): Double = x * y
  transparent inline def negate(x: Double): Double = -x
  transparent inline def fromInt(x: Int): Double = x.toDouble
  transparent inline def parseString(str: String): Option[Double] = str.toDoubleOption
  transparent inline def toInt(x: Double): Int = x.toInt
  transparent inline def toLong(x: Double): Long = x.toLong
  transparent inline def toFloat(x: Double): Float = x.toFloat
  transparent inline def toDouble(x: Double): Double = x
  transparent override inline def abs(x: Double): Double = math.abs(x)
  transparent override inline def sign(x: Double): Double = math.signum(x)

  transparent inline def div(x: Double, y: Double): Double = x / y

trait BigDecimalIsConflicted extends InlineNumeric[BigDecimal] with Ordering.BigDecimalOrdering:
  transparent inline def plus(x: BigDecimal, y: BigDecimal): BigDecimal = x + y
  transparent inline def minus(x: BigDecimal, y: BigDecimal): BigDecimal = x - y
  transparent inline def times(x: BigDecimal, y: BigDecimal): BigDecimal = x * y
  transparent inline def negate(x: BigDecimal): BigDecimal = -x
  transparent inline def fromInt(x: Int): BigDecimal = BigDecimal(x)
  transparent inline def parseString(str: String): Option[BigDecimal] = Try(BigDecimal(str)).toOption
  transparent inline def toInt(x: BigDecimal): Int = x.intValue
  transparent inline def toLong(x: BigDecimal): Long = x.longValue
  transparent inline def toFloat(x: BigDecimal): Float = x.floatValue
  transparent inline def toDouble(x: BigDecimal): Double = x.doubleValue
  transparent override inline def abs(x: BigDecimal): BigDecimal = x.abs
  transparent override inline def sign(x: BigDecimal): BigDecimal = x.sign

given BigDecimalIsInlineFractional: BigDecimalIsConflicted with InlineFractional[BigDecimal] with
  transparent inline def div(x: BigDecimal, y: BigDecimal): BigDecimal = x / y

given BigDecimalAsIfInlineIntegral: BigDecimalIsConflicted with InlineIntegral[BigDecimal] with
  transparent inline def quot(x: BigDecimal, y: BigDecimal): BigDecimal = x quot y
  transparent inline def rem(x: BigDecimal, y: BigDecimal): BigDecimal = x remainder y

object tests:
  import InlineNumeric.*
  import InlineIntegral.{/ => ¦, %}
  import InlineFractional./

  inline def foo[T: InlineNumeric](a: T, b: T) =
    a + b * b

  inline def div[T: InlineIntegral](a: T, b: T) =
    a ¦ b % b

  inline def div[T: InlineFractional](a: T, b: T) =
    a / b + a

  inline def bar[T: InlineNumeric](a: T) = a.toInt

  inline def sign[T: InlineNumeric](a: T) = a.sign

  def test(a: Int, b: Int) =
    val v1 = foo(a, b) // should be a + b * b // can check with -Xprint:inlining
    val v2 = foo(a.toShort, b.toShort) // should be a + b * b

    val v3 = div(BigDecimal(a), BigDecimal(b))(using BigDecimalAsIfInlineIntegral) // should be BigDecimal(a) quot BigDecimal(b) remainder BigDecimal(b)
    val v4 = div(BigDecimal(a), BigDecimal(b))(using BigDecimalIsInlineFractional) // should be BigDecimal(a) / BigDecimal(b) + BigDecimal(a)

    val v5 = bar(a.toFloat) // should be a.toFloat.toInt
    val v6 = bar(a) // should be a

    val v7 = sign(a)
    val v8 = sign(a.toChar)
    val v9 = sign(-7F)

    val v10 = sign(BigDecimal(a))(using BigDecimalAsIfInlineIntegral)
    val v11 = sign(BigDecimal(a))(using BigDecimalIsInlineFractional) // the condition with isNan() should be removed, i.e. it should be equivalent to v10