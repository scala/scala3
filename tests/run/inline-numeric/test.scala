import scala.math.inline.*
import scala.math.inline.Ordering.*
import scala.math.inline.Integral.given
import scala.math.inline.Fractional.given

object tests:
  inline def foo[T: Numeric](inline a: T, inline b: T) =
    a + b * b

  inline def div[T: Integral](inline a: T, inline b: T) =
    a / b % b

  inline def div[T: Fractional](inline a: T, inline b: T) =
    a / b + a

  inline def toInt[T: Numeric](inline a: T) =
    a.toInt

  inline def explicitToInt[T](inline a: T)(using n: Numeric[T]) =
    n.toInt(a)

  inline def sign[T: Numeric](inline a: T) =
    a.sign

  inline def explicitPlus[T](inline a: T, inline b: T)(using n: Numeric[T]) =
    n.plus(a, b)

  @main def Test =
    def a: Int = 0
    def b: Int = 1

    val v1 = foo(a, b) // should be a + b * b // can check with -Vprint:inlining
    val v2 = foo(a.toShort, b.toShort) // should be a + b * b

    val v3 = div(BigDecimal(a), BigDecimal(b))(using BigDecimalAsIfIntegral) // should be BigDecimal(a) quot BigDecimal(b) remainder BigDecimal(b)
    val v4 = div(BigDecimal(a), BigDecimal(b))(using BigDecimalIsFractional) // should be BigDecimal(a) / BigDecimal(b) + BigDecimal(a)

    val v5 = toInt(a.toFloat) // should be a.toFloat.toInt
    val v6 = toInt(a) // should be a

    val v7 = sign(a)
    val v8 = sign(a.toChar)
    val v9 = sign(-7F)

    val v10 = sign(BigDecimal(a))(using BigDecimalAsIfIntegral)
    val v11 = sign(BigDecimal(a))(using BigDecimalIsFractional) // the condition with isNan() should be removed, i.e. it should be equivalent to v10

    val v12 = explicitPlus(3, 5) // should be 8
    val v13 = explicitPlus(a, b) // should be a + b

    val v14 = explicitToInt(3.2) // should be (3.2).toInt
    val v15 = explicitToInt(3) // should be 3
    val v16 = explicitToInt(a) // should be a
    val v17 = explicitToInt(a.toShort) // should be a.toShort.toInt
