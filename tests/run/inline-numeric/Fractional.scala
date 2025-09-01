package scala.math
package inline

trait Fractional[T] extends Numeric[T]:
  transparent inline def div(inline x: T, inline y: T): T
  protected transparent inline def isNaN(inline x: T): Boolean
  protected transparent inline def isNegZero(inline x: T): Boolean

  extension (inline x: T)
    transparent inline def abs: T =
      if lt(x, zero) || isNegZero(x) then negate(x) else x
    transparent inline def sign: T =
      if isNaN(x) || isNegZero(x) then x
      else if lt(x, zero) then negate(one)
      else if gt(x, zero) then one
      else zero
    transparent inline def /(inline y: T) = div(x, y)

object Fractional:
  given BigDecimalIsFractional: BigDecimalIsConflicted, Fractional[BigDecimal]:
    transparent inline def div(inline x: BigDecimal, inline y: BigDecimal): BigDecimal = x / y

    protected transparent inline def isNaN(inline x: BigDecimal): Boolean = false
    protected transparent inline def isNegZero(inline x: BigDecimal): Boolean = false

  given DoubleIsFractional: Fractional[Double], Ordering.DoubleIeeeOrdering:
    transparent inline def plus(inline x: Double, inline y: Double): Double = x + y
    transparent inline def minus(inline x: Double, inline y: Double): Double = x - y
    transparent inline def times(inline x: Double, inline y: Double): Double = x * y
    transparent inline def div(inline x: Double, inline y: Double): Double = x / y
    transparent inline def negate(inline x: Double): Double = -x

    transparent inline def fromInt(x: Int): Double = x.toDouble
    def parseString(str: String): Option[Double] = str.toDoubleOption

    protected transparent inline def isNaN(inline x: Double): Boolean = x.isNaN
    protected transparent inline def isNegZero(inline x: Double): Boolean = x.equals(-0.0)

    extension (inline x: Double)
      transparent inline def toInt: Int = x.toInt
      transparent inline def toLong: Long = x.toLong
      transparent inline def toFloat: Float = x.toFloat
      transparent inline def toDouble: Double = x

  given FloatIsFractional: Fractional[Float], Ordering.FloatIeeeOrdering:
    transparent inline def plus(inline x: Float, inline y: Float): Float = x + y
    transparent inline def minus(inline x: Float, inline y: Float): Float = x - y
    transparent inline def times(inline x: Float, inline y: Float): Float = x * y
    transparent inline def div(inline x: Float, inline y: Float): Float = x / y
    transparent inline def negate(inline x: Float): Float = -x

    transparent inline def fromInt(x: Int): Float = x.toFloat
    def parseString(str: String): Option[Float] = str.toFloatOption

    protected transparent inline def isNaN(inline x: Float): Boolean = x.isNaN
    protected transparent inline def isNegZero(inline x: Float): Boolean = x.equals(-0f)

    extension (inline x: Float)
      transparent inline def toInt: Int = x.toInt
      transparent inline def toLong: Long = x.toLong
      transparent inline def toFloat: Float = x
      transparent inline def toDouble: Double = x.toDouble
