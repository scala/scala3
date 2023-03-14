package scala.math
package inline

import scala.util.Try

trait Numeric[T] extends Ordering[T]:
  inline def plus(inline x: T, inline y: T): T
  inline def minus(inline x: T, inline y: T): T
  inline def times(inline x: T, inline y: T): T
  inline def negate(inline x: T): T

  def fromInt(x: Int): T
  def parseString(str: String): Option[T]

  transparent inline def zero = fromInt(0)
  transparent inline def one = fromInt(1)

  extension (inline x: T)
    transparent inline def +(inline y: T): T = plus(x, y)
    transparent inline def -(inline y: T) = minus(x, y)
    transparent inline def *(inline y: T): T = times(x, y)
    transparent inline def unary_- = negate(x)
    inline def toInt: Int
    inline def toLong: Long
    inline def toFloat: Float
    inline def toDouble: Double
    inline def abs: T
    inline def sign: T

trait BigDecimalIsConflicted extends Numeric[BigDecimal] with Ordering.BigDecimalOrdering:
  transparent inline def plus(inline x: BigDecimal, inline y: BigDecimal): BigDecimal = x + y
  transparent inline def minus(inline x: BigDecimal, inline y: BigDecimal): BigDecimal = x - y
  transparent inline def times(inline x: BigDecimal, inline y: BigDecimal): BigDecimal = x * y
  transparent inline def negate(inline x: BigDecimal): BigDecimal = -x

  transparent inline def fromInt(x: Int): BigDecimal = BigDecimal(x)
  def parseString(str: String): Option[BigDecimal] = Try(BigDecimal(str)).toOption

  extension (inline x: BigDecimal)
    transparent inline def toInt: Int = x.intValue
    transparent inline def toLong: Long = x.longValue
    transparent inline def toFloat: Float = x.floatValue
    transparent inline def toDouble: Double = x.doubleValue
