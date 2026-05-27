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

import scala.language.`2.13`

/** The package object `scala.math` contains methods for performing basic
 *  numeric operations such as elementary exponential, logarithmic, root and
 *  trigonometric functions.
 *
 *  All methods forward to [[java.lang.Math]] unless otherwise noted.
 *
 *  @see [[java.lang.Math]]
 *
 *  @groupname math-const Mathematical Constants
 *  @groupprio math-const 10
 *
 *  @groupname minmax Minimum and Maximum
 *  @groupdesc minmax Find the min or max of two numbers. Note: [[scala.collection.IterableOnceOps]] has
 *           min and max methods which determine the min or max of a collection.
 *  @groupprio minmax 20
 *
 *  @groupname rounding Rounding
 *  @groupprio rounding 30
 *
 *  @groupname scaling Scaling
 *  @groupdesc scaling Scaling with rounding guarantees
 *  @groupprio scaling 40
 *
 *  @groupname explog Exponential and Logarithmic
 *  @groupprio explog 50
 *
 *  @groupname trig Trigonometric
 *  @groupdesc trig Arguments in radians
 *  @groupprio trig 60
 *
 *  @groupname angle-conversion Angular Measurement Conversion
 *  @groupprio angle-conversion 70
 *
 *  @groupname hyperbolic Hyperbolic
 *  @groupprio hyperbolic 80
 *
 *  @groupname abs Absolute Values
 *  @groupdesc abs Determine the magnitude of a value by discarding the sign. Results are >= 0.
 *  @groupprio abs 90
 *
 *  @groupname signs Signs
 *  @groupdesc signs For `signum` extract the sign of a value. Results are -1, 0 or 1.
 *  Note the `signum` methods are not pure forwarders to the Java versions.
 *  In particular, the return type of `java.lang.Long.signum` is `Int`,
 *  but here it is widened to `Long` so that each overloaded variant
 *  will return the same numeric type it is passed.
 *  @groupprio signs 100
 *
 *  @groupname root-extraction Root Extraction
 *  @groupprio root-extraction 110
 *
 *  @groupname polar-coords Polar Coordinates
 *  @groupprio polar-coords 120
 *
 *  @groupname ulp Unit of Least Precision
 *  @groupprio ulp 130
 *
 *  @groupname randomisation Pseudo Random Number Generation
 *  @groupprio randomisation 140
 *
 *  @groupname exact Exact Arithmetic
 *  @groupdesc exact Integral addition, multiplication, stepping and conversion throwing ArithmeticException instead of underflowing or overflowing
 *  @groupprio exact 150
 *
 *  @groupname modquo Modulus and Quotient
 *  @groupdesc modquo Calculate quotient values by rounding to negative infinity
 *  @groupprio modquo 160
 *
 *  @groupname adjacent-float Adjacent Floats
 *  @groupprio adjacent-float 170
 */
package object math {
  /** The `Double` value that is closer than any other to `e`, the base of
   *  the natural logarithms.
   *  @group math-const
   */
  @inline final val E = java.lang.Math.E

  /** The `Double` value that is closer than any other to `pi`, the ratio of
   *  the circumference of a circle to its diameter.
   *  @group math-const
   */
  @inline final val Pi = java.lang.Math.PI

  /** Returns a `Double` value with a positive sign, greater than or equal
   *  to `0.0` and less than `1.0`.
   *
   *  @group randomisation
   */
  def random(): Double = java.lang.Math.random()

  /**
   *  @group trig
   *
   *  @param x the angle, in radians
   */
  def sin(x: Double): Double = java.lang.Math.sin(x)
  /**
   *  @group trig
   *
   *  @param x the angle, in radians
   */
  def cos(x: Double): Double = java.lang.Math.cos(x)
  /**
   *  @group trig
   *
   *  @param x the angle, in radians
   */
  def tan(x: Double): Double = java.lang.Math.tan(x)
  /**
   *  @group trig
   *
   *  @param x the value whose arc sine is to be returned
   */
  def asin(x: Double): Double = java.lang.Math.asin(x)
  /**
   *  @group trig
   *
   *  @param x the value whose arc cosine is to be returned
   */
  def acos(x: Double): Double = java.lang.Math.acos(x)
  /**
   *  @group trig
   *
   *  @param x the value whose arc tangent is to be returned
   */
  def atan(x: Double): Double = java.lang.Math.atan(x)

  /** Converts an angle measured in degrees to an approximately equivalent
   *  angle measured in radians.
   *
   *  @param  x an angle, in degrees
   *  @return the measurement of the angle `x` in radians.
   *  @group angle-conversion
   */
  def toRadians(x: Double): Double = java.lang.Math.toRadians(x)

  /** Converts an angle measured in radians to an approximately equivalent
   *  angle measured in degrees.
   *
   *  @param  x angle, in radians
   *  @return the measurement of the angle `x` in degrees.
   *  @group angle-conversion
   */
  def toDegrees(x: Double): Double = java.lang.Math.toDegrees(x)

  /** Converts rectangular coordinates `(x, y)` to polar `(r, theta)`.
   *
   *  @param  y the ordinate coordinate
   *  @param  x the abscissa coordinate
   *  @return the *theta* component of the point `(r, theta)` in polar
   *          coordinates that corresponds to the point `(x, y)` in
   *          Cartesian coordinates.
   *  @group polar-coords
   */
  def atan2(y: Double, x: Double): Double = java.lang.Math.atan2(y, x)

  /** Returns the square root of the sum of the squares of both given `Double`
   *  values without intermediate underflow or overflow.
   *
   *  The *r* component of the point `(r, theta)` in polar
   *  coordinates that corresponds to the point `(x, y)` in
   *  Cartesian coordinates.
   *  @group polar-coords
   *
   *  @param x the x coordinate value
   *  @param y the y coordinate value
   *  @return sqrt(`x`² + `y`²) without intermediate overflow or underflow
   */
  def hypot(x: Double, y: Double): Double = java.lang.Math.hypot(x, y)

  // -----------------------------------------------------------------------
  // rounding functions
  // -----------------------------------------------------------------------

  /**
   *  @group rounding
   *
   *  @param x the value to be rounded up
   */
  def ceil(x: Double): Double  = java.lang.Math.ceil(x)
  /**
   *  @group rounding
   *
   *  @param x the value to be rounded down
   */
  def floor(x: Double): Double = java.lang.Math.floor(x)

  /** Returns the `Double` value that is closest in value to the
   *  argument and is equal to a mathematical integer.
   *
   *  @param  x a `Double` value
   *  @return the closest floating-point value to a that is equal to a
   *          mathematical integer.
   *  @group rounding
   */
  def rint(x: Double): Double = java.lang.Math.rint(x)

  /** There is no reason to round a `Long`, but this method prevents unintended conversion to `Float` followed by rounding to `Int`.
   *
   *  @note Does not forward to [[java.lang.Math]]
   *  @group rounding
   */
  @deprecated("This is an integer type; there is no reason to round it. Perhaps you meant to call this with a floating-point value?", "2.11.0")
  def round(x: Long): Long = x

  /** Returns the closest `Int` to the argument.
   *
   *  @param  x a floating-point value to be rounded to a `Int`.
   *  @return the value of the argument rounded to the nearest `Int` value.
   *  @group rounding
   */
  def round(x: Float): Int = java.lang.Math.round(x)

  /** Returns the closest `Long` to the argument.
   *
   *  @param  x a floating-point value to be rounded to a `Long`.
   *  @return the value of the argument rounded to the nearest `Long` value.
   *  @group rounding
   */
  def round(x: Double): Long = java.lang.Math.round(x)

  /**
   *  @group abs
   *
   *  @param x the value whose absolute value is to be determined
   */
  def abs(x: Int): Int       = java.lang.Math.abs(x)
  /**
   *  @group abs
   *
   *  @param x the value whose absolute value is to be determined
   */
  def abs(x: Long): Long     = java.lang.Math.abs(x)
  /**
   *  @group abs
   *
   *  @param x the value whose absolute value is to be determined
   */
  def abs(x: Float): Float   = java.lang.Math.abs(x)
  /**
   *  @group abs
   *
   *  @param x the value whose absolute value is to be determined
   */
  def abs(x: Double): Double = java.lang.Math.abs(x)

  /**
   *  @group minmax
   *
   *  @param x the first value to compare
   *  @param y the second value to compare
   */
  def max(x: Int, y: Int): Int          = java.lang.Math.max(x, y)
  /**
   *  @group minmax
   *
   *  @param x the first value to compare
   *  @param y the second value to compare
   */
  def max(x: Long, y: Long): Long       = java.lang.Math.max(x, y)
  /**
   *  @group minmax
   *
   *  @param x the first value to compare
   *  @param y the second value to compare
   */
  def max(x: Float, y: Float): Float    = java.lang.Math.max(x, y)
  /**
   *  @group minmax
   *
   *  @param x the first value to compare
   *  @param y the second value to compare
   */
  def max(x: Double, y: Double): Double = java.lang.Math.max(x, y)

  /**
   *  @group minmax
   *
   *  @param x the first value to compare
   *  @param y the second value to compare
   */
  def min(x: Int, y: Int): Int          = java.lang.Math.min(x, y)
  /**
   *  @group minmax
   *
   *  @param x the first value to compare
   *  @param y the second value to compare
   */
  def min(x: Long, y: Long): Long       = java.lang.Math.min(x, y)
  /**
   *  @group minmax
   *
   *  @param x the first value to compare
   *  @param y the second value to compare
   */
  def min(x: Float, y: Float): Float    = java.lang.Math.min(x, y)
  /**
   *  @group minmax
   *
   *  @param x the first value to compare
   *  @param y the second value to compare
   */
  def min(x: Double, y: Double): Double = java.lang.Math.min(x, y)

  /**
   *  @group signs
   *  @note Forwards to [[java.lang.Integer]]
   *
   *  @param x the value whose signum is to be computed
   */
  def signum(x: Int): Int       = java.lang.Integer.signum(x)
  /**
   *  @group signs
   *  @note Forwards to [[java.lang.Long]]
   *
   *  @param x the value whose signum is to be computed
   */
  def signum(x: Long): Long     = java.lang.Long.signum(x)
  /**
   *  @group signs
   *
   *  @param x the value whose signum is to be computed
   */
  def signum(x: Float): Float   = java.lang.Math.signum(x)
  /**
   *  @group signs
   *
   *  @param x the value whose signum is to be computed
   */
  def signum(x: Double): Double = java.lang.Math.signum(x)

  /**
   *  @group modquo
   *
   *  @param x the dividend
   *  @param y the divisor
   */
  def floorDiv(x: Int, y: Int): Int = java.lang.Math.floorDiv(x, y)

  /**
   *  @group modquo
   *
   *  @param x the dividend
   *  @param y the divisor
   */
  def floorDiv(x: Long, y: Long): Long = java.lang.Math.floorDiv(x, y)

  /**
   *  @group modquo
   *
   *  @param x the dividend
   *  @param y the divisor
   */
  def floorMod(x: Int, y: Int): Int = java.lang.Math.floorMod(x, y)

  /**
   *  @group modquo
   *
   *  @param x the dividend
   *  @param y the divisor
   */
  def floorMod(x: Long, y: Long): Long = java.lang.Math.floorMod(x, y)

  /**
   *  @group signs
   *
   *  @param magnitude the value providing the magnitude of the result
   *  @param sign the value providing the sign of the result
   */
  def copySign(magnitude: Double, sign: Double): Double = java.lang.Math.copySign(magnitude, sign)

  /**
   *  @group signs
   *
   *  @param magnitude the value providing the magnitude of the result
   *  @param sign the value providing the sign of the result
   */
  def copySign(magnitude: Float, sign: Float): Float = java.lang.Math.copySign(magnitude, sign)

  /**
   *  @group adjacent-float
   *
   *  @param start the starting floating-point value
   *  @param direction the value indicating which of `start`'s neighbors should be returned
   */
  def nextAfter(start: Double, direction: Double): Double = java.lang.Math.nextAfter(start, direction)

  /**
   *  @group adjacent-float
   *
   *  @param start the starting floating-point value
   *  @param direction the value indicating which of `start`'s neighbors should be returned
   */
  def nextAfter(start: Float, direction: Double): Float = java.lang.Math.nextAfter(start, direction)

  /**
   *  @group adjacent-float
   *
   *  @param d the starting floating-point value
   */
  def nextUp(d: Double): Double = java.lang.Math.nextUp(d)

  /**
   *  @group adjacent-float
   *
   *  @param f the starting floating-point value
   */
  def nextUp(f: Float): Float = java.lang.Math.nextUp(f)

  /**
   *  @group adjacent-float
   *
   *  @param d the starting floating-point value
   */
  def nextDown(d: Double): Double = java.lang.Math.nextDown(d)

  /**
   *  @group adjacent-float
   *
   *  @param f the starting floating-point value
   */
  def nextDown(f: Float): Float = java.lang.Math.nextDown(f)

  /**
   *  @group scaling
   *
   *  @param d the value to be scaled by a power of two
   *  @param scaleFactor the power of 2 used to scale `d`
   */
  def scalb(d: Double, scaleFactor: Int): Double = java.lang.Math.scalb(d, scaleFactor)

  /**
   *  @group scaling
   *
   *  @param f the value to be scaled by a power of two
   *  @param scaleFactor the power of 2 used to scale `f`
   */
  def scalb(f: Float, scaleFactor: Int): Float = java.lang.Math.scalb(f, scaleFactor)

  // -----------------------------------------------------------------------
  // root functions
  // -----------------------------------------------------------------------

  /** Returns the square root of a `Double` value.
   *
   *  @param  x the number to take the square root of
   *  @return the value √x
   *  @group root-extraction
   */
  def sqrt(x: Double): Double = java.lang.Math.sqrt(x)

  /** Returns the cube root of the given `Double` value.
   *
   *  @param  x the number to take the cube root of
   *  @return the value ∛x
   *  @group root-extraction
   */
  def cbrt(x: Double): Double = java.lang.Math.cbrt(x)

  // -----------------------------------------------------------------------
  // exponential functions
  // -----------------------------------------------------------------------

  /** Returns the value of the first argument raised to the power of the
   *  second argument.
   *
   *  @param x the base.
   *  @param y the exponent.
   *  @return the value `x^y^`.
   *  @group explog
   */
  def pow(x: Double, y: Double): Double = java.lang.Math.pow(x, y)

  /** Returns Euler's number `e` raised to the power of a `Double` value.
   *
   *  @param  x the exponent to raise `e` to.
   *  @return the value `e^x^`, where `e` is the base of the natural
   *          logarithms.
   *  @group explog
   */
  def exp(x: Double): Double = java.lang.Math.exp(x)

  /** Returns `exp(x) - 1`.
   *  @group explog
   *
   *  @param x the exponent to raise `e` to in the computation of `e`^`x`^ - 1
   */
  def expm1(x: Double): Double = java.lang.Math.expm1(x)

  /**
   *  @group explog
   *
   *  @param f the `Float` value whose unbiased exponent is to be extracted
   */
  def getExponent(f: Float): Int = java.lang.Math.getExponent(f)

  /**
   *  @group explog
   *
   *  @param d the `Double` value whose unbiased exponent is to be extracted
   */
  def getExponent(d: Double): Int = java.lang.Math.getExponent(d)

  // -----------------------------------------------------------------------
  // logarithmic functions
  // -----------------------------------------------------------------------

  /** Returns the natural logarithm of a `Double` value.
   *
   *  @param  x the number to take the natural logarithm of
   *  @return the value `logₑ(x)` where `e` is Euler's number
   *  @group explog
   */
  def log(x: Double): Double = java.lang.Math.log(x)

  /** Returns the natural logarithm of the sum of the given `Double` value and 1.
   *  @group explog
   *
   *  @param x the value for which to compute `ln(1 + x)`
   */
  def log1p(x: Double): Double = java.lang.Math.log1p(x)

  /** Returns the base 10 logarithm of the given `Double` value.
   *  @group explog
   *
   *  @param x the value whose base 10 logarithm is to be computed
   */
  def log10(x: Double): Double = java.lang.Math.log10(x)

  // -----------------------------------------------------------------------
  // trigonometric functions
  // -----------------------------------------------------------------------

  /** Returns the hyperbolic sine of the given `Double` value.
   *  @group hyperbolic
   *
   *  @param x the value whose hyperbolic sine is to be returned
   */
  def sinh(x: Double): Double = java.lang.Math.sinh(x)

  /** Returns the hyperbolic cosine of the given `Double` value.
   *  @group hyperbolic
   *
   *  @param x the value whose hyperbolic cosine is to be returned
   */
  def cosh(x: Double): Double = java.lang.Math.cosh(x)

  /** Returns the hyperbolic tangent of the given `Double` value.
   *  @group hyperbolic
   *
   *  @param x the value whose hyperbolic tangent is to be returned
   */
  def tanh(x: Double):Double = java.lang.Math.tanh(x)

  // -----------------------------------------------------------------------
  // miscellaneous functions
  // -----------------------------------------------------------------------

  /** Returns the size of an ulp of the given `Double` value.
   *  @group ulp
   *
   *  @param x the `Double` value whose ulp is to be returned
   */
  def ulp(x: Double): Double = java.lang.Math.ulp(x)

  /** Returns the size of an ulp of the given `Float` value.
   *  @group ulp
   *
   *  @param x the `Float` value whose ulp is to be returned
   */
  def ulp(x: Float): Float = java.lang.Math.ulp(x)

  /**
   *  @group exact
   *
   *  @param x the dividend value
   *  @param y the divisor value
   */
  def IEEEremainder(x: Double, y: Double): Double = java.lang.Math.IEEEremainder(x, y)

  // -----------------------------------------------------------------------
  // exact functions
  // -----------------------------------------------------------------------

  /**
   *  @group exact
   *
   *  @param x the first addend
   *  @param y the second addend
   */
  def addExact(x: Int, y: Int): Int = java.lang.Math.addExact(x, y)

  /**
   *  @group exact
   *
   *  @param x the first addend
   *  @param y the second addend
   */
  def addExact(x: Long, y: Long): Long = java.lang.Math.addExact(x, y)

  /**
   *  @group exact
   *
   *  @param x the minuend
   *  @param y the subtrahend
   */
  def subtractExact(x: Int, y: Int): Int = java.lang.Math.subtractExact(x, y)

  /**
   *  @group exact
   *
   *  @param x the minuend
   *  @param y the subtrahend
   */
  def subtractExact(x: Long, y: Long): Long = java.lang.Math.subtractExact(x, y)

  /**
   *  @group exact
   *
   *  @param x the first factor
   *  @param y the second factor
   */
  def multiplyExact(x: Int, y: Int): Int = java.lang.Math.multiplyExact(x, y)

  /**
   *  @group exact
   *
   *  @param x the first factor
   *  @param y the second factor
   */
  def multiplyExact(x: Long, y: Long): Long = java.lang.Math.multiplyExact(x, y)

  /**
   *  @group exact
   *
   *  @param x the value to be incremented
   */
  def incrementExact(x: Int): Int = java.lang.Math.incrementExact(x)

  /**
   *  @group exact
   *
   *  @param x the value to be incremented
   */
  def incrementExact(x: Long) =  java.lang.Math.incrementExact(x)

  /**
   *  @group exact
   *
   *  @param x the value to be decremented
   */
  def decrementExact(x: Int) =  java.lang.Math.decrementExact(x)

  /**
   *  @group exact
   *
   *  @param x the value to be decremented
   */
  def decrementExact(x: Long) =  java.lang.Math.decrementExact(x)

  /**
   *  @group exact
   *
   *  @param x the value to be negated
   */
  def negateExact(x: Int) =  java.lang.Math.negateExact(x)

  /**
   *  @group exact
   *
   *  @param x the value to be negated
   */
  def negateExact(x: Long) =  java.lang.Math.negateExact(x)

  /**
   *  @group exact
   *
   *  @param x the `Long` value to convert to an `Int`
   */
  def toIntExact(x: Long): Int = java.lang.Math.toIntExact(x)

}
