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
import scala.language.implicitConversions

/** A typeclass for fractional numeric types.
 *
 *  @tparam T the type of the fractional numbers
 */
trait Fractional[T] extends Numeric[T] {
  /** Returns the result of dividing `x` by `y`.
   *
   *  @param x the dividend
   *  @param y the divisor
   *  @return the quotient of `x` divided by `y`
   */
  def div(x: T, y: T): T

  /** Provides fractional operations for a given value.
   *
   *  @param lhs the value to perform operations on
   */
  class FractionalOps(lhs: T) extends NumericOps(lhs) {
    /** Returns the result of dividing `lhs` by `rhs`.
     *
     *  @param rhs the divisor
     */
    def /(rhs: T) = div(lhs, rhs)
  }
  /** Returns an object that provides fractional operations for `lhs`.
   *
   *  @param lhs the value to perform operations on
   *  @return an object providing fractional operations for `lhs`
   */
  override implicit def mkNumericOps(lhs: T): FractionalOps =
    new FractionalOps(lhs)
}

object Fractional {
  /** Returns the `Fractional` instance for type `T`.
   *
   *  @tparam T the type of the fractional numbers
   *  @param frac the implicit `Fractional` instance for `T`
   *  @return the `Fractional` instance for `T`
   */
  @inline def apply[T](implicit frac: Fractional[T]): Fractional[T] = frac

  /** Provides additional implicit conversions for fractional types. */
  trait ExtraImplicits {
    /** Returns an object that provides fractional operations for `x`.
     *
     *  @tparam T the type of the fractional numbers
     *  @param x the value to perform operations on
     *  @param num the implicit `Fractional` instance for `T`
     *  @return an object providing fractional operations for `x`
     */
    implicit def infixFractionalOps[T](x: T)(implicit num: Fractional[T]): Fractional[T]#FractionalOps = new num.FractionalOps(x)
  }
  object Implicits extends ExtraImplicits
}
