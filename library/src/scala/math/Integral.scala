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

/** A typeclass representing integral numeric types.
 *
 *  @tparam T the type of the integral values
 */
trait Integral[T] extends Numeric[T] {
  /** Returns the quotient of `x` divided by `y`.
   *
   *  @param x the dividend
   *  @param y the divisor
   *  @return the quotient of `x` divided by `y`
   */
  def quot(x: T, y: T): T
  /** Returns the remainder of `x` divided by `y`.
   *
   *  @param x the dividend
   *  @param y the divisor
   *  @return the remainder of `x` divided by `y`
   */
  def rem(x: T, y: T): T

  /** Provides integral operations on a value of type `T`.
   *
   *  @param lhs the value to operate on
   */
  class IntegralOps(lhs: T) extends NumericOps(lhs) {
    /** Returns the quotient of this value divided by `rhs`.
     *
     *  @param rhs the divisor
     */
    def /(rhs: T) = quot(lhs, rhs)
    /** Returns the remainder of this value divided by `rhs`.
     *
     *  @param rhs the divisor
     */
    def %(rhs: T) = rem(lhs, rhs)
    /** Returns the quotient and remainder of this value divided by `rhs`, as a `(quotient, remainder)` tuple.
     *
     *  @param rhs the divisor
     */
    def /%(rhs: T) = (quot(lhs, rhs), rem(lhs, rhs))
  }
  /** Returns an `IntegralOps` instance providing integral operators on `lhs`.
   *
   *  @param lhs the value to wrap with integral operations
   */
  override implicit def mkNumericOps(lhs: T): IntegralOps = new IntegralOps(lhs)
}

object Integral {
  /** Returns the implicit `Integral` instance for type `T`.
   *
   *  @tparam T the type for which an `Integral` instance is requested
   *  @param int the implicit `Integral` instance for type `T`
   */
  @inline def apply[T](implicit int: Integral[T]): Integral[T] = int

  /** Provides additional implicit conversions for integral types. */
  trait ExtraImplicits {
    /** The regrettable design of Numeric/Integral/Fractional has them all
     *  bumping into one another when searching for this implicit, so they
     *  are exiled into their own companions.
     *
     *  @tparam T the numeric type for which an `Integral` instance exists
     *  @param x the value to wrap with integral operator syntax (`/`, `%`, `/%`)
     *  @param num the implicit `Integral` instance for type `T`
     *  @return an `Integral[T]#IntegralOps` instance providing integral operators on `x`
     */
    implicit def infixIntegralOps[T](x: T)(implicit num: Integral[T]): Integral[T]#IntegralOps = new num.IntegralOps(x)
  }
  object Implicits extends ExtraImplicits
}
