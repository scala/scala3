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

trait Integral[T] extends Numeric[T] {
  def quot(x: T, y: T): T
  def rem(x: T, y: T): T

  extension (lhs: T) {
    def /(rhs: T): T = quot(lhs, rhs)

    def %(rhs: T): T = rem(lhs, rhs)

    def /%(rhs: T): (T, T) = (quot(lhs, rhs), rem(lhs, rhs))
  }
  
  @deprecated("use the extension methods available instead", since = "3.10.0")
  class IntegralOps(lhs: T) extends NumericOps(lhs) {
    def /(rhs: T) = quot(lhs, rhs)
    def %(rhs: T) = rem(lhs, rhs)
    def /%(rhs: T) = (quot(lhs, rhs), rem(lhs, rhs))
  }
  @deprecated("use the extension methods available instead", since = "3.10.0")
  override def mkNumericOps(lhs: T): IntegralOps = new IntegralOps(lhs)
}

object Integral {
  @inline def apply[T](implicit int: Integral[T]): Integral[T] = int

  trait ExtraImplicits {
    /** The regrettable design of Numeric/Integral/Fractional has them all
     *  bumping into one another when searching for this implicit, so they
     *  are exiled into their own companions.
     *
     *  @tparam T the numeric type for which an `Integral` instance exists
     *  @param x the value to wrap with integral operator syntax (`/`, `%`, `/%`)
     *  @param num the implicit `Integral` instance for type `T`
     *  @return an `IntegralOps` instance providing integral operators on `x`
     */
    @deprecated("use the extension methods available instead", since = "3.10.0")
    def infixIntegralOps[T](x: T)(implicit num: Integral[T]): Integral[T]#IntegralOps = new num.IntegralOps(x)
  }
  object Implicits extends ExtraImplicits
}
