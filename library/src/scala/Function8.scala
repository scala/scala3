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

// GENERATED CODE: DO NOT EDIT. See scala.Function0 for timestamp.

package scala

import scala.language.`2.13`

/** A function of 8 parameters.
 *
 *  @tparam T1 the type of the first argument
 *  @tparam T2 the type of the second argument
 *  @tparam T3 the type of the third argument
 *  @tparam T4 the type of the fourth argument
 *  @tparam T5 the type of the fifth argument
 *  @tparam T6 the type of the sixth argument
 *  @tparam T7 the type of the seventh argument
 *  @tparam T8 the type of the eighth argument
 *  @tparam R the return type of the function
 */
trait Function8[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, +R] extends AnyRef {
  /** Applies the body of this function to the arguments.
   *
   *  @param v1 the first argument
   *  @param v2 the second argument
   *  @param v3 the third argument
   *  @param v4 the fourth argument
   *  @param v5 the fifth argument
   *  @param v6 the sixth argument
   *  @param v7 the seventh argument
   *  @param v8 the eighth argument
   *  @return   the result of function application.
   */
  def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8): R
  /** Creates a curried version of this function.
   *
   *  @return   a function `f` such that `f(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8) == apply(x1, x2, x3, x4, x5, x6, x7, x8)`
   */
  @annotation.unspecialized def curried: T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => R = {
    (x1: T1) => ((x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8) => this.apply(x1, x2, x3, x4, x5, x6, x7, x8)).curried
  }
  /** Creates a tupled version of this function: instead of 8 arguments,
   *  it accepts a single [[scala.Tuple8]] argument.
   *
   *  @return   a function `f` such that `f((x1, x2, x3, x4, x5, x6, x7, x8)) == f(Tuple8(x1, x2, x3, x4, x5, x6, x7, x8)) == apply(x1, x2, x3, x4, x5, x6, x7, x8)`
   */

  @annotation.unspecialized def tupled: ((T1, T2, T3, T4, T5, T6, T7, T8)) => R = {
    case ((x1, x2, x3, x4, x5, x6, x7, x8)) => apply(x1, x2, x3, x4, x5, x6, x7, x8)
  }
  override def toString(): String = "<function8>"
}
