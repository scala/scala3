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

/** A function of 5 parameters.
 *
 *  @tparam T1 the type of the first argument
 *  @tparam T2 the type of the second argument
 *  @tparam T3 the type of the third argument
 *  @tparam T4 the type of the fourth argument
 *  @tparam T5 the type of the fifth argument
 *  @tparam R the return type of this function
 */
trait Function5[-T1, -T2, -T3, -T4, -T5, +R] extends AnyRef {
  /** Applies the body of this function to the arguments.
   *
   *  @param v1 the first argument
   *  @param v2 the second argument
   *  @param v3 the third argument
   *  @param v4 the fourth argument
   *  @param v5 the fifth argument
   *  @return   the result of function application.
   */
  def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5): R
  /** Creates a curried version of this function.
   *
   *  @return   a function `f` such that `f(x1)(x2)(x3)(x4)(x5) == apply(x1, x2, x3, x4, x5)`
   */
  @annotation.unspecialized def curried: T1 => T2 => T3 => T4 => T5 => R = {
    (x1: T1) => ((x2: T2, x3: T3, x4: T4, x5: T5) => this.apply(x1, x2, x3, x4, x5)).curried
  }
  /** Creates a tupled version of this function: instead of 5 arguments,
   *  it accepts a single [[scala.Tuple5]] argument.
   *
   *  @return   a function `f` such that `f((x1, x2, x3, x4, x5)) == f(Tuple5(x1, x2, x3, x4, x5)) == apply(x1, x2, x3, x4, x5)`
   */

  @annotation.unspecialized def tupled: ((T1, T2, T3, T4, T5)) => R = {
    case ((x1, x2, x3, x4, x5)) => apply(x1, x2, x3, x4, x5)
  }
  override def toString(): String = "<function5>"
}
