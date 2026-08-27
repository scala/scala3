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

/** A function of 11 parameters.
 *
 *  @tparam T1 the type of the 1st parameter of this function
 *  @tparam T2 the type of the 2nd parameter of this function
 *  @tparam T3 the type of the 3rd parameter of this function
 *  @tparam T4 the type of the 4th parameter of this function
 *  @tparam T5 the type of the 5th parameter of this function
 *  @tparam T6 the type of the 6th parameter of this function
 *  @tparam T7 the type of the 7th parameter of this function
 *  @tparam T8 the type of the 8th parameter of this function
 *  @tparam T9 the type of the 9th parameter of this function
 *  @tparam T10 the type of the 10th parameter of this function
 *  @tparam T11 the type of the 11th parameter of this function
 *  @tparam R the return type of this function
 */
trait Function11[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, +R] extends AnyRef {
  /** Applies the body of this function to the arguments.
   *
   *  @param v1 the value of the 1st parameter
   *  @param v2 the value of the 2nd parameter
   *  @param v3 the value of the 3rd parameter
   *  @param v4 the value of the 4th parameter
   *  @param v5 the value of the 5th parameter
   *  @param v6 the value of the 6th parameter
   *  @param v7 the value of the 7th parameter
   *  @param v8 the value of the 8th parameter
   *  @param v9 the value of the 9th parameter
   *  @param v10 the value of the 10th parameter
   *  @param v11 the value of the 11th parameter
   *  @return   the result of function application.
   */
  def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11): R
  /** Creates a curried version of this function.
   *
   *  @return   a function `f` such that `f(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x9)(x10)(x11) == apply(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)`
   */
  @annotation.unspecialized def curried: T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => R = {
    (x1: T1) => ((x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11) => this.apply(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)).curried
  }
  /** Creates a tupled version of this function: instead of 11 arguments,
   *  it accepts a single [[scala.Tuple11]] argument.
   *
   *  @return   a function `f` such that `f((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)) == f(Tuple11(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)) == apply(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)`
   */

  @annotation.unspecialized def tupled: ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)) => R = {
    case ((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)) => apply(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
  }
  override def toString(): String = "<function11>"
}
