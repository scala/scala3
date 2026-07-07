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

/** A function of 22 parameters.
 *
 *  @tparam T1 the type of the 1st argument
 *  @tparam T2 the type of the 2nd argument
 *  @tparam T3 the type of the 3rd argument
 *  @tparam T4 the type of the 4th argument
 *  @tparam T5 the type of the 5th argument
 *  @tparam T6 the type of the 6th argument
 *  @tparam T7 the type of the 7th argument
 *  @tparam T8 the type of the 8th argument
 *  @tparam T9 the type of the 9th argument
 *  @tparam T10 the type of the 10th argument
 *  @tparam T11 the type of the 11th argument
 *  @tparam T12 the type of the 12th argument
 *  @tparam T13 the type of the 13th argument
 *  @tparam T14 the type of the 14th argument
 *  @tparam T15 the type of the 15th argument
 *  @tparam T16 the type of the 16th argument
 *  @tparam T17 the type of the 17th argument
 *  @tparam T18 the type of the 18th argument
 *  @tparam T19 the type of the 19th argument
 *  @tparam T20 the type of the 20th argument
 *  @tparam T21 the type of the 21st argument
 *  @tparam T22 the type of the 22nd argument
 *  @tparam R the result type of the function
 */
trait Function22[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, -T21, -T22, +R] extends AnyRef {
  /** Applies the body of this function to the arguments.
   *
   *  @param v1 the 1st argument of type `T1`
   *  @param v2 the 2nd argument of type `T2`
   *  @param v3 the 3rd argument of type `T3`
   *  @param v4 the 4th argument of type `T4`
   *  @param v5 the 5th argument of type `T5`
   *  @param v6 the 6th argument of type `T6`
   *  @param v7 the 7th argument of type `T7`
   *  @param v8 the 8th argument of type `T8`
   *  @param v9 the 9th argument of type `T9`
   *  @param v10 the 10th argument of type `T10`
   *  @param v11 the 11th argument of type `T11`
   *  @param v12 the 12th argument of type `T12`
   *  @param v13 the 13th argument of type `T13`
   *  @param v14 the 14th argument of type `T14`
   *  @param v15 the 15th argument of type `T15`
   *  @param v16 the 16th argument of type `T16`
   *  @param v17 the 17th argument of type `T17`
   *  @param v18 the 18th argument of type `T18`
   *  @param v19 the 19th argument of type `T19`
   *  @param v20 the 20th argument of type `T20`
   *  @param v21 the 21st argument of type `T21`
   *  @param v22 the 22nd argument of type `T22`
   *  @return   the result of function application.
   */
  def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20, v21: T21, v22: T22): R
  /** Creates a curried version of this function.
   *
   *  @return   a function `f` such that `f(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x9)(x10)(x11)(x12)(x13)(x14)(x15)(x16)(x17)(x18)(x19)(x20)(x21)(x22) == apply(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22)`
   */
  @annotation.unspecialized def curried: T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => T12 => T13 => T14 => T15 => T16 => T17 => T18 => T19 => T20 => T21 => T22 => R = {
    (x1: T1) => ((x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13, x14: T14, x15: T15, x16: T16, x17: T17, x18: T18, x19: T19, x20: T20, x21: T21, x22: T22) => this.apply(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22)).curried
  }
  /** Creates a tupled version of this function: instead of 22 arguments,
   *  it accepts a single [[scala.Tuple22]] argument.
   *
   *  @return   a function `f` such that `f((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22)) == f(Tuple22(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22)) == apply(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22)`
   */

  @annotation.unspecialized def tupled: ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)) => R = {
    case ((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22)) => apply(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22)
  }
  override def toString(): String = "<function22>"
}
