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

/** A function of 21 parameters.
 *
 *  @tparam T1 the type of the first argument
 *  @tparam T2 the type of the second argument
 *  @tparam T3 the type of the third argument
 *  @tparam T4 the type of the fourth argument
 *  @tparam T5 the type of the fifth argument
 *  @tparam T6 the type of the sixth argument
 *  @tparam T7 the type of the seventh argument
 *  @tparam T8 the type of the eighth argument
 *  @tparam T9 the type of the ninth argument
 *  @tparam T10 the type of the tenth argument
 *  @tparam T11 the type of the eleventh argument
 *  @tparam T12 the type of the twelfth argument
 *  @tparam T13 the type of the thirteenth argument
 *  @tparam T14 the type of the fourteenth argument
 *  @tparam T15 the type of the fifteenth argument
 *  @tparam T16 the type of the sixteenth argument
 *  @tparam T17 the type of the seventeenth argument
 *  @tparam T18 the type of the eighteenth argument
 *  @tparam T19 the type of the nineteenth argument
 *  @tparam T20 the type of the twentieth argument
 *  @tparam T21 the type of the twenty-first argument
 *  @tparam R the result type of the function
 */
trait Function21[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, -T21, +R] extends AnyRef {
  /** Applies the body of this function to the arguments.
   *
   *  @param v1 the first argument of type `T1`
   *  @param v2 the second argument of type `T2`
   *  @param v3 the third argument of type `T3`
   *  @param v4 the fourth argument of type `T4`
   *  @param v5 the fifth argument of type `T5`
   *  @param v6 the sixth argument of type `T6`
   *  @param v7 the seventh argument of type `T7`
   *  @param v8 the eighth argument of type `T8`
   *  @param v9 the ninth argument of type `T9`
   *  @param v10 the tenth argument of type `T10`
   *  @param v11 the eleventh argument of type `T11`
   *  @param v12 the twelfth argument of type `T12`
   *  @param v13 the thirteenth argument of type `T13`
   *  @param v14 the fourteenth argument of type `T14`
   *  @param v15 the fifteenth argument of type `T15`
   *  @param v16 the sixteenth argument of type `T16`
   *  @param v17 the seventeenth argument of type `T17`
   *  @param v18 the eighteenth argument of type `T18`
   *  @param v19 the nineteenth argument of type `T19`
   *  @param v20 the twentieth argument of type `T20`
   *  @param v21 the twenty-first argument of type `T21`
   *  @return   the result of function application.
   */
  def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20, v21: T21): R
  /** Creates a curried version of this function.
   *
   *  @return   a function `f` such that `f(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x9)(x10)(x11)(x12)(x13)(x14)(x15)(x16)(x17)(x18)(x19)(x20)(x21) == apply(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21)`
   */
  @annotation.unspecialized def curried: T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => T12 => T13 => T14 => T15 => T16 => T17 => T18 => T19 => T20 => T21 => R = {
    (x1: T1) => ((x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13, x14: T14, x15: T15, x16: T16, x17: T17, x18: T18, x19: T19, x20: T20, x21: T21) => this.apply(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21)).curried
  }
  /** Creates a tupled version of this function: instead of 21 arguments,
   *  it accepts a single [[scala.Tuple21]] argument.
   *
   *  @return   a function `f` such that `f((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21)) == f(Tuple21(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21)) == apply(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21)`
   */

  @annotation.unspecialized def tupled: ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)) => R = {
    case ((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21)) => apply(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21)
  }
  override def toString(): String = "<function21>"
}
