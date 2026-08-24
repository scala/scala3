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

/** A tuple of 14 elements; the canonical representation of a [[scala.Product14]].
 *
 *  @constructor  Create a new tuple with 14 elements. Note that it is more idiomatic to create a Tuple14 via `(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)`
 *  @tparam T1 the type of the 1st element
 *  @tparam T2 the type of the 2nd element
 *  @tparam T3 the type of the 3rd element
 *  @tparam T4 the type of the 4th element
 *  @tparam T5 the type of the 5th element
 *  @tparam T6 the type of the 6th element
 *  @tparam T7 the type of the 7th element
 *  @tparam T8 the type of the 8th element
 *  @tparam T9 the type of the 9th element
 *  @tparam T10 the type of the 10th element
 *  @tparam T11 the type of the 11th element
 *  @tparam T12 the type of the 12th element
 *  @tparam T13 the type of the 13th element
 *  @tparam T14 the type of the 14th element
 *  @param  _1   the 1st element of this Tuple14
 *  @param  _2   the 2nd element of this Tuple14
 *  @param  _3   the 3rd element of this Tuple14
 *  @param  _4   the 4th element of this Tuple14
 *  @param  _5   the 5th element of this Tuple14
 *  @param  _6   the 6th element of this Tuple14
 *  @param  _7   the 7th element of this Tuple14
 *  @param  _8   the 8th element of this Tuple14
 *  @param  _9   the 9th element of this Tuple14
 *  @param  _10   the 10th element of this Tuple14
 *  @param  _11   the 11th element of this Tuple14
 *  @param  _12   the 12th element of this Tuple14
 *  @param  _13   the 13th element of this Tuple14
 *  @param  _14   the 14th element of this Tuple14
 */
final case class Tuple14[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14)
  extends Product14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]
{
  /** Returns a string representation of this tuple, the elements separated by commas and enclosed in parentheses. */
  override def toString(): String = "(" + _1 + "," + _2 + "," + _3 + "," + _4 + "," + _5 + "," + _6 + "," + _7 +
    "," + _8 + "," + _9 + "," + _10 + "," + _11 + "," + _12 + "," + _13 + "," + _14 + ")"
  
}
