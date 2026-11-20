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

/** A tuple of 8 elements; the canonical representation of a [[scala.Product8]].
 *
 *  @constructor  Create a new tuple with 8 elements. Note that it is more idiomatic to create a Tuple8 via `(t1, t2, t3, t4, t5, t6, t7, t8)`
 *  @tparam T1 the type of the 1st element
 *  @tparam T2 the type of the 2nd element
 *  @tparam T3 the type of the 3rd element
 *  @tparam T4 the type of the 4th element
 *  @tparam T5 the type of the 5th element
 *  @tparam T6 the type of the 6th element
 *  @tparam T7 the type of the 7th element
 *  @tparam T8 the type of the 8th element
 *  @param  _1   the 1st element of this Tuple8
 *  @param  _2   the 2nd element of this Tuple8
 *  @param  _3   the 3rd element of this Tuple8
 *  @param  _4   the 4th element of this Tuple8
 *  @param  _5   the 5th element of this Tuple8
 *  @param  _6   the 6th element of this Tuple8
 *  @param  _7   the 7th element of this Tuple8
 *  @param  _8   the 8th element of this Tuple8
 */
final case class Tuple8[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8)
  extends Product8[T1, T2, T3, T4, T5, T6, T7, T8]
{
  /** Returns a string representation of this tuple, with the eight elements separated by commas and enclosed in parentheses, as in `(1,two,3.0,four,5,six,7,eight)`. */
  override def toString(): String = "(" + _1 + "," + _2 + "," + _3 + "," + _4 + "," + _5 + "," + _6 + "," + _7 + "," + _8 + ")"
  
}
