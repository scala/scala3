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

/** A tuple of 2 elements; the canonical representation of a [[scala.Product2]].
 *
 *  @constructor  Create a new tuple with 2 elements. Note that it is more idiomatic to create a Tuple2 via `(t1, t2)`
 *  @param  _1   the 1st element of this Tuple2
 *  @param  _2   the 2nd element of this Tuple2
 */
final case class Tuple2[@specialized(Int, Long, Double, Char, Boolean/*, AnyRef*/) +T1, @specialized(Int, Long, Double, Char, Boolean/*, AnyRef*/) +T2](_1: T1, _2: T2)
  extends Product2[T1, T2]
{
  /** Returns a string representation of this tuple, with the two elements separated by a comma and enclosed in parentheses, as in `(1,two)`. */
  override def toString(): String = "(" + _1 + "," + _2 + ")"
  
  /** Swaps the elements of this `Tuple`.
   *  @return a new Tuple where the 1st element is the 2nd element of this Tuple and the
   *  2nd element is the 1st element of this Tuple.
   */
  def swap: Tuple2[T2,T1] = Tuple2(_2, _1)

}
