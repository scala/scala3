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

/** A tuple of 1 elements; the canonical representation of a [[scala.Product1]].
 *
 *  @constructor  Create a new tuple with 1 elements.
 *  @param  _1   the 1st element of this Tuple1
 */
final case class Tuple1[@specialized(Int, Long, Double) +T1](_1: T1)
  extends Product1[T1]
{
  /** Returns a string representation of this tuple, the element enclosed in parentheses. */
  override def toString(): String = "(" + _1 + ")"
  
}
