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

/** A tuple of 3 elements; the canonical representation of a [[scala.Product3]].
 *
 *  @constructor  Create a new tuple with 3 elements. Note that it is more idiomatic to create a Tuple3 via `(t1, t2, t3)`
 *  @param  _1   Element 1 of this Tuple3
 *  @param  _2   Element 2 of this Tuple3
 *  @param  _3   Element 3 of this Tuple3
 */
final case class Tuple3[+T1, +T2, +T3](_1: T1, _2: T2, _3: T3)
  extends Product3[T1, T2, T3]
{
  override def toString(): String = "(" + _1 + "," + _2 + "," + _3 + ")"
  
}
