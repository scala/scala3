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

package scala.runtime.java8

import scala.language.`2.13`

/** A `@FunctionalInterface` specialization of [[scala.Function2]] for a
 *  function from an unboxed `Double` and an unboxed `Double` to an
 *  unboxed `Long`, allowing a Java lambda or method reference (and the
 *  compiler's `invokedynamic` lambda encoding) to implement
 *  `Function2[Double, Double, Long]` without boxing.
 */
@FunctionalInterface trait JFunction2$mcJDD$sp extends Function2[Any, Any, Any] with Serializable {
  /** Applies this function to the given arguments.
   *
   *  @param v1 the 1st argument, as an unboxed `Double`
   *  @param v2 the 2nd argument, as an unboxed `Double`
   *  @return the result of applying this function, as an unboxed `Long`
   */
  def apply$mcJDD$sp(v1: Double, v2: Double): Long
  /** Applies this function to the given arguments by delegating to
   *  `apply$mcJDD$sp`, unboxing the arguments and boxing the result.
   *
   *  @param v1 the 1st argument, unboxed to a `Double`
   *  @param v2 the 2nd argument, unboxed to a `Double`
   *  @return the result of applying this function, as a boxed `Long`
   */
  override def apply(v1: Any, v2: Any): Any = scala.runtime.BoxesRunTime.boxToLong(apply$mcJDD$sp(scala.runtime.BoxesRunTime.unboxToDouble(v1), scala.runtime.BoxesRunTime.unboxToDouble(v2)))
}
