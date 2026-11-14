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
 *  function from an unboxed `Long` and an unboxed `Long` to an
 *  unboxed `Long`, allowing a Java lambda or method reference (and the
 *  compiler's `invokedynamic` lambda encoding) to implement
 *  `Function2[Long, Long, Long]` without boxing.
 */
@FunctionalInterface trait JFunction2$mcJJJ$sp extends Function2[Any, Any, Any] with Serializable {
  /** Applies this function to the given arguments.
   *
   *  @param v1 the 1st argument, as an unboxed `Long`
   *  @param v2 the 2nd argument, as an unboxed `Long`
   *  @return the result of applying this function, as an unboxed `Long`
   */
  def apply$mcJJJ$sp(v1: Long, v2: Long): Long
  /** Applies this function to the given arguments by delegating to
   *  `apply$mcJJJ$sp`, unboxing the arguments and boxing the result.
   *
   *  @param v1 the 1st argument, unboxed to a `Long`
   *  @param v2 the 2nd argument, unboxed to a `Long`
   *  @return the result of applying this function, as a boxed `Long`
   */
  override def apply(v1: Any, v2: Any): Any = scala.runtime.BoxesRunTime.boxToLong(apply$mcJJJ$sp(scala.runtime.BoxesRunTime.unboxToLong(v1), scala.runtime.BoxesRunTime.unboxToLong(v2)))
}
