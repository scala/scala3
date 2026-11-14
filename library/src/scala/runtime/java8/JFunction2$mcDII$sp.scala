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
 *  function from an unboxed `Int` and an unboxed `Int` to an
 *  unboxed `Double`, allowing a Java lambda or method reference (and the
 *  compiler's `invokedynamic` lambda encoding) to implement
 *  `Function2[Int, Int, Double]` without boxing.
 */
@FunctionalInterface trait JFunction2$mcDII$sp extends Function2[Any, Any, Any] with Serializable {
  /** Applies this function to the given arguments.
   *
   *  @param v1 the 1st argument, as an unboxed `Int`
   *  @param v2 the 2nd argument, as an unboxed `Int`
   *  @return the result of applying this function, as an unboxed `Double`
   */
  def apply$mcDII$sp(v1: Int, v2: Int): Double
  /** Applies this function to the given arguments by delegating to
   *  `apply$mcDII$sp`, unboxing the arguments and boxing the result.
   *
   *  @param v1 the 1st argument, unboxed to an `Int`
   *  @param v2 the 2nd argument, unboxed to an `Int`
   *  @return the result of applying this function, as a boxed `Double`
   */
  override def apply(v1: Any, v2: Any): Any = scala.runtime.BoxesRunTime.boxToDouble(apply$mcDII$sp(scala.runtime.BoxesRunTime.unboxToInt(v1), scala.runtime.BoxesRunTime.unboxToInt(v2)))
}
