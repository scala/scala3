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
 *  function from an unboxed `Int` and an unboxed `Long` to `Unit`,
 *  allowing a Java lambda or method reference (and the compiler's
 *  `invokedynamic` lambda encoding) to implement
 *  `Function2[Int, Long, Unit]` without boxing.
 */
@FunctionalInterface trait JFunction2$mcVIJ$sp extends Function2[Any, Any, Any] with Serializable {
  /** Applies this function to the given arguments for its side effects.
   *
   *  @param v1 the 1st argument, as an unboxed `Int`
   *  @param v2 the 2nd argument, as an unboxed `Long`
   */
  def apply$mcVIJ$sp(v1: Int, v2: Long): Unit
  /** Applies this function to the given arguments by delegating to
   *  `apply$mcVIJ$sp`, unboxing the arguments.
   *
   *  @param v1 the 1st argument, unboxed to an `Int`
   *  @param v2 the 2nd argument, unboxed to a `Long`
   *  @return the boxed unit value, `BoxedUnit.UNIT`
   */
  override def apply(v1: Any, v2: Any): Any = {
    apply$mcVIJ$sp(scala.runtime.BoxesRunTime.unboxToInt(v1), scala.runtime.BoxesRunTime.unboxToLong(v2))
    scala.runtime.BoxedUnit.UNIT
  }
}
