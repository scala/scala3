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
 *  function from an unboxed `Double` and an unboxed `Int` to `Unit`,
 *  allowing a Java lambda or method reference (and the compiler's
 *  `invokedynamic` lambda encoding) to implement
 *  `Function2[Double, Int, Unit]` without boxing.
 */
@FunctionalInterface trait JFunction2$mcVDI$sp extends Function2[Any, Any, Any] with Serializable {
  /** Applies this function to the given arguments for its side effects.
   *
   *  @param v1 the 1st argument, as an unboxed `Double`
   *  @param v2 the 2nd argument, as an unboxed `Int`
   */
  def apply$mcVDI$sp(v1: Double, v2: Int): Unit
  /** Applies this function to the given arguments by delegating to
   *  `apply$mcVDI$sp`, unboxing the arguments.
   *
   *  @param v1 the 1st argument, unboxed to a `Double`
   *  @param v2 the 2nd argument, unboxed to an `Int`
   *  @return the boxed unit value, `BoxedUnit.UNIT`
   */
  override def apply(v1: Any, v2: Any): Any = {
    apply$mcVDI$sp(scala.runtime.BoxesRunTime.unboxToDouble(v1), scala.runtime.BoxesRunTime.unboxToInt(v2))
    scala.runtime.BoxedUnit.UNIT
  }
}
