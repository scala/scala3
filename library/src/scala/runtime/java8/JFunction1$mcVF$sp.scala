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

/** A `@FunctionalInterface` specialization of [[scala.Function1]] for a
 *  function from an unboxed `Float` to `Unit`, allowing a Java lambda or
 *  method reference (and the compiler's `invokedynamic` lambda encoding) to
 *  implement `Function1[Float, Unit]` without boxing.
 */
@FunctionalInterface trait JFunction1$mcVF$sp extends Function1[Any, Any] with Serializable {
  /** Applies this function to the given argument for its side effects.
   *
   *  @param v1 the argument, as an unboxed `Float`
   */
  def apply$mcVF$sp(v1: Float): Unit
  /** Applies this function to the given argument by delegating to
   *  `apply$mcVF$sp`, unboxing the argument.
   *
   *  @param t the argument, unboxed to a `Float`
   *  @return the boxed unit value, `BoxedUnit.UNIT`
   */
  override def apply(t: Any): Any = {
    apply$mcVF$sp(scala.runtime.BoxesRunTime.unboxToFloat(t))
    scala.runtime.BoxedUnit.UNIT
  }
}
