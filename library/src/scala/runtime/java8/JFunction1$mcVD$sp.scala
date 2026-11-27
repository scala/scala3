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
 *  function from an unboxed `Double` to `Unit`, allowing a Java lambda or
 *  method reference (and the compiler's `invokedynamic` lambda encoding) to
 *  implement `Function1[Double, Unit]` without boxing.
 */
@FunctionalInterface trait JFunction1$mcVD$sp extends Function1[Any, Any] with Serializable {
  /** Applies this function to the given argument for its side effects.
   *
   *  @param v1 the argument, as an unboxed `Double`
   */
  def apply$mcVD$sp(v1: Double): Unit
  /** Applies this function to the given argument by delegating to
   *  `apply$mcVD$sp`, unboxing the argument.
   *
   *  @param t the argument, unboxed to a `Double`
   *  @return the boxed unit value, `BoxedUnit.UNIT`
   */
  override def apply(t: Any): Any = {
    apply$mcVD$sp(scala.runtime.BoxesRunTime.unboxToDouble(t))
    scala.runtime.BoxedUnit.UNIT
  }
}
