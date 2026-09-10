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
 *  function from an unboxed `Int` to an unboxed `Float`, allowing a Java
 *  lambda or method reference (and the compiler's `invokedynamic` lambda
 *  encoding) to implement `Function1[Int, Float]` without boxing.
 */
@FunctionalInterface trait JFunction1$mcFI$sp extends Function1[Any, Any] with Serializable {
  /** Applies this function to the given argument.
   *
   *  @param v1 the argument, as an unboxed `Int`
   *  @return the result of applying this function, as an unboxed `Float`
   */
  def apply$mcFI$sp(v1: Int): Float
  /** Applies this function to the given argument by delegating to
   *  `apply$mcFI$sp`, unboxing the argument and boxing the result.
   *
   *  @param t the argument, unboxed to an `Int`
   *  @return the result of applying this function, as a boxed `Float`
   */
  override def apply(t: Any): Any = scala.runtime.BoxesRunTime.boxToFloat(apply$mcFI$sp(scala.runtime.BoxesRunTime.unboxToInt(t)))
}
