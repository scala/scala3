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

/** A `@FunctionalInterface` specialization of [[scala.Function0]] returning an
 *  unboxed `Short`, allowing a Java lambda or method reference (and the
 *  compiler's `invokedynamic` lambda encoding) to implement `Function0[Short]`
 *  without boxing.
 */
@FunctionalInterface trait JFunction0$mcS$sp extends Function0[Any] with Serializable {
  /** Applies this function, returning the result as an unboxed `Short`. */
  def apply$mcS$sp(): Short
  /** Applies this function by delegating to `apply$mcS$sp`, boxing the
   *  `Short` result.
   */
  override def apply(): Any = scala.runtime.BoxesRunTime.boxToShort(apply$mcS$sp())
}
