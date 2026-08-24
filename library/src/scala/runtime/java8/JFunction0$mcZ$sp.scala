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
 *  unboxed `Boolean`, allowing a Java lambda or method reference (and the
 *  compiler's `invokedynamic` lambda encoding) to implement `Function0[Boolean]`
 *  without boxing.
 */
@FunctionalInterface trait JFunction0$mcZ$sp extends Function0[Any] with Serializable {
  /** Applies this function, returning the result as an unboxed `Boolean`. */
  def apply$mcZ$sp(): Boolean
  /** Applies this function by delegating to `apply$mcZ$sp`, boxing the
   *  `Boolean` result.
   */
  override def apply(): Any = scala.runtime.BoxesRunTime.boxToBoolean(apply$mcZ$sp())
}
