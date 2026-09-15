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
 *  unboxed `Long`, allowing a Java lambda or method reference (and the
 *  compiler's `invokedynamic` lambda encoding) to implement `Function0[Long]`
 *  without boxing.
 */
@FunctionalInterface trait JFunction0$mcJ$sp extends Function0[Any] with Serializable {
  /** Applies this function, returning the result as an unboxed `Long`. */
  def apply$mcJ$sp(): Long
  /** Applies this function by delegating to `apply$mcJ$sp`, boxing the
   *  `Long` result.
   */
  override def apply(): Any = scala.runtime.BoxesRunTime.boxToLong(apply$mcJ$sp())
}
