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

/** A `@FunctionalInterface` specialization of [[scala.Function0]] returning
 *  `Unit`, allowing a Java lambda or method reference (and the compiler's
 *  `invokedynamic` lambda encoding) to implement `Function0[Unit]` without
 *  boxing.
 */
@FunctionalInterface trait JFunction0$mcV$sp extends Function0[Any] with Serializable {
  /** Applies this function for its side effects. */
  def apply$mcV$sp(): Unit
  /** Applies this function by delegating to `apply$mcV$sp`, then returns the
   *  boxed unit value, `BoxedUnit.UNIT`.
   */
  override def apply(): Any = {
    apply$mcV$sp()
    scala.runtime.BoxedUnit.UNIT
  }
}
