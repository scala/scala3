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

package scala.runtime

import scala.language.`2.13`
import scala.util.control.ControlThrowable

// remove Unit specialization when binary compatibility permits
// @annotation.nowarn("cat=lint-unit-specialization") TODO: Add warning back when specialization is implemented
/** The control-flow throwable the compiler uses to implement a non-local
 *  return, that is, a `return` inside a closure that exits the enclosing
 *  method.
 *
 *  The compiler compiles such a `return` to throwing an instance of this
 *  class, and wraps the body of the returned-from method in a handler that
 *  catches it: the handler compares `key` with its own marker object by
 *  reference and completes the method with `value` on a match, rethrowing
 *  otherwise, so a throw always unwinds to the invocation it belongs to. As a
 *  [[scala.util.control.ControlThrowable]] it carries no stack trace and is
 *  not matched by `NonFatal`.
 *
 *  @param key the marker object identifying the method invocation to return
 *             from; compared by reference by each enclosing handler
 *  @param value the value returned by the method invocation identified by `key`
 */
class NonLocalReturnControl[@specialized(Byte, Short, Int, Long, Char, Float, Double, Boolean, Unit) T](val key: AnyRef, val value: T) extends ControlThrowable {
  /** Returns `this` without recording a stack trace, keeping the throw cheap;
   *  the exception exists purely for control flow.
   */
  final override def fillInStackTrace(): Throwable = this
}
