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

package scala.ref

import scala.language.`2.13`

/**
 *  @see `java.lang.ref.Reference`
 *
 *  @tparam T the type of the referenced object, constrained to `AnyRef` (i.e., non-primitive types)
 */
trait Reference[+T <: AnyRef] extends Function0[T] {
  /** Returns the underlying value. */
  def apply(): T
  /** Returns `Some` underlying if it hasn't been collected, otherwise `None`. */
  def get: Option[T]
  /** Returns the string representation of the referenced value, or
   *  `"<deleted>"` if it has been cleared or collected.
   */
  override def toString(): String = get.map(_.toString).getOrElse("<deleted>")
  /** Clears this reference so that it no longer refers to its referent. This
   *  does not enqueue the reference.
   */
  def clear(): Unit
  /** Adds this reference to the reference queue with which it was registered,
   *  if any. Returns `true` if it was successfully enqueued, or `false` if it
   *  was already enqueued or was not registered with a queue.
   */
  def enqueue(): Boolean
  /** Tests whether this reference is currently enqueued in the reference queue
   *  with which it was registered, if any.
   */
  def isEnqueued: Boolean
}
