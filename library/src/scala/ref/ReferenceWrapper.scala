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
import scala.annotation.nowarn

@nowarn("cat=deprecation")
/** Implements the Scala [[scala.ref.Reference]] interface by delegating to an
 *  underlying `java.lang.ref.Reference`.
 *
 *  @tparam T the type of the referenced object, constrained to `AnyRef` (i.e., non-primitive types)
 */
trait ReferenceWrapper[+T <: AnyRef] extends Reference[T] with Proxy {
  val underlying: java.lang.ref.Reference[? <: T]
  /** Returns `Some(value)` wrapping `underlying.get` if it is non-null, or `None` otherwise.
   *  For phantom references, `underlying.get` always returns `null`, so this is always `None`.
   */
  override def get = Option(underlying.get)
  /** Returns `underlying.get` when it is non-null, throwing `NoSuchElementException` when it is `null`.
   *  For phantom references, `underlying.get` always returns `null`, so this always throws.
   */
  def apply() = {
    val ret = underlying.get
    if (ret eq null) throw new NoSuchElementException
    ret
  }
  /** Clears this reference so that it no longer refers to its referent; the reference is not enqueued. */
  def clear(): Unit = underlying.clear()
  /** Returns `true` if this reference was successfully added to the reference queue with which it was registered, or `false` if it was already enqueued or was not registered with a queue. */
  def enqueue(): Boolean = underlying.enqueue()
  /** Tests whether this reference is currently enqueued in the reference queue with which it was registered, if any. */
  def isEnqueued: Boolean = underlying.isEnqueued
  /** Returns the underlying `java.lang.ref.Reference`, to which `Proxy` delegates equality, hashing, and string conversion. */
  def self: java.lang.ref.Reference[? <: T] = underlying
}

private trait ReferenceWithWrapper[T <: AnyRef] {
  val wrapper: ReferenceWrapper[T]
}
