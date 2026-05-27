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

/** A wrapper class for java.lang.ref.WeakReference
 *  The new functionality is (1) results are Option values, instead of using null.
 *  (2) There is an extractor that maps the weak reference itself into an option.
 *
 *  @tparam T the covariant type of the weakly referenced object, must be a subtype of `AnyRef`
 *  @param value the object to be weakly referenced
 *  @param queue an optional reference queue to which the reference will be enqueued when the referent becomes weakly reachable, or `null` for no queue
 */
class WeakReference[+T <: AnyRef](value: T, queue: ReferenceQueue[T] | Null) extends ReferenceWrapper[T] {
  def this(value: T) = this(value, null)
  val underlying: java.lang.ref.WeakReference[? <: T] =
    new WeakReferenceWithWrapper[T](value, queue, this)
}

/** An extractor for weak reference values. */
object WeakReference {

  /** Creates a weak reference pointing to `value`.
   *
   *  @tparam T the type of the value to wrap in a weak reference
   *  @param value the object to be weakly referenced
   */
  def apply[T <: AnyRef](value: T): WeakReference[T] = new WeakReference(value)

  /** Optionally returns the referenced value, or `None` if that value no longer exists.
   *
   *  @tparam T the type of the value to extract from the weak reference
   *  @param wr the weak reference to extract a value from
   */
  def unapply[T <: AnyRef](wr: WeakReference[T]): Option[T] = Option(wr.underlying.get)
}

private class WeakReferenceWithWrapper[T <: AnyRef](value: T, queue: ReferenceQueue[T] | Null, val wrapper: WeakReference[T])
  extends java.lang.ref.WeakReference[T](value, if (queue == null) null else queue.underlying.asInstanceOf[java.lang.ref.ReferenceQueue[T]]) with ReferenceWithWrapper[T]
