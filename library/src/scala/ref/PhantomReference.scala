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

/** A wrapper class for `java.lang.ref.PhantomReference`.
 *
 *  @tparam T the covariant type of the phantom-referenced object, must be a subtype of `AnyRef`
 *  @param value the object to be phantom referenced
 *  @param queue the reference queue to which this reference will be enqueued when the referent becomes phantom reachable
 */
class PhantomReference[+T <: AnyRef](value: T, queue: ReferenceQueue[T]) extends ReferenceWrapper[T] {
  val underlying: java.lang.ref.PhantomReference[? <: T] =
    new PhantomReferenceWithWrapper[T](value, queue, this)
}

private class PhantomReferenceWithWrapper[T <: AnyRef](value: T, queue: ReferenceQueue[T], val wrapper: PhantomReference[T])
  extends java.lang.ref.PhantomReference[T](value, queue.underlying.asInstanceOf[java.lang.ref.ReferenceQueue[T]]) with ReferenceWithWrapper[T]
