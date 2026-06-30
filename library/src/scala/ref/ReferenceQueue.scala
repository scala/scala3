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

/** A queue onto which registered references are appended by the garbage collector
 *  once it determines that their referents have become unreachable.
 *
 *  @tparam T the type of the referents of the references enqueued on this queue
 */
class ReferenceQueue[+T <: AnyRef] {

  private[ref] val underlying: java.lang.ref.ReferenceQueue[? <: T] = new java.lang.ref.ReferenceQueue[T]
  /** Returns the string representation of the underlying Java reference queue. */
  override def toString(): String = underlying.toString

  /** Extracts the Scala `Reference` wrapper from a reference dequeued from the underlying queue.
   *
   *  @param jref a reference dequeued from the underlying queue, or `null` if none was available
   *  @return `Some` wrapper associated with `jref`, or `None` if `jref` is `null`
   */
  protected def Wrapper(jref: java.lang.ref.Reference[?] | Null): Option[Reference[T]] =
    jref match {
      case null => None
      case ref => Some(ref.asInstanceOf[ReferenceWithWrapper[T]].wrapper)
    }

  /** Returns and removes the next available reference from this queue without blocking, or `None` if the queue is empty. */
  def poll: Option[Reference[T]] = Wrapper(underlying.poll)
  /** Returns and removes the next reference from this queue, blocking until one becomes available. */
  def remove: Option[Reference[T]] = Wrapper(underlying.remove)
  /** Removes and returns the next reference from this queue, blocking until one becomes available or the given timeout elapses.
   *
   *  @param timeout the maximum number of milliseconds to wait, where `0` means wait indefinitely and a negative value throws `IllegalArgumentException`
   *  @return `Some` reference if one became available within the timeout, or `None` if the timeout elapsed first
   */
  def remove(timeout: Long): Option[Reference[T]] = Wrapper(underlying.remove(timeout))

}
