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

package scala.collection
package mutable

import scala.language.`2.13`
import language.experimental.captureChecking
import scala.annotation.nowarn
import scala.collection.generic.DefaultSerializable


/** `Queue` objects implement data structures that allow to
 *  insert and retrieve elements in a first-in-first-out (FIFO) manner.
 *
 *  @define Coll `mutable.Queue`
 *  @define coll mutable queue
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 *
 *  @tparam A the element type stored in this queue
 */
class Queue[A] protected (array: Array[AnyRef | Null], start: Int, end: Int)
  extends ArrayDeque[A](array, start, end)
    with IndexedSeqOps[A, Queue, Queue[A]]
    with StrictOptimizedSeqOps[A, Queue, Queue[A]]
    with IterableFactoryDefaults[A, Queue]
    with ArrayDequeOps[A, Queue, Queue[A]]
    with Cloneable[Queue[A]]
    with DefaultSerializable {

  /** Creates an empty queue whose backing array can hold at least `initialSize` elements.
   *
   *  @param initialSize the initial capacity hint, 16 by default
   */
  def this(initialSize: Int = ArrayDeque.DefaultInitialSize) =
    this(ArrayDeque.alloc(initialSize), start = 0, end = 0)

  /** The factory used to build queues, the [[Queue$ `Queue`]] companion object. */
  override def iterableFactory: SeqFactory[Queue] = Queue

  /** The prefix used in the string representation of this queue, `"Queue"`. */
  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected def stringPrefix = "Queue"

  /** Adds elements to the end of this queue
   *
   *  @param elem the element to enqueue
   *  @return the queue with the element enqueued
   */
  def enqueue(elem: A): this.type = this += elem

  /** Enqueue two or more elements at the end of the queue. The last element
   *  of the sequence will be on end of the queue.
   *
   *  @param elem1 the first element to enqueue
   *  @param elem2 the second element to enqueue
   *  @param elems the remaining elements to enqueue
   *  @return the queue with the elements enqueued
   */
  def enqueue(elem1: A, elem2: A, elems: A*): this.type = enqueue(elem1).enqueue(elem2).enqueueAll(elems)

  /** Enqueues all elements in the given iterable object into the queue. The
   *  last element in the iterable object will be at the end of the queue.
   *
   *  @param elems the iterable object.
   *  @return the queue with the elements enqueued
   */
  def enqueueAll(elems: scala.collection.IterableOnce[A]^): this.type = this ++= elems

  /** Removes the first element from this queue and returns it.
   *
   *  @return the first element of the queue
   *  @throws NoSuchElementException when queue is empty
   */
  def dequeue(): A = removeHead()

  /** Returns the first element in the queue which satisfies the
   *  given predicate, and removes this element from the queue.
   *
   *  @param p   the predicate used for choosing the first element
   *  @return the first element of the queue for which p yields true
   */
  def dequeueFirst(p: A => Boolean): Option[A] =
    removeFirst(p)

  /** Returns all elements in the queue which satisfy the
   *  given predicate, and removes those elements from the queue.
   *
   *  @param p   the predicate used for choosing elements
   *  @return    a sequence of all elements in the queue for which
   *             p yields true.
   */
  def dequeueAll(p: A => Boolean): scala.collection.immutable.Seq[A] =
    removeAll(p)

  /** Returns and dequeues all elements from the queue which satisfy the given predicate.
   *
   *  @param f   the predicate that must hold true for elements to be dequeued from the front
   *  @return the removed elements, in order from front of the queue
   */
  def dequeueWhile(f: A => Boolean): scala.collection.Seq[A] = removeHeadWhile(f)

  /** Returns the first element in the queue, or throws an error if there
   *  is no element contained in the queue.
   *
   *  @return the first element.
   */
  @`inline` final def front: A = head

  /** Returns a copy of this queue, containing the same elements in the same
   *  order. Called by `clone()`.
   */
  override protected def klone(): Queue[A] = {
    val bf = newSpecificBuilder
    bf ++= this
    bf.result()
  }

  /** Wraps an array in a new queue, without copying.
   *
   *  @param array the backing array, allocated by `ArrayDeque.alloc` so that
   *               its length is a power of 2
   *  @param end the number of elements: `array` holds them at indices `0` until `end`
   *  @return a new queue backed by `array`
   */
  override protected def ofArray(array: Array[AnyRef | Null], end: Int): Queue[A] =
    new Queue(array, start = 0, end)

}

/** $factoryInfo
 *  @define coll queue
 *  @define Coll `Queue`
 */
@SerialVersionUID(3L)
object Queue extends StrictOptimizedSeqFactory[Queue] {

  /** Creates a new queue containing the elements of the given collection, in
   *  its iteration order.
   *
   *  The first element of `source` is at the front of the new queue.
   *
   *  @tparam A the element type of the queue
   *  @param source the collection of elements
   *  @return a new queue containing the elements of `source`
   */
  def from[A](source: IterableOnce[A]^): Queue[A] = empty ++= source

  /** Creates a new empty queue.
   *
   *  @tparam A the element type of the queue
   *  @return a new empty `Queue[A]`
   */
  def empty[A]: Queue[A] = new Queue

  /** Returns a new builder that accumulates elements into a queue.
   *
   *  The first element added to the builder is at the front of the resulting queue.
   *
   *  @tparam A the element type of the queue
   *  @return a builder for a `Queue[A]`
   */
  def newBuilder[A]: Builder[A, Queue[A]] = new GrowableBuilder[A, Queue[A]](empty)

}
