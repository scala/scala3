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
package immutable

import scala.language.`2.13`
import language.experimental.captureChecking

import scala.collection.generic.DefaultSerializable
import scala.collection.mutable.{Builder, ListBuffer}

/** `Queue` objects implement data structures that allow to
 *  insert and retrieve elements in a first-in-first-out (FIFO) manner.
 *
 *  `Queue` is implemented as a pair of `List`s, one containing the *in* elements and the other the *out* elements.
 *  Elements are added to the *in* list and removed from the *out* list. When the *out* list runs dry, the
 *  queue is pivoted by replacing the *out* list by *in.reverse*, and *in* by *Nil*.
 *
 *  Adding items to the queue always has cost `O(1)`. Removing items has cost `O(1)`, except in the case
 *  where a pivot is required, in which case, a cost of `O(n)` is incurred, where `n` is the number of elements in the queue. When this happens,
 *  `n` remove operations with `O(1)` cost are guaranteed. Removing an item is on average `O(1)`.
 *
 *  @see ["Scala's Collection Library overview"](https://docs.scala-lang.org/overviews/collections-2.13/concrete-immutable-collection-classes.html#immutable-queues)
 *  section on `Immutable Queues` for more information.
 *
 *  @define Coll `immutable.Queue`
 *  @define coll immutable queue
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 *
 *  @tparam A the type of elements contained in this queue
 */

sealed class Queue[+A] protected(protected val in: List[A], protected val out: List[A])
  extends AbstractSeq[A]
    with LinearSeq[A]
    with LinearSeqOps[A, Queue, Queue[A]]
    with StrictOptimizedLinearSeqOps[A, Queue, Queue[A]]
    with StrictOptimizedSeqOps[A, Queue, Queue[A]]
    with IterableFactoryDefaults[A, Queue]
    with DefaultSerializable {

  /** The factory used to build queues, the [[Queue$ `Queue`]] companion object. */
  override def iterableFactory: SeqFactory[Queue] = Queue

  /** Returns the `n`-th element of this queue.
   *  The first element is at position `0`.
   *
   *  @param  n index of the element to return
   *  @return   the element at position `n` in this queue.
   *  @throws NoSuchElementException if the queue is too short.
   */
  override def apply(n: Int): A = {
    def indexOutOfRange(): Nothing = throw new IndexOutOfBoundsException(n.toString)

    var index = 0
    var curr = out

    while (index < n && curr.nonEmpty) {
      index += 1
      curr = curr.tail
    }

    if (index == n) {
      if (curr.nonEmpty) curr.head
      else if (in.nonEmpty) in.last
      else indexOutOfRange()
    } else {
      val indexFromBack = n - index
      val inLength = in.length
      if (indexFromBack >= inLength) indexOutOfRange()
      else in(inLength - indexFromBack - 1)
    }
  }

  /** Returns the elements in the list as an iterator */
  override def iterator: Iterator[A] = out.iterator.concat(in.reverse)

  /** Checks if the queue is empty.
   *
   *  @return true, iff there is no element in the queue.
   */
  override def isEmpty: Boolean = in.isEmpty && out.isEmpty

  /** Returns the first element of this queue, the one that `dequeue` would remove.
   *
   *  This costs `O(1)` unless the *out* list has run dry, in which case the last
   *  element of the *in* list is found in `O(n)`; no pivot is performed.
   *
   *  @throws NoSuchElementException if this queue is empty
   */
  override def head: A =
    if (out.nonEmpty) out.head
    else if (in.nonEmpty) in.last
    else throw new NoSuchElementException("head on empty queue")

  /** Returns a queue holding all elements of this queue except the first.
   *
   *  This costs `O(1)` unless the *out* list has run dry, in which case the queue is
   *  pivoted at a cost of `O(n)`.
   *
   *  @throws NoSuchElementException if this queue is empty
   */
  override def tail: Queue[A] =
    if (out.nonEmpty) new Queue(in, out.tail)
    else if (in.nonEmpty) new Queue(Nil, in.reverse.tail)
    else throw new NoSuchElementException("tail on empty queue")

  /** Returns the last element of this queue, the one enqueued most recently.
   *
   *  This costs `O(1)` unless nothing has been enqueued since the last pivot, in
   *  which case the last element of the *out* list is found in `O(n)`.
   *
   *  @throws NoSuchElementException if this queue is empty
   */
  override def last: A =
    if (in.nonEmpty) in.head
    else if (out.nonEmpty) out.last
    else throw new NoSuchElementException("last on empty queue")

  /* This is made to avoid inefficient implementation of iterator. */
  /** Returns `true` if `p` holds for every element of this queue.
   *
   *  Tests the two underlying lists directly instead of going through the iterator,
   *  which would have to reverse the *in* list.
   *
   *  @param p the predicate used to test elements
   *  @return `true` if this queue is empty or `p` holds for all of its elements
   */
  override def forall(p: A => Boolean): Boolean =
    in.forall(p) && out.forall(p)

  /* This is made to avoid inefficient implementation of iterator. */
  /** Returns `true` if `p` holds for at least one element of this queue.
   *
   *  Tests the two underlying lists directly instead of going through the iterator,
   *  which would have to reverse the *in* list; the elements are therefore not
   *  necessarily tested in queue order.
   *
   *  @param p the predicate used to test elements
   *  @return `true` if `p` holds for some element of this queue, `false` if this
   *          queue is empty
   */
  override def exists(p: A => Boolean): Boolean =
    in.exists(p) || out.exists(p)

  /** The prefix of this queue's string representation: `"Queue"`. */
  override protected def className = "Queue"

  /** Returns the length of the queue. */
  override def length: Int = in.length + out.length

  /** Returns a queue with `elem` at its front, so that `elem` is the next element to
   *  be dequeued, followed by all elements of this queue.
   *
   *  This costs `O(1)`.
   *
   *  @tparam B the element type of the returned queue, a supertype of `A`
   *  @param elem the element to place at the front
   *  @return a new queue consisting of `elem` followed by the elements of this queue
   */
  override def prepended[B >: A](elem: B): Queue[B] = new Queue(in, elem :: out)

  /** Returns a queue with `elem` at its end, the same queue that `enqueue` returns.
   *
   *  This costs `O(1)`.
   *
   *  @tparam B the element type of the returned queue, a supertype of `A`
   *  @param elem the element to place at the end
   *  @return a new queue consisting of the elements of this queue followed by `elem`
   */
  override def appended[B >: A](elem: B): Queue[B] = enqueue(elem)

  /** Returns a queue consisting of the elements of this queue followed by the
   *  elements of `that`, in the order `that` gives them out.
   *
   *  The elements of `that` are pushed onto the *in* list, which costs `O(m)` in the
   *  size of `that` and leaves this queue's *out* list untouched. If `that` is empty
   *  this queue is returned as is.
   *
   *  @tparam B the element type of the returned queue, a supertype of `A`
   *  @param that the collection of elements to append
   *  @return a new queue with the elements of `that` appended, or this queue itself
   *          if `that` is empty
   */
  override def appendedAll[B >: A](that: scala.collection.IterableOnce[B]^): Queue[B] = {
    val newIn = that match {
      case that: Queue[B] => that.in ++ (that.out reverse_::: this.in)
      case that: List[B] => that reverse_::: this.in
      case _ =>
        var result: List[B] = this.in
        val iter = that.iterator
        while (iter.hasNext) {
          result = iter.next() :: result
        }
        result
    }
    if (newIn eq this.in) this else new Queue[B](newIn, this.out)
  }

  /** Creates a new queue with element added at the end
   *  of the old queue.
   *
   *  @tparam B the element type of the returned queue, a supertype of `A`
   *  @param  elem        the element to insert
   *  @return a new queue with `elem` appended at the end
   */
  def enqueue[B >: A](elem: B): Queue[B] = new Queue(elem :: in, out)

  /** Creates a new queue with all elements provided by an `Iterable` object
   *  added at the end of the old queue.
   *
   *  The elements are appended in the order they are given out by the
   *  iterator.
   *
   *  @param  iter        an iterable object
   */
  @deprecated("Use `enqueueAll` instead of `enqueue` to enqueue a collection of elements", "2.13.0")
  @`inline` final def enqueue[B >: A](iter: scala.collection.Iterable[B]^) = enqueueAll(iter)

  /** Creates a new queue with all elements provided by an `Iterable` object
   *  added at the end of the old queue.
   *
   *  The elements are appended in the order they are given out by the
   *  iterator.
   *
   *  @tparam B the element type of the returned queue, a supertype of `A`
   *  @param  iter        an iterable object
   *  @return a new queue with all elements of `iter` appended at the end
   */
  def enqueueAll[B >: A](iter: scala.collection.Iterable[B]^): Queue[B] = appendedAll(iter)

  /** Returns a tuple with the first element in the queue,
   *  and a new queue with this element removed.
   *
   *  @return the first element of the queue.
   *  @throws NoSuchElementException if the queue is empty
   */
  def dequeue: (A, Queue[A]) = out match {
    case Nil if !in.isEmpty => val rev = in.reverse ; (rev.head, new Queue(Nil, rev.tail))
    case x :: xs            => (x, new Queue(in, xs))
    case _                  => throw new NoSuchElementException("dequeue on empty queue")
  }

  /** Optionally retrieves the first element and a queue of the remaining elements.
   *
   *  @return A tuple of the first element of the queue, and a new queue with this element removed.
   *         If the queue is empty, `None` is returned.
   */
  def dequeueOption: Option[(A, Queue[A])] = if(isEmpty) None else Some(dequeue)

  /** Returns the first element in the queue, or throws an error if there
   *  is no element contained in the queue.
   *
   *  @return the first element.
   *  @throws NoSuchElementException if the queue is empty
   */
  def front: A = head

  /** Returns a string representation of this queue. */
  override def toString(): String = mkString("Queue(", ", ", ")")
}

/** $factoryInfo
 *  @define Coll `immutable.Queue`
 *  @define coll immutable queue
 */
@SerialVersionUID(3L)
object Queue extends StrictOptimizedSeqFactory[Queue] {
  /** Returns a new builder that collects elements into a queue, in the order they
   *  are added.
   *
   *  @tparam A the element type of the queue being built
   *  @return a builder backed by a `ListBuffer` whose result is a queue ready to be
   *          dequeued without a pivot
   */
  def newBuilder[A]: Builder[A, Queue[A]] = new ListBuffer[A] mapResult (x => new Queue[A](Nil, x))

  /** Returns a queue containing the elements of `source`, in the order `source`
   *  gives them out, so that its first element is the first to be dequeued.
   *
   *  If `source` is already a queue it is returned unchanged.
   *
   *  @tparam A the element type
   *  @param source the collection whose elements are to be contained
   */
  def from[A](source: IterableOnce[A]^): Queue[A] = source match {
    case q: Queue[A] => q
    case _ =>
      val list = List.from(source)
      if (list.isEmpty) empty
      else new Queue(Nil, list)
  }

  /** Returns the empty queue.
   *
   *  All calls return the same instance, which is shared across element types.
   *
   *  @tparam A the element type of the queue
   */
  def empty[A]: Queue[A] = EmptyQueue
  /** Returns a queue containing the given elements, the first of which is the first
   *  to be dequeued.
   *
   *  @tparam A the element type of the queue
   *  @param xs the elements of the new queue, in dequeuing order
   */
  override def apply[A](xs: A*): Queue[A] = new Queue[A](Nil, xs.toList)

  private object EmptyQueue extends Queue[Nothing](Nil, Nil) { }
}
