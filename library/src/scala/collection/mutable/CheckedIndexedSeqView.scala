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

package scala
package collection
package mutable

import scala.language.`2.13`
import language.experimental.captureChecking

private[mutable] trait CheckedIndexedSeqView[+A] extends IndexedSeqView[A] {
  /** A function returning the current mutation count of the underlying collection.
   *
   *  Iterators over this view capture the count when they are created and compare
   *  it against the current count on every `hasNext` call; a difference means the
   *  underlying collection was mutated, and iteration fails with a
   *  `ConcurrentModificationException`.
   */
  protected val mutationCount: () => Int

  /** Returns a fail-fast iterator over the elements of this view: it records the
   *  current mutation count and throws a `ConcurrentModificationException` from
   *  `hasNext` if the underlying collection is mutated during iteration.
   */
  override def iterator: Iterator[A]^{this} = new CheckedIndexedSeqView.CheckedIterator(this, mutationCount())
  /** Returns a fail-fast iterator over the elements of this view in reverse order:
   *  it records the current mutation count and throws a
   *  `ConcurrentModificationException` from `hasNext` if the underlying collection
   *  is mutated during iteration.
   */
  override def reverseIterator: Iterator[A]^{this} = new CheckedIndexedSeqView.CheckedReverseIterator(this, mutationCount())

  /** Returns a view of the elements of this view followed by `elem`.
   *
   *  @tparam B the element type of the returned view, a supertype of `A`
   *  @param elem the element to append
   *  @return a view with `elem` appended, which retains this view's mutation checking
   */
  override def appended[B >: A](elem: B): IndexedSeqView[B]^{this} = new CheckedIndexedSeqView.Appended(this, elem)(mutationCount)
  /** Returns a view of `elem` followed by the elements of this view.
   *
   *  @tparam B the element type of the returned view, a supertype of `A`
   *  @param elem the element to prepend
   *  @return a view with `elem` prepended, which retains this view's mutation checking
   */
  override def prepended[B >: A](elem: B): IndexedSeqView[B]^{this} = new CheckedIndexedSeqView.Prepended(elem, this)(mutationCount)
  /** Returns a view of the first `n` elements of this view.
   *
   *  @param n the number of elements to take
   *  @return a view of the first `n` elements of this view, or of all elements if
   *          this view has fewer than `n`; it retains this view's mutation checking
   */
  override def take(n: Int): IndexedSeqView[A]^{this} = new CheckedIndexedSeqView.Take(this, n)(mutationCount)
  /** Returns a view of the last `n` elements of this view.
   *
   *  @param n the number of elements to take
   *  @return a view of the last `n` elements of this view, or of all elements if
   *          this view has fewer than `n`; it retains this view's mutation checking
   */
  override def takeRight(n: Int): IndexedSeqView[A]^{this} = new CheckedIndexedSeqView.TakeRight(this, n)(mutationCount)
  /** Returns a view of the elements of this view except the first `n`.
   *
   *  @param n the number of elements to drop
   *  @return a view of all elements of this view except the first `n`, empty if
   *          this view has fewer than `n`; it retains this view's mutation checking
   */
  override def drop(n: Int): IndexedSeqView[A]^{this} = new CheckedIndexedSeqView.Drop(this, n)(mutationCount)
  /** Returns a view of the elements of this view except the last `n`.
   *
   *  @param n the number of elements to drop
   *  @return a view of all elements of this view except the last `n`, empty if
   *          this view has fewer than `n`; it retains this view's mutation checking
   */
  override def dropRight(n: Int): IndexedSeqView[A]^{this} = new CheckedIndexedSeqView.DropRight(this, n)(mutationCount)
  /** Returns a view of the results of applying `f` to each element of this view.
   *
   *  `f` is applied lazily, each time an element of the returned view is accessed.
   *
   *  @tparam B the element type of the returned view
   *  @param f the function to apply to each element
   *  @return a view of the transformed elements, which retains this view's mutation checking
   */
  override def map[B](f: A => B): IndexedSeqView[B]^{this, f} = new CheckedIndexedSeqView.Map(this, f)(mutationCount)
  /** Returns a view of the elements of this view in reverse order, which retains
   *  this view's mutation checking.
   */
  override def reverse: IndexedSeqView[A]^{this} = new CheckedIndexedSeqView.Reverse(this)(mutationCount)
  /** Returns a view of the elements of this view between index `from` (inclusive)
   *  and index `until` (exclusive), with both bounds clamped to the valid range.
   *
   *  @param from the index of the first element of the slice
   *  @param until the index one past the last element of the slice
   *  @return a view of the selected elements, which retains this view's mutation checking
   */
  override def slice(from: Int, until: Int): IndexedSeqView[A]^{this} = new CheckedIndexedSeqView.Slice(this, from, until)(mutationCount)
  /** Returns a view over the same elements that applies the side-effecting
   *  function `f` to each element as it is accessed.
   *
   *  @tparam U the result type of `f`; the result is discarded
   *  @param f the function to apply to each element when it is accessed
   *  @return a view of the same elements, which retains this view's mutation checking
   */
  override def tapEach[U](f: A => U): IndexedSeqView[A]^{this, f} = new CheckedIndexedSeqView.Map(this, { (a: A) => f(a); a})(mutationCount)

  /** Returns a view of the elements of this view followed by the elements of `suffix`.
   *
   *  @tparam B the element type of the returned view, a supertype of `A`
   *  @param suffix the indexed sequence to append
   *  @return a view of the concatenation, which retains this view's mutation checking
   */
  override def concat[B >: A](suffix: IndexedSeqView.SomeIndexedSeqOps[B]^): IndexedSeqView[B]^{this, suffix} = new CheckedIndexedSeqView.Concat(this, suffix)(mutationCount)
  /** Returns a view of the elements of this view followed by the elements of
   *  `suffix`. This is the same as `concat`.
   *
   *  @tparam B the element type of the returned view, a supertype of `A`
   *  @param suffix the indexed sequence to append
   *  @return a view of the concatenation, which retains this view's mutation checking
   */
  override def appendedAll[B >: A](suffix: IndexedSeqView.SomeIndexedSeqOps[B]^): IndexedSeqView[B]^{this, suffix} = new CheckedIndexedSeqView.Concat(this, suffix)(mutationCount)
  /** Returns a view of the elements of `prefix` followed by the elements of this view.
   *
   *  @tparam B the element type of the returned view, a supertype of `A`
   *  @param prefix the indexed sequence to prepend
   *  @return a view of the concatenation, which retains this view's mutation checking
   */
  override def prependedAll[B >: A](prefix: IndexedSeqView.SomeIndexedSeqOps[B]^): IndexedSeqView[B]^{this, prefix} = new CheckedIndexedSeqView.Concat(prefix, this)(mutationCount)
}

private[mutable] object CheckedIndexedSeqView {
  import IndexedSeqView.SomeIndexedSeqOps

  @SerialVersionUID(3L)
  private[mutable] class CheckedIterator[A](self: IndexedSeqView[A]^, mutationCount: => Int)
    extends IndexedSeqView.IndexedSeqViewIterator[A](self) {
    private val expectedCount = mutationCount
    /** Returns `true` if there are more elements, after first checking that the
     *  underlying collection has not been mutated since this iterator was created.
     *
     *  @throws java.util.ConcurrentModificationException if the underlying collection
     *          was mutated after this iterator was created
     */
    override def hasNext: Boolean = {
      MutationTracker.checkMutationsForIteration(expectedCount, mutationCount)
      super.hasNext
    }
  }

  @SerialVersionUID(3L)
  private[mutable] class CheckedReverseIterator[A](self: IndexedSeqView[A]^, mutationCount: => Int)
    extends IndexedSeqView.IndexedSeqViewReverseIterator[A](self) {
    private val expectedCount = mutationCount
    /** Returns `true` if there are more elements, after first checking that the
     *  underlying collection has not been mutated since this iterator was created.
     *
     *  @throws java.util.ConcurrentModificationException if the underlying collection
     *          was mutated after this iterator was created
     */
    override def hasNext: Boolean = {
      MutationTracker.checkMutationsForIteration(expectedCount, mutationCount)
      super.hasNext
    }
  }

  /** An identity view of `underlying` whose iterators check for mutation of the
   *  underlying collection.
   *
   *  @tparam A the element type of the view
   *  @param underlying the indexed sequence being viewed
   *  @param mutationCount a function returning the underlying collection's current mutation count
   */
  @SerialVersionUID(3L)
  class Id[+A](underlying: SomeIndexedSeqOps[A]^)(protected val mutationCount: () => Int)
    extends IndexedSeqView.Id(underlying) with CheckedIndexedSeqView[A]

  /** A view of the elements of `underlying` followed by `elem`, whose iterators
   *  check for mutation of the underlying collection.
   *
   *  @tparam A the element type of the view
   *  @param underlying the indexed sequence being viewed
   *  @param elem the appended element
   *  @param mutationCount a function returning the underlying collection's current mutation count
   */
  @SerialVersionUID(3L)
  class Appended[+A](underlying: SomeIndexedSeqOps[A]^, elem: A)(protected val mutationCount: () => Int)
    extends IndexedSeqView.Appended(underlying, elem) with CheckedIndexedSeqView[A]

  /** A view of `elem` followed by the elements of `underlying`, whose iterators
   *  check for mutation of the underlying collection.
   *
   *  @tparam A the element type of the view
   *  @param elem the prepended element
   *  @param underlying the indexed sequence being viewed
   *  @param mutationCount a function returning the underlying collection's current mutation count
   */
  @SerialVersionUID(3L)
  class Prepended[+A](elem: A, underlying: SomeIndexedSeqOps[A]^)(protected val mutationCount: () => Int)
    extends IndexedSeqView.Prepended(elem, underlying) with CheckedIndexedSeqView[A]

  /** A view of the elements of `prefix` followed by the elements of `suffix`,
   *  whose iterators check for mutation of the underlying collection.
   *
   *  @tparam A the element type of the view
   *  @param prefix the indexed sequence whose elements come first
   *  @param suffix the indexed sequence whose elements come last
   *  @param mutationCount a function returning the underlying collection's current mutation count
   */
  @SerialVersionUID(3L)
  class Concat[A](prefix: SomeIndexedSeqOps[A]^, suffix: SomeIndexedSeqOps[A]^)(protected val mutationCount: () => Int)
    extends IndexedSeqView.Concat[A](prefix, suffix) with CheckedIndexedSeqView[A]

  /** A view of the first `n` elements of `underlying`, whose iterators check for
   *  mutation of the underlying collection.
   *
   *  @tparam A the element type of the view
   *  @param underlying the indexed sequence being viewed
   *  @param n the number of elements to take
   *  @param mutationCount a function returning the underlying collection's current mutation count
   */
  @SerialVersionUID(3L)
  class Take[A](underlying: SomeIndexedSeqOps[A]^, n: Int)(protected val mutationCount: () => Int)
    extends IndexedSeqView.Take(underlying, n) with CheckedIndexedSeqView[A]

  /** A view of the last `n` elements of `underlying`, whose iterators check for
   *  mutation of the underlying collection.
   *
   *  @tparam A the element type of the view
   *  @param underlying the indexed sequence being viewed
   *  @param n the number of elements to take
   *  @param mutationCount a function returning the underlying collection's current mutation count
   */
  @SerialVersionUID(3L)
  class TakeRight[A](underlying: SomeIndexedSeqOps[A]^, n: Int)(protected val mutationCount: () => Int)
    extends IndexedSeqView.TakeRight(underlying, n) with CheckedIndexedSeqView[A]

  /** A view of the elements of `underlying` except the first `n`, whose iterators
   *  check for mutation of the underlying collection.
   *
   *  @tparam A the element type of the view
   *  @param underlying the indexed sequence being viewed
   *  @param n the number of elements to drop
   *  @param mutationCount a function returning the underlying collection's current mutation count
   */
  @SerialVersionUID(3L)
  class Drop[A](underlying: SomeIndexedSeqOps[A]^, n: Int)(protected val mutationCount: () => Int)
    extends IndexedSeqView.Drop[A](underlying, n) with CheckedIndexedSeqView[A]

  /** A view of the elements of `underlying` except the last `n`, whose iterators
   *  check for mutation of the underlying collection.
   *
   *  @tparam A the element type of the view
   *  @param underlying the indexed sequence being viewed
   *  @param n the number of elements to drop
   *  @param mutationCount a function returning the underlying collection's current mutation count
   */
  @SerialVersionUID(3L)
  class DropRight[A](underlying: SomeIndexedSeqOps[A]^, n: Int)(protected val mutationCount: () => Int)
    extends IndexedSeqView.DropRight[A](underlying, n) with CheckedIndexedSeqView[A]

  /** A view of the results of applying `f` to each element of `underlying`, whose
   *  iterators check for mutation of the underlying collection. `f` is applied
   *  each time an element is accessed.
   *
   *  @tparam A the element type of the underlying sequence
   *  @tparam B the element type of the view
   *  @param underlying the indexed sequence being viewed
   *  @param f the function applied to each element that is accessed
   *  @param mutationCount a function returning the underlying collection's current mutation count
   */
  @SerialVersionUID(3L)
  class Map[A, B](underlying: SomeIndexedSeqOps[A]^, f: A => B)(protected val mutationCount: () => Int)
    extends IndexedSeqView.Map(underlying, f) with CheckedIndexedSeqView[B]

  /** A view of the elements of `underlying` in reverse order, whose iterators
   *  check for mutation of the underlying collection.
   *
   *  @tparam A the element type of the view
   *  @param underlying the indexed sequence being viewed
   *  @param mutationCount a function returning the underlying collection's current mutation count
   */
  @SerialVersionUID(3L)
  class Reverse[A](underlying: SomeIndexedSeqOps[A]^)(protected val mutationCount: () => Int)
    extends IndexedSeqView.Reverse[A](underlying) with CheckedIndexedSeqView[A] {
    /** Returns the underlying sequence itself when it is an `IndexedSeqView`,
     *  since reversing a reversed view yields the original view; otherwise
     *  returns a new checked view of this view in reverse order.
     */
    override def reverse: IndexedSeqView[A]^{this} = underlying match {
      case x: IndexedSeqView[A @unchecked] => x
      case _ => super.reverse
    }
  }

  /** A view of the elements of `underlying` between index `from` (inclusive) and
   *  index `until` (exclusive), with both bounds clamped to the valid range, whose
   *  iterators check for mutation of the underlying collection.
   *
   *  @tparam A the element type of the view
   *  @param underlying the indexed sequence being viewed
   *  @param from the index of the first element of the slice
   *  @param until the index one past the last element of the slice
   *  @param mutationCount a function returning the underlying collection's current mutation count
   */
  @SerialVersionUID(3L)
  class Slice[A](underlying: SomeIndexedSeqOps[A]^, from: Int, until: Int)(protected val mutationCount: () => Int)
    extends AbstractIndexedSeqView[A] with CheckedIndexedSeqView[A] {
    /** The inclusive lower bound of the slice within `underlying`: `from`, or 0 if `from` is negative. */
    protected val lo = from max 0
    /** The exclusive upper bound of the slice within `underlying`: `until` clamped
     *  to be between 0 and `underlying.length`.
     */
    protected val hi = (until max 0) min underlying.length
    /** The number of elements in the slice: `hi - lo`, or 0 if `lo` exceeds `hi`. */
    protected val len = (hi - lo) max 0
    /** Returns the element at index `i` of this slice, read from the underlying
     *  sequence at index `lo + i`.
     *
     *  @param i the index within the slice
     *  @return the element of the underlying sequence at index `lo + i`
     *  @throws IndexOutOfBoundsException if `lo + i` is out of bounds of the underlying sequence
     */
    @throws[IndexOutOfBoundsException]
    def apply(i: Int): A = underlying(lo + i)
    /** Returns the number of elements in this slice. */
    def length: Int = len
  }
}
