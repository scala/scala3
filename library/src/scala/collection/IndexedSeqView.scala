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

import scala.language.`2.13`
import language.experimental.captureChecking

import scala.annotation.nowarn


/** View defined in terms of indexing a range.
 *
 *  @tparam A the element type of the view
 */
trait IndexedSeqView[+A] extends IndexedSeqOps[A, View, View[A]] with SeqView[A] {

  /** Returns this view itself, since it is already a view. */
  override def view: IndexedSeqView[A]^{this} = this

  /** Returns a view of the elements of this view between index `from`
   *  (inclusive) and index `until` (exclusive), like `slice`.
   *
   *  @param from the index of the first element of the returned view
   *  @param until the index one past the last element of the returned view
   *  @return a view of the selected elements
   */
  @deprecated("Use .view.slice(from, until) instead of .view(from, until)", "2.13.0")
  override def view(from: Int, until: Int): IndexedSeqView[A]^{this} = view.slice(from, until)

  /** Returns an iterator over the elements of this view, produced by indexing
   *  into it.
   *
   *  The iterator's `drop` and `slice` operations adjust indices in constant
   *  time, without accessing the skipped elements.
   */
  override def iterator: Iterator[A]^{this} = new IndexedSeqView.IndexedSeqViewIterator(this)
  /** Returns an iterator over the elements of this view in reverse order,
   *  from the last element to the first, produced by indexing into this view.
   */
  override def reverseIterator: Iterator[A]^{this} = new IndexedSeqView.IndexedSeqViewReverseIterator(this)

  /** Returns a view of the elements of this view followed by `elem`.
   *
   *  @tparam B the element type of the returned view, a supertype of `A`
   *  @param elem the element to append
   */
  override def appended[B >: A](elem: B): IndexedSeqView[B]^{this} = new IndexedSeqView.Appended(this, elem)
  /** Returns a view of `elem` followed by the elements of this view.
   *
   *  @tparam B the element type of the returned view, a supertype of `A`
   *  @param elem the element to prepend
   */
  override def prepended[B >: A](elem: B): IndexedSeqView[B]^{this} = new IndexedSeqView.Prepended(elem, this)
  /** Returns a view of the first `n` elements of this view.
   *
   *  @param n the number of elements to take
   *  @return a view of the first `n` elements of this view, or of all elements
   *          if this view has fewer than `n`
   */
  override def take(n: Int): IndexedSeqView[A]^{this} = new IndexedSeqView.Take(this, n)
  /** Returns a view of the last `n` elements of this view.
   *
   *  @param n the number of elements to take
   *  @return a view of the last `n` elements of this view, or of all elements
   *          if this view has fewer than `n`
   */
  override def takeRight(n: Int): IndexedSeqView[A]^{this} = new IndexedSeqView.TakeRight(this, n)
  /** Returns a view of the elements of this view except the first `n`.
   *
   *  @param n the number of elements to drop
   *  @return a view of all elements of this view except the first `n`, empty
   *          if this view has fewer than `n`
   */
  override def drop(n: Int): IndexedSeqView[A]^{this} = new IndexedSeqView.Drop(this, n)
  /** Returns a view of the elements of this view except the last `n`.
   *
   *  @param n the number of elements to drop
   *  @return a view of all elements of this view except the last `n`, empty
   *          if this view has fewer than `n`
   */
  override def dropRight(n: Int): IndexedSeqView[A]^{this} = new IndexedSeqView.DropRight(this, n)
  /** Returns a view of the results of applying `f` to each element of this view.
   *
   *  `f` is applied lazily, each time an element of the returned view is
   *  accessed.
   *
   *  @tparam B the element type of the returned view
   *  @param f the function to apply to each element
   */
  override def map[B](f: A => B): IndexedSeqView[B]^{this, f} = new IndexedSeqView.Map(this, f)
  /** Returns a view of the elements of this view in reverse order. */
  override def reverse: IndexedSeqView[A]^{this} = new IndexedSeqView.Reverse(this)
  /** Returns a view of the elements of this view between index `from`
   *  (inclusive) and index `until` (exclusive), with both bounds clamped to
   *  the valid range.
   *
   *  @param from the index of the first element of the slice
   *  @param until the index one past the last element of the slice
   */
  override def slice(from: Int, until: Int): IndexedSeqView[A]^{this} = new IndexedSeqView.Slice(this, from, until)
  /** Returns a view over the same elements that applies the side-effecting
   *  function `f` to each element as it is accessed.
   *
   *  @tparam U the result type of `f`; the result is discarded
   *  @param f the function to apply to each element when it is accessed
   */
  override def tapEach[U](f: A => U): IndexedSeqView[A]^{this, f} = new IndexedSeqView.Map(this, { (a: A) => f(a); a})

  /** Returns a view of the elements of this view followed by the elements of
   *  `suffix`.
   *
   *  @tparam B the element type of the returned view, a supertype of `A`
   *  @param suffix the indexed sequence to append
   */
  def concat[B >: A](suffix: IndexedSeqView.SomeIndexedSeqOps[B]^): IndexedSeqView[B]^{this, suffix} = new IndexedSeqView.Concat(this, suffix)
  /** Returns a view of the elements of this view followed by the elements of
   *  `suffix`. This is the same as `concat`.
   *
   *  @tparam B the element type of the returned view, a supertype of `A`
   *  @param suffix the indexed sequence to append
   */
  def appendedAll[B >: A](suffix: IndexedSeqView.SomeIndexedSeqOps[B]^): IndexedSeqView[B]^{this, suffix} = new IndexedSeqView.Concat(this, suffix)
  /** Returns a view of the elements of `prefix` followed by the elements of
   *  this view.
   *
   *  @tparam B the element type of the returned view, a supertype of `A`
   *  @param prefix the indexed sequence to prepend
   */
  def prependedAll[B >: A](prefix: IndexedSeqView.SomeIndexedSeqOps[B]^): IndexedSeqView[B]^{this, prefix} = new IndexedSeqView.Concat(prefix, this)

  /** The prefix of this view's string representation, `"IndexedSeqView"`. */
  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected def stringPrefix: String = "IndexedSeqView"
}

object IndexedSeqView {

  @SerialVersionUID(3L)
  private[collection] class IndexedSeqViewIterator[A](self: IndexedSeqView[A]^) extends AbstractIterator[A] with Serializable {
    private var current = 0
    private var remainder = self.length
    /** Returns the number of elements remaining in this iterator. */
    override def knownSize: Int = remainder
    @inline private def _hasNext: Boolean = remainder > 0
    /** Returns `true` if this iterator has more elements. */
    def hasNext: Boolean = _hasNext
    /** Returns the next element, read from the view at this iterator's
     *  current index, and advances the iterator.
     *
     *  @throws NoSuchElementException if no elements remain
     */
    def next(): A =
      if (_hasNext) {
        val r = self(current)
        current += 1
        remainder -= 1
        r
      } else Iterator.empty.next()

    /** Advances this iterator past the next `n` elements in constant time,
     *  without accessing them.
     *
     *  A non-positive `n` has no effect; at most the remaining elements are
     *  dropped.
     *
     *  @param n the number of elements to drop
     *  @return this iterator
     */
    override def drop(n: Int): Iterator[A]^{this} = {
      if (n > 0) {
        current += n
        remainder = Math.max(0, remainder - n)
      }
      this
    }

    /** Restricts this iterator, in place, to the elements between index `from`
     *  (inclusive) and index `until` (exclusive) of the remaining elements.
     *
     *  Both bounds are clamped to the range from 0 to the number of remaining
     *  elements, and the adjustment is made in constant time by index
     *  arithmetic, without accessing any elements.
     *
     *  @param from the index among the remaining elements of the first element to keep
     *  @param until the index among the remaining elements one past the last element to keep
     *  @return this iterator
     */
    override protected def sliceIterator(from: Int, until: Int): Iterator[A]^{this} = {

      def formatRange(value : Int) : Int = if (value < 0) 0 else if (value > remainder) remainder else value

      val formatFrom = formatRange(from)
      val formatUntil = formatRange(until)
      remainder = Math.max(0, formatUntil - formatFrom)
      current = current + formatFrom
      this
    }
  }
  @SerialVersionUID(3L)
  private[collection] class IndexedSeqViewReverseIterator[A](self: IndexedSeqView[A]^) extends AbstractIterator[A] with Serializable {
    private var remainder = self.length
    private var pos = remainder - 1
    @inline private def _hasNext: Boolean = remainder > 0
    /** Returns `true` if this iterator has more elements. */
    def hasNext: Boolean = _hasNext
    /** Returns the next element in reverse order, read from the view at this
     *  iterator's current position, and moves the position backward.
     *
     *  @throws NoSuchElementException if no elements remain
     */
    def next(): A =
      if (_hasNext) {
        val r = self(pos)
        pos -= 1
        remainder -= 1
        r
      } else Iterator.empty.next()

    // from < 0 means don't move pos, until < 0 means don't limit remainder
    //
    /** Restricts this iterator, in place, to the elements between index `from`
     *  (inclusive) and index `until` (exclusive) of the remaining elements.
     *
     *  Skips `from` elements when `from` is positive and limits the number of
     *  remaining elements to `until - from`; a negative `until` imposes no
     *  limit. The adjustment is made in constant time, without accessing any
     *  elements, and has no effect if this iterator is already exhausted.
     *
     *  @param from the index among the remaining elements of the first element
     *         to keep; if not positive, no elements are skipped
     *  @param until the index among the remaining elements one past the last
     *         element to keep; if negative, no limit is applied
     *  @return this iterator
     */
    override protected def sliceIterator(from: Int, until: Int): Iterator[A]^{this} = {
      if (_hasNext) {
        if (remainder <= from) remainder = 0                              // exhausted by big skip
        else if (from <= 0) {                                             // no skip, pos is same
          if (until >= 0 && until < remainder) remainder = until          // ...limited by until
        }
        else {
          pos -= from                                                     // skip ahead
          if (until >= 0 && until < remainder) {                          // ...limited by until
            if (until <= from) remainder = 0                              // ...exhausted if limit is smaller than skip
            else remainder = until - from                                 // ...limited by until, less the skip
          }
          else remainder -= from                                          // ...otherwise just less the skip
        }
      }
      this
    }
  }

  /** An `IndexedSeqOps` whose collection type and collection type constructor are unknown. */
  type SomeIndexedSeqOps[A] = IndexedSeqOps[A, AnyConstr, ?]

  /** An identity view of `underlying`, presenting its elements unchanged.
   *
   *  @tparam A the element type of the view
   *  @param underlying the indexed sequence being viewed
   */
  @SerialVersionUID(3L)
  class Id[+A](underlying: SomeIndexedSeqOps[A]^)
    extends SeqView.Id(underlying) with IndexedSeqView[A]

  /** A view of the elements of `underlying` followed by `elem`.
   *
   *  @tparam A the element type of the view
   *  @param underlying the indexed sequence being viewed
   *  @param elem the appended element
   */
  @SerialVersionUID(3L)
  class Appended[+A](underlying: SomeIndexedSeqOps[A]^, elem: A)
    extends SeqView.Appended(underlying, elem) with IndexedSeqView[A]

  /** A view of `elem` followed by the elements of `underlying`.
   *
   *  @tparam A the element type of the view
   *  @param elem the prepended element
   *  @param underlying the indexed sequence being viewed
   */
  @SerialVersionUID(3L)
  class Prepended[+A](elem: A, underlying: SomeIndexedSeqOps[A]^)
    extends SeqView.Prepended(elem, underlying) with IndexedSeqView[A]

  /** A view of the elements of `prefix` followed by the elements of `suffix`.
   *
   *  @tparam A the element type of the view
   *  @param prefix the indexed sequence whose elements come first
   *  @param suffix the indexed sequence whose elements come last
   */
  @SerialVersionUID(3L)
  class Concat[A](prefix: SomeIndexedSeqOps[A]^, suffix: SomeIndexedSeqOps[A]^)
    extends SeqView.Concat[A](prefix, suffix) with IndexedSeqView[A]

  /** A view of the first `n` elements of `underlying`.
   *
   *  @tparam A the element type of the view
   *  @param underlying the indexed sequence being viewed
   *  @param n the number of elements to take
   */
  @SerialVersionUID(3L)
  class Take[A](underlying: SomeIndexedSeqOps[A]^, n: Int)
    extends SeqView.Take(underlying, n) with IndexedSeqView[A]

  /** A view of the last `n` elements of `underlying`.
   *
   *  @tparam A the element type of the view
   *  @param underlying the indexed sequence being viewed
   *  @param n the number of elements to take
   */
  @SerialVersionUID(3L)
  class TakeRight[A](underlying: SomeIndexedSeqOps[A]^, n: Int)
    extends SeqView.TakeRight(underlying, n) with IndexedSeqView[A]

  /** A view of the elements of `underlying` except the first `n`.
   *
   *  @tparam A the element type of the view
   *  @param underlying the indexed sequence being viewed
   *  @param n the number of elements to drop
   */
  @SerialVersionUID(3L)
  class Drop[A](underlying: SomeIndexedSeqOps[A]^, n: Int)
    extends SeqView.Drop[A](underlying, n) with IndexedSeqView[A]

  /** A view of the elements of `underlying` except the last `n`.
   *
   *  @tparam A the element type of the view
   *  @param underlying the indexed sequence being viewed
   *  @param n the number of elements to drop
   */
  @SerialVersionUID(3L)
  class DropRight[A](underlying: SomeIndexedSeqOps[A]^, n: Int)
    extends SeqView.DropRight[A](underlying, n) with IndexedSeqView[A]

  /** A view of the results of applying `f` to each element of `underlying`.
   *
   *  `f` is applied each time an element is accessed.
   *
   *  @tparam A the element type of the underlying sequence
   *  @tparam B the element type of the view
   *  @param underlying the indexed sequence being viewed
   *  @param f the function applied to each element that is accessed
   */
  @SerialVersionUID(3L)
  class Map[A, B](underlying: SomeIndexedSeqOps[A]^, f: A => B)
    extends SeqView.Map(underlying, f) with IndexedSeqView[B]

  /** A view of the elements of `underlying` in reverse order: index `i` of
   *  the view reads index `length - 1 - i` of the underlying sequence.
   *
   *  @tparam A the element type of the view
   *  @param underlying the indexed sequence being viewed
   */
  @SerialVersionUID(3L)
  class Reverse[A](underlying: SomeIndexedSeqOps[A]^) extends SeqView.Reverse[A](underlying) with IndexedSeqView[A] {
    /** Returns the underlying sequence itself when it is an `IndexedSeqView`,
     *  since reversing a reversed view yields the original view; otherwise
     *  returns a new view of this view in reverse order.
     */
    override def reverse: IndexedSeqView[A]^{this} = underlying match {
      case x: IndexedSeqView[A @unchecked] => x
      case _ => super.reverse
    }
  }

  /** A view of the elements of `underlying` between index `from` (inclusive)
   *  and index `until` (exclusive).
   *
   *  A negative `from` is treated as 0 and `until` is clamped to lie between 0
   *  and the length of `underlying`; the view is empty whenever the resulting
   *  lower bound is not below the resulting upper bound, so a `from` beyond the
   *  end of `underlying` yields an empty view rather than an error.
   *
   *  @tparam A the element type of the view
   *  @param underlying the indexed sequence being viewed
   *  @param from the index of the first element of the slice
   *  @param until the index one past the last element of the slice
   */
  @SerialVersionUID(3L)
  class Slice[A](underlying: SomeIndexedSeqOps[A]^, from: Int, until: Int) extends AbstractIndexedSeqView[A] {
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

/** Explicit instantiation of the `IndexedSeqView` trait to reduce class file size in subclasses. */
@SerialVersionUID(3L)
abstract class AbstractIndexedSeqView[+A] extends AbstractSeqView[A] with IndexedSeqView[A]
