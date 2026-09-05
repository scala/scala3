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
import scala.collection.generic.CommonErrors
import scala.runtime.ScalaRunTime.nullForGC


/** A view of a sequence: a [[scala.collection.View]] whose elements additionally can be
 *  accessed by index via `apply` and counted via `length`.
 *
 *  As with all views, transformation operations are non strict: they return other views,
 *  and elements are evaluated only when the view is traversed or converted to a strict
 *  collection. The operations that must count from the end - `takeRight`, `dropRight`
 *  and `sorted` - are an exception in one respect: each computes the length of this view
 *  as the new view is created, though it still leaves the elements themselves
 *  unevaluated.
 *
 *  @tparam A the element type of the view
 */
trait SeqView[+A] extends SeqOps[A, View, View[A]] with View[A] {
  /** Returns this view unchanged: a sequence view is already a view. */
  override def view: SeqView[A]^{this} = this

  /** Returns a view of the results of applying `f` to each element of this view.
   *
   *  `f` is applied lazily, each time an element is accessed or traversed, and is
   *  re-applied on every access.
   *
   *  @tparam B the element type of the returned view
   *  @param f the function to apply to each element
   *  @return a view producing `f(x)` for each element `x` of this view
   */
  override def map[B](f: A => B): SeqView[B]^{this, f} = new SeqView.Map(this, f)
  /** Returns a view of the elements of this view followed by `elem`.
   *
   *  @tparam B the element type of the returned view, a supertype of this view's element type
   *  @param elem the element to append
   *  @return a view whose last element is `elem`
   */
  override def appended[B >: A](elem: B): SeqView[B]^{this} = new SeqView.Appended(this, elem)
  /** Returns a view of `elem` followed by the elements of this view.
   *
   *  @tparam B the element type of the returned view, a supertype of this view's element type
   *  @param elem the element to prepend
   *  @return a view whose first element is `elem`
   */
  override def prepended[B >: A](elem: B): SeqView[B]^{this} = new SeqView.Prepended(elem, this)
  /** Returns a view of the elements of this view in reverse order. */
  override def reverse: SeqView[A]^{this} = new SeqView.Reverse(this)
  /** Returns a view of the first `n` elements of this view, or of all elements if this
   *  view has fewer than `n`.
   *
   *  @param n the number of elements to take; a negative value is treated as 0
   *  @return a view of the `n` leading elements, of all of them if there are fewer
   *          than `n`, or an empty view if `n` is not positive
   */
  override def take(n: Int): SeqView[A]^{this} = new SeqView.Take(this, n)
  /** Returns a view of the elements of this view except the first `n`.
   *
   *  @param n the number of elements to drop
   *  @return a view of the elements that remain after dropping `n` leading elements
   */
  override def drop(n: Int): SeqView[A]^{this} = new SeqView.Drop(this, n)
  /** Returns a view of the last `n` elements of this view, or of all elements if this
   *  view has fewer than `n`.
   *
   *  Creating the returned view computes the size of this view immediately.
   *
   *  @param n the number of elements to take; a negative value is treated as 0
   *  @return a view of the `n` trailing elements, of all of them if there are fewer
   *          than `n`, or an empty view if `n` is not positive
   */
  override def takeRight(n: Int): SeqView[A]^{this} = new SeqView.TakeRight(this, n)
  /** Returns a view of the elements of this view except the last `n`.
   *
   *  Creating the returned view computes the size of this view immediately.
   *
   *  @param n the number of elements to drop
   *  @return a view of the elements that remain after dropping `n` trailing elements
   */
  override def dropRight(n: Int): SeqView[A]^{this} = new SeqView.DropRight(this, n)
  /** Returns a view of the same elements that additionally applies `f` to each element,
   *  for its side effect, every time the element is accessed or traversed.
   *
   *  @tparam U the result type of `f`; the results are discarded
   *  @param f the side-effecting function to apply to each element
   *  @return a view of the elements of this view, invoking `f` on each element access
   */
  override def tapEach[U](f: A => U): SeqView[A]^{this, f} = new SeqView.Map(this, { (a: A) => f(a); a })

  /** Returns a view of the elements of this view followed by the elements of `suffix`.
   *
   *  @tparam B the element type of the returned view, a supertype of this view's element type
   *  @param suffix the sequence whose elements follow those of this view
   *  @return a view of the concatenation of this view and `suffix`
   */
  def concat[B >: A](suffix: SeqView.SomeSeqOps[B]^): SeqView[B]^{this, suffix} = new SeqView.Concat(this, suffix)
  /** Returns a view of the elements of this view followed by the elements of `suffix`;
   *  an alias of `concat`.
   *
   *  @tparam B the element type of the returned view, a supertype of this view's element type
   *  @param suffix the sequence whose elements follow those of this view
   *  @return a view of the concatenation of this view and `suffix`
   */
  def appendedAll[B >: A](suffix: SeqView.SomeSeqOps[B]^): SeqView[B]^{this, suffix} = new SeqView.Concat(this, suffix)
  /** Returns a view of the elements of `prefix` followed by the elements of this view.
   *
   *  @tparam B the element type of the returned view, a supertype of this view's element type
   *  @param prefix the sequence whose elements precede those of this view
   *  @return a view of the concatenation of `prefix` and this view
   */
  def prependedAll[B >: A](prefix: SeqView.SomeSeqOps[B]^): SeqView[B]^{this, prefix} = new SeqView.Concat(prefix, this)

  /** Returns a view of the elements of this view in the order given by `ord`.
   *
   *  The length of this view is computed immediately, so calling this method on a view
   *  over an infinite sequence does not terminate. The sort itself is deferred until an
   *  element of the returned view is first accessed, and its result is then cached.
   *
   *  @tparam B the type on which `ord` compares, a supertype of this view's element type
   *  @param ord the ordering to sort by
   *  @return a view of the same elements, sorted according to `ord`
   */
  override def sorted[B >: A](implicit ord: Ordering[B]): SeqView[A]^{this} = new SeqView.Sorted(this, ord)

  /** Returns `"SeqView"`, the prefix used in the string representation of this view. */
  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected def stringPrefix: String = "SeqView"
}

object SeqView {

  /** A `SeqOps` whose collection type and collection type constructor are unknown. */
  private type SomeSeqOps[+A] = SeqOps[A, AnyConstr, ?]

  /** A view that doesn't apply any transformation to an underlying sequence. */
  @SerialVersionUID(3L)
  class Id[+A](underlying: SomeSeqOps[A]^) extends AbstractSeqView[A] {
    /** Returns the element of the underlying sequence at index `idx`.
     *
     *  @param idx the index of the element
     *  @return the underlying element at `idx`
     */
    def apply(idx: Int): A = underlying.apply(idx)
    /** Returns the length of the underlying sequence. */
    def length: Int = underlying.length
    /** Returns an iterator over the underlying elements, in order. */
    def iterator: Iterator[A]^{this} = underlying.iterator
    /** Returns the underlying sequence's known size, or -1 if it is not known. */
    override def knownSize: Int = underlying.knownSize
    /** Returns `true` if the underlying sequence is empty. */
    override def isEmpty: Boolean = underlying.isEmpty
  }

  /** A view that applies a function to each element of an underlying sequence.
   *
   *  @tparam A the element type of the underlying sequence
   *  @tparam B the element type of the view
   *  @param underlying the sequence providing the elements
   *  @param f the function applied on demand to each element
   */
  @SerialVersionUID(3L)
  class Map[+A, +B](underlying: SomeSeqOps[A]^, f: A => B) extends View.Map[A, B](underlying, f) with SeqView[B] {
    /** Returns the result of applying `f` to the underlying element at index `idx`;
     *  `f` is re-applied on every access.
     *
     *  @param idx the index of the element
     *  @return `f` applied to the underlying element at `idx`
     */
    def apply(idx: Int): B = f(underlying(idx))
    /** Returns the length of the underlying sequence; mapping does not change it. */
    def length: Int = underlying.length
  }

  /** A view of an underlying sequence with one element appended.
   *
   *  @tparam A the element type of the view
   *  @param underlying the sequence providing all elements but the last
   *  @param elem the appended element, the last of the view
   */
  @SerialVersionUID(3L)
  class Appended[+A](underlying: SomeSeqOps[A]^, elem: A) extends View.Appended(underlying, elem) with SeqView[A] {
    /** Returns `elem` if `idx` equals the length of the underlying sequence, and
     *  otherwise the underlying element at index `idx`.
     *
     *  @param idx the index of the element
     *  @return the element of this view at `idx`
     */
    def apply(idx: Int): A = if (idx == underlying.length) elem else underlying(idx)
    /** Returns the length of the underlying sequence plus 1. */
    def length: Int = underlying.length + 1
  }

  /** A view of an underlying sequence with one element prepended.
   *
   *  @tparam A the element type of the view
   *  @param elem the prepended element, the first of the view
   *  @param underlying the sequence providing all elements but the first
   */
  @SerialVersionUID(3L)
  class Prepended[+A](elem: A, underlying: SomeSeqOps[A]^) extends View.Prepended(elem, underlying) with SeqView[A] {
    /** Returns `elem` if `idx` is 0, and otherwise the underlying element at index
     *  `idx - 1`.
     *
     *  @param idx the index of the element
     *  @return the element of this view at `idx`
     */
    def apply(idx: Int): A = if (idx == 0) elem else underlying(idx - 1)
    /** Returns the length of the underlying sequence plus 1. */
    def length: Int = underlying.length + 1
  }

  /** A view of the concatenation of two sequences.
   *
   *  @tparam A the element type of the view
   *  @param prefix the sequence providing the leading elements
   *  @param suffix the sequence providing the trailing elements
   */
  @SerialVersionUID(3L)
  class Concat[A](prefix: SomeSeqOps[A]^, suffix: SomeSeqOps[A]^) extends View.Concat[A](prefix, suffix) with SeqView[A] {
    /** Returns the prefix element at index `idx` if `idx` is smaller than the prefix
     *  length, and otherwise the suffix element at `idx` minus the prefix length; the
     *  prefix length is recomputed on every access.
     *
     *  @param idx the index of the element
     *  @return the element of this view at `idx`
     */
    def apply(idx: Int): A = {
      val l = prefix.length
      if (idx < l) prefix(idx) else suffix(idx - l)
    }
    /** Returns the sum of the lengths of the prefix and the suffix. */
    def length: Int = prefix.length + suffix.length
  }

  /** A view of the elements of an underlying sequence in reverse order.
   *
   *  @tparam A the element type of the view
   *  @param underlying the sequence whose elements are reversed
   */
  @SerialVersionUID(3L)
  class Reverse[A](underlying: SomeSeqOps[A]^) extends AbstractSeqView[A] {
    /** Returns the underlying element at index `size - 1 - i`, that is, the element at
     *  index `i` counting from the end of the underlying sequence.
     *
     *  @param i the index of the element in this view
     */
    def apply(i: Int) = underlying.apply(size - 1 - i)
    /** Returns the size of the underlying sequence; reversing does not change it. */
    def length = underlying.size
    /** Returns an iterator over the underlying elements in reverse order. */
    def iterator: Iterator[A]^{this} = underlying.reverseIterator
    /** Returns the underlying sequence's known size, or -1 if it is not known. */
    override def knownSize: Int = underlying.knownSize
    /** Returns `true` if the underlying sequence is empty. */
    override def isEmpty: Boolean = underlying.isEmpty
  }

  /** A view of the first `n` elements of an underlying sequence.
   *
   *  @tparam A the element type of the view
   *  @param underlying the sequence whose leading elements are taken
   *  @param n the number of elements to take
   */
  @SerialVersionUID(3L)
  class Take[+A](underlying: SomeSeqOps[A]^, n: Int) extends View.Take(underlying, n) with SeqView[A] {
    /** Returns the underlying element at index `idx`.
     *
     *  @param idx the index of the element
     *  @return the element of this view at `idx`
     *  @throws IndexOutOfBoundsException if `idx` is negative, at least `n`, or beyond
     *          the end of the underlying sequence
     */
    def apply(idx: Int): A = if (idx < n) {
      underlying(idx)
    } else {
      throw (
        if (underlying.knownSize >= 0) CommonErrors.indexOutOfBounds(index = idx, max = knownSize - 1)
        else CommonErrors.indexOutOfBounds(index = idx)
      )
    }
    /** Returns the smaller of the underlying sequence's length and the take count. */
    def length: Int = underlying.length min normN
  }

  /** A view of the last `n` elements of an underlying sequence.
   *
   *  The size of the underlying sequence is computed eagerly, when this view is created.
   *
   *  @tparam A the element type of the view
   *  @param underlying the sequence whose trailing elements are taken
   *  @param n the number of elements to take
   */
  @SerialVersionUID(3L)
  class TakeRight[+A](underlying: SomeSeqOps[A]^, n: Int) extends View.TakeRight(underlying, n) with SeqView[A] {
    private val delta = (underlying.size - (n max 0)) max 0
    /** Returns the underlying sequence's size minus the number of leading elements that
     *  were excluded when this view was created: the smaller of that size and the take
     *  count, if the underlying sequence has not changed size since.
     */
    def length = underlying.size - delta
    /** Returns the underlying element at index `i` plus the offset of the first taken
     *  element; bounds are checked only by the underlying sequence, not against the
     *  length of this view.
     *
     *  @param i the index of the element in this view
     */
    @throws[IndexOutOfBoundsException]
    def apply(i: Int) = underlying.apply(i + delta)
  }

  /** A view of the elements of an underlying sequence except the first `n`.
   *
   *  @tparam A the element type of the view
   *  @param underlying the sequence whose leading elements are dropped
   *  @param n the number of elements to drop
   */
  @SerialVersionUID(3L)
  class Drop[A](underlying: SomeSeqOps[A]^, n: Int) extends View.Drop[A](underlying, n) with SeqView[A] {
    /** Returns the underlying sequence's size minus the drop count, or 0 if the drop
     *  count is larger.
     */
    def length = (underlying.size - normN) max 0
    /** Returns the underlying element at index `i` plus the drop count; bounds are
     *  checked only by the underlying sequence, not against the length of this view.
     *
     *  @param i the index of the element in this view
     */
    @throws[IndexOutOfBoundsException]
    def apply(i: Int) = underlying.apply(i + normN)
    /** Returns a view that drops `n` more elements of the underlying sequence, fusing
     *  this drop and the new one into a single `Drop` whose drop count is the sum of
     *  the two, before that sum is clamped to be non-negative. A negative count on
     *  either side therefore offsets the other rather than being ignored on its own.
     *
     *  @param n the number of additional elements to drop
     *  @return a view of the underlying elements except the first `this.n + n`, or of
     *          all of them if that sum is not positive
     */
    override def drop(n: Int): SeqView[A]^{this} = new Drop(underlying, this.n + n)
  }

  /** A view of the elements of an underlying sequence except the last `n`.
   *
   *  The size of the underlying sequence is computed eagerly, when this view is created.
   *
   *  @tparam A the element type of the view
   *  @param underlying the sequence whose trailing elements are dropped
   *  @param n the number of elements to drop
   */
  @SerialVersionUID(3L)
  class DropRight[A](underlying: SomeSeqOps[A]^, n: Int) extends View.DropRight[A](underlying, n) with SeqView[A] {
    private val len = (underlying.size - (n max 0)) max 0
    /** Returns the underlying sequence's size minus the drop count (at least 0), as
     *  computed when this view was created.
     */
    def length = len
    /** Returns the underlying element at index `i`; bounds are checked only by the
     *  underlying sequence, not against the shortened length of this view.
     *
     *  @param i the index of the element
     */
    @throws[IndexOutOfBoundsException]
    def apply(i: Int) = underlying.apply(i)
  }

  /** A view of the elements of an underlying sequence in sorted order.
   *
   *  Creating the view computes the length of the underlying sequence eagerly. The sort
   *  itself is deferred until an element is first accessed; its result is then cached,
   *  and the reference to the underlying sequence released.
   *
   *  @tparam A the element type of the view
   *  @tparam B the type on which the ordering compares, a supertype of the element type
   */
  @SerialVersionUID(3L)
  class Sorted[A, B >: A] private (underlying_ : SomeSeqOps[A]^,
                                   private val len: Int,
                                   ord: Ordering[B])
    extends SeqView[A] {
    outer: Sorted[A, B]^ =>

    private var underlying = underlying_

    // force evaluation immediately by calling `length` so infinite collections
    // hang on `sorted`/`sortWith`/`sortBy` rather than on arbitrary method calls
    /** Creates a sorted view of `underlying`, eagerly computing its length; on a view
     *  over an infinite sequence, this constructor does not terminate.
     *
     *  @param underlying the sequence whose elements are sorted
     *  @param ord the ordering to sort by
     */
    def this(underlying: SomeSeqOps[A]^, ord: Ordering[B]) = this(underlying, underlying.length, ord)

    @SerialVersionUID(3L)
    private class ReverseSorted extends SeqView[A] {
      private lazy val _reversed = new SeqView.Reverse(_sorted)

      def apply(i: Int): A = _reversed.apply(i)
      def length: Int = len
      def iterator: Iterator[A]^{this} = Iterator.empty ++ _reversed.iterator // very lazy
      override def knownSize: Int = len
      override def isEmpty: Boolean = len == 0
      override def to[C1](factory: Factory[A, C1]): C1 = _reversed.to(factory)
      override def reverse: SeqView[A]^{this} = outer
      override protected def reversed: Iterable[A]^{outer} = outer

      override def sorted[B1 >: A](implicit ord1: Ordering[B1]): SeqView[A]^{this} =
        if (ord1 == Sorted.this.ord) outer
        else if (ord1.isReverseOf(Sorted.this.ord)) this
        else new Sorted(elems, len, ord1)
    }

    @volatile private var evaluated = false

    private lazy val _sorted: Seq[A] = {
      val res = {
        val len = this.len
        if (len == 0) Nil
        else if (len == 1) List(underlying.head)
        else {
          val arr = new Array[Any](len) // Array[Any] =:= Array[AnyRef]
          @annotation.unused val copied = underlying.copyToArray(arr)
          //assert(copied == len)
          java.util.Arrays.sort(arr.asInstanceOf[Array[AnyRef]], ord.asInstanceOf[Ordering[AnyRef]])
          // casting the Array[AnyRef] to Array[A] and creating an ArraySeq from it
          // is safe because:
          //   - the ArraySeq is immutable, and items that are not of type A
          //     cannot be added to it
          //   - we know it only contains items of type A (and if this collection
          //     contains items of another type, we'd get a CCE anyway)
          //   - the cast doesn't actually do anything in the runtime because the
          //     type of A is not known and Array[_] is Array[AnyRef]
          immutable.ArraySeq.unsafeWrapArray(arr.asInstanceOf[Array[A]])
        }
      }
      evaluated = true
      underlying = nullForGC[SomeSeqOps[A]]
      res
    }

    private def elems: SomeSeqOps[A]^{this} = {
      if (evaluated) _sorted else underlying
    }

    /** Returns the element at index `i` in sorted order, forcing the sort on the first
     *  element access; later accesses read the cached result.
     *
     *  @param i the index of the element
     *  @return the element of this view at `i`
     */
    def apply(i: Int): A = _sorted.apply(i)
    /** Returns the length, computed when this view was created; does not force the sort. */
    def length: Int = len
    /** Returns an iterator over the elements in sorted order; the sort is deferred until
     *  the iterator is first queried.
     */
    def iterator: Iterator[A]^{this} = Iterator.empty ++ _sorted.iterator // very lazy
    /** Returns the length, computed when this view was created; does not force the sort. */
    override def knownSize: Int = len
    /** Returns `true` if the length is 0; does not force the sort. */
    override def isEmpty: Boolean = len == 0
    /** Returns a collection containing the elements in sorted order, forcing the sort.
     *
     *  @tparam C1 the type of the collection to build
     *  @param factory the factory that builds the result
     *  @return a collection of the elements in sorted order
     */
    override def to[C1](factory: Factory[A, C1]): C1 = _sorted.to(factory)
    /** Returns a view of the elements in reverse sorted order; does not force the sort. */
    override def reverse: SeqView[A]^{this} = new ReverseSorted
    /** Returns a view of the elements in reverse sorted order; does not force the sort. */
    // we know `_sorted` is either tiny or has efficient random access,
    //  so this is acceptable for `reversed`
    override protected def reversed: Iterable[A]^{this} = new ReverseSorted

    /** Returns this view if `ord1` equals the ordering it was sorted by, a reverse-order
     *  view if `ord1` is the reverse of that ordering, and otherwise a new sorted view,
     *  reusing the cached sorted elements when the sort has already been performed.
     *
     *  @tparam B1 the type on which `ord1` compares
     *  @param ord1 the ordering to sort by
     *  @return a view of the same elements, sorted according to `ord1`
     */
    override def sorted[B1 >: A](implicit ord1: Ordering[B1]): SeqView[A]^{this} =
      if (ord1 == this.ord) this
      else if (ord1.isReverseOf(this.ord)) reverse
      else new Sorted(elems, len, ord1)
  }
}

/** Explicit instantiation of the `SeqView` trait to reduce class file size in subclasses. */
@SerialVersionUID(3L)
abstract class AbstractSeqView[+A] extends AbstractView[A] with SeqView[A]
