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

import scala.annotation.{nowarn, tailrec}
import scala.collection.Searching.{Found, InsertionPoint, SearchResult}
import scala.collection.Stepper.EfficientSplit
import scala.math.Ordering

/** Base trait for indexed sequences that have efficient `apply` and `length`.
 *
 *  @tparam A the element type of the indexed sequence
 */
trait IndexedSeq[+A] extends Seq[A]
  with IndexedSeqOps[A, IndexedSeq, IndexedSeq[A]]
  with IterableFactoryDefaults[A, IndexedSeq] {
  /** The prefix used in the string representation of this sequence, `"IndexedSeq"`. */
  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected def stringPrefix: String = "IndexedSeq"

  /** The factory used to build indexed sequences, the [[IndexedSeq$ `IndexedSeq`]] companion object. */
  override def iterableFactory: SeqFactory[IndexedSeq] = IndexedSeq
}

@SerialVersionUID(3L)
object IndexedSeq extends SeqFactory.Delegate[IndexedSeq](immutable.IndexedSeq)

/** Base trait for indexed Seq operations.
 *
 *  @tparam A the element type of the sequence
 *  @tparam CC the type constructor for the resulting collection (e.g., `IndexedSeq`)
 *  @tparam C the type of the concrete collection
 */
transparent trait IndexedSeqOps[+A, +CC[_], +C] extends Any with SeqOps[A, CC, C] { self: IndexedSeqOps[A, CC, C]^ =>

  /** Returns an iterator over the elements of this sequence.
   *
   *  The iterator accesses elements by index, so each step costs one call
   *  to `apply`.
   */
  def iterator: Iterator[A]^{this} = view.iterator

  /** Returns a [[scala.collection.Stepper]] for the elements of this sequence
   *  that supports efficient splitting, enabling parallel processing.
   *
   *  The stepper accesses elements by index. For elements of type `Int`, `Long`
   *  or `Double`, a primitive-typed stepper is used that does not box the
   *  elements.
   *
   *  @tparam S the type of the returned `Stepper`, determined by the implicit `StepperShape`
   *  @param shape the `StepperShape` that determines the concrete `Stepper` subtype to return
   *  @return an indexed stepper over the elements of this sequence, marked with
   *          [[scala.collection.Stepper.EfficientSplit]]
   */
  override def stepper[S <: Stepper[?]](implicit shape: StepperShape[A, S]): S & EfficientSplit = {
    import convert.impl._
    val s = shape.shape match {
      case StepperShape.IntShape    => new IntIndexedSeqStepper   (this.asInstanceOf[IndexedSeqOps[Int, AnyConstr, ?]],    0, length)
      case StepperShape.LongShape   => new LongIndexedSeqStepper  (this.asInstanceOf[IndexedSeqOps[Long, AnyConstr, ?]],   0, length)
      case StepperShape.DoubleShape => new DoubleIndexedSeqStepper(this.asInstanceOf[IndexedSeqOps[Double, AnyConstr, ?]], 0, length)
      case _                        => shape.parUnbox(new AnyIndexedSeqStepper[A](this, 0, length))
    }
    s.asInstanceOf[S & EfficientSplit]
  }

  /** Returns an iterator over the elements of this sequence in reverse order.
   *
   *  The iterator accesses elements by index, from `length - 1` down to `0`,
   *  without copying the sequence.
   */
  override def reverseIterator: Iterator[A]^{this} = view.reverseIterator

  /* TODO 2.14+ uncomment and delete related code in IterableOnce
  @tailrec private def foldl[B](start: Int, end: Int, z: B, op: (B, A) => B): B =
    if (start == end) z
    else foldl(start + 1, end, op(z, apply(start)), op)
   */

  @tailrec private def foldr[B](start: Int, end: Int, z: B, op: (A, B) => B): B =
    if (start == end) z
    else foldr(start, end - 1, op(apply(end - 1), z), op)

  //override def foldLeft[B](z: B)(op: (B, A) => B): B = foldl(0, length, z, op)

  /** Applies the binary operator `op` between all elements of this sequence and
   *  `z`, going right to left.
   *
   *  Elements are accessed by index, from the last element down to the first,
   *  so no reversed intermediate collection is built.
   *
   *  @tparam B the result type of the binary operator
   *  @param z the start value, combined with the last element first
   *  @param op the binary operator
   *  @return the result of inserting `op` between consecutive elements of this
   *          sequence, going right to left with the start value `z` on the
   *          right, or `z` if this sequence is empty
   */
  override def foldRight[B](z: B)(op: (A, B) => B): B = foldr(0, length, z, op)

  //override def reduceLeft[B >: A](op: (B, A) => B): B = if (length > 0) foldl(1, length, apply(0), op) else super.reduceLeft(op)

  //override def reduceRight[B >: A](op: (A, B) => B): B = if (length > 0) foldr(0, length - 1, apply(length - 1), op) else super.reduceRight(op)

  /** Returns an [[IndexedSeqView]] over the elements of this sequence.
   *
   *  The view supports indexed access and is not evaluated: it reflects any
   *  changes to the underlying sequence.
   */
  override def view: IndexedSeqView[A]^{this} = new IndexedSeqView.Id[A](this)

  /** Returns an [[IndexedSeqView]] over the elements of this sequence between
   *  indices `from` (inclusive) and `until` (exclusive).
   *
   *  @param from the index of the first element of the view
   *  @param until the index following the last element of the view
   *  @return a non-strict view of the specified slice of this sequence
   */
  @deprecated("Use .view.slice(from, until) instead of .view(from, until)", "2.13.0")
  override def view(from: Int, until: Int): IndexedSeqView[A]^{this} = view.slice(from, until)

  /** Returns a non-strict view of this sequence in reverse order.
   *
   *  The view accesses elements by index, so no copy is made. It is used by
   *  the default implementations of reversal-based operations.
   */
  override protected def reversed: Iterable[A]^{this} = new IndexedSeqView.Reverse(this)

  // Override transformation operations to use more efficient views than the default ones
  /** Returns a copy of this sequence with the element `elem` prepended.
   *
   *  The result is built from an indexed view, so the elements of this
   *  sequence are accessed by index rather than through an iterator.
   *
   *  @tparam B the element type of the returned sequence
   *  @param elem the prepended element
   *  @return a new collection consisting of `elem` followed by all elements
   *          of this sequence
   */
  override def prepended[B >: A](elem: B): CC[B]^{this} = iterableFactory.from(new IndexedSeqView.Prepended(elem, this))

  /** Returns a collection containing the first `n` elements of this sequence.
   *
   *  The result is built from an indexed view of the selected elements.
   *
   *  @param n the number of elements to take
   *  @return a collection consisting of the first `n` elements of this
   *          sequence, all elements if `n` is greater than the length, or an
   *          empty collection if `n` is non-positive
   */
  override def take(n: Int): C^{this} = fromSpecific(new IndexedSeqView.Take(this, n))

  /** Returns a collection containing the last `n` elements of this sequence.
   *
   *  The result is built from an indexed view of the selected elements.
   *
   *  @param n the number of elements to take
   *  @return a collection consisting of the last `n` elements of this
   *          sequence, all elements if `n` is greater than the length, or an
   *          empty collection if `n` is non-positive
   */
  override def takeRight(n: Int): C^{this} = fromSpecific(new IndexedSeqView.TakeRight(this, n))

  /** Returns a collection containing all elements of this sequence except the
   *  first `n`.
   *
   *  The result is built from an indexed view of the selected elements, so the
   *  dropped prefix is skipped rather than iterated over.
   *
   *  @param n the number of elements to drop
   *  @return a collection consisting of all elements of this sequence except
   *          the first `n`: all elements if `n` is non-positive, or an empty
   *          collection if `n` is greater than or equal to the length
   */
  override def drop(n: Int): C^{this} = fromSpecific(new IndexedSeqView.Drop(this, n))

  /** Returns a collection containing all elements of this sequence except the
   *  last `n`.
   *
   *  The result is built from an indexed view of the selected elements.
   *
   *  @param n the number of elements to drop
   *  @return a collection consisting of all elements of this sequence except
   *          the last `n`: all elements if `n` is non-positive, or an empty
   *          collection if `n` is greater than or equal to the length
   */
  override def dropRight(n: Int): C^{this} = fromSpecific(new IndexedSeqView.DropRight(this, n))

  /** Builds a new collection by applying a function to all elements of this
   *  sequence.
   *
   *  The result is built from an indexed view, so the elements of this
   *  sequence are accessed by index rather than through an iterator.
   *
   *  @tparam B the element type of the returned collection
   *  @param f the function to apply to each element
   *  @return a new collection consisting of the results of applying `f` to
   *          each element of this sequence, in order
   */
  override def map[B](f: A => B): CC[B]^{this, f} = iterableFactory.from(new IndexedSeqView.Map(this, f))

  /** Returns a new collection with the elements of this sequence in reverse
   *  order.
   *
   *  The result is built from an indexed view that accesses the elements of
   *  this sequence from the last down to the first, so no intermediate
   *  reversed collection is created.
   */
  override def reverse: C^{this} = fromSpecific(new IndexedSeqView.Reverse(this))

  /** Returns a collection containing the elements of this sequence between
   *  indices `from` (inclusive) and `until` (exclusive).
   *
   *  The result is built from an indexed view of the selected elements, so the
   *  skipped prefix is not iterated over.
   *
   *  @param from the index of the first element to include, clamped to the
   *              valid range
   *  @param until the index following the last element to include, clamped to
   *               the valid range
   *  @return a collection containing the elements of this sequence at indices
   *          `from` up to but not including `until`, or an empty collection if
   *          `until <= from`
   */
  override def slice(from: Int, until: Int): C^{this} = fromSpecific(new IndexedSeqView.Slice(this, from, until))

  /** Groups elements in fixed size blocks by passing a "sliding window" over
   *  them, advancing the starting index by `step` for each block.
   *
   *  Each block is produced with `slice`, so elements are accessed by index
   *  and only the elements of each window are copied. The returned iterator is
   *  empty if this sequence is empty; otherwise the last block may be smaller
   *  than `size` if fewer than `size` elements remain.
   *
   *  @param size the number of elements per group
   *  @param step the distance between the first elements of successive groups
   *  @return an iterator producing collections of `size` elements, except
   *          possibly the last one, which may be smaller
   *  @throws IllegalArgumentException if `size` or `step` is less than 1
   *  @throws java.util.ConcurrentModificationException from the returned
   *          iterator, if the length of this sequence changes during iteration
   */
  override def sliding(size: Int, step: Int): Iterator[C^{this}]^{this} = {
    require(size >= 1 && step >= 1, s"size=$size and step=$step, but both must be positive")
    val it = new IndexedSeqSlidingIterator[A, CC, C](this, size, step)
    it.asInstanceOf[Iterator[Nothing]] // TODO: seems like CC cannot figure this out yet
  }

  /** Returns the first element of this sequence, `apply(0)`.
   *
   *  @throws NoSuchElementException if this sequence is empty
   */
  override def head: A =
    if (!isEmpty) apply(0)
    else throw new NoSuchElementException(s"head of empty ${
      self match {
        case self: IndexedSeq[?] => self.collectionClassName
        case _ => toString
      }
    }")

  /** Returns the first element of this sequence wrapped in `Some`, or `None`
   *  if this sequence is empty.
   */
  override def headOption: Option[A] = if (isEmpty) None else Some(head)

  /** Returns the last element of this sequence, `apply(length - 1)`.
   *
   *  @throws NoSuchElementException if this sequence is empty
   */
  override def last: A =
    if (!isEmpty) apply(length - 1)
    else throw new NoSuchElementException(s"last of empty ${
      self match {
        case self: IndexedSeq[?] => self.collectionClassName
        case _ => toString
      }
    }")

  // We already inherit an efficient `lastOption = if (isEmpty) None else Some(last)`

  /** Compares the length of this sequence to a test value.
   *
   *  Because indexed sequences have an efficient `length`, this compares
   *  `length` and `len` directly rather than iterating.
   *
   *  @param len the test value that gets compared with the length
   *  @return a negative value if `this.length < len`, zero if
   *          `this.length == len`, and a positive value if `this.length > len`
   */
  override final def lengthCompare(len: Int): Int = Integer.compare(length, len)

  /** Returns the number of elements in this sequence, which is always known
   *  and equal to `length`.
   */
  override def knownSize: Int = length

  /** Compares the length of this sequence to the size of another `Iterable`.
   *
   *  Delegates to `that.sizeCompare(length)` with the result inverted, so
   *  `that` is traversed at most `length` elements rather than this sequence
   *  being traversed.
   *
   *  @param that the `Iterable` whose size is compared with this sequence's length
   *  @return a negative value if `this.length < that.size`, zero if they are
   *          equal, and a positive value if `this.length > that.size`
   */
  override final def lengthCompare(that: Iterable[?]^): Int = {
    val res = that.sizeCompare(length)
    // can't just invert the result, because `-Int.MinValue == Int.MinValue`
    if (res == Int.MinValue) 1 else -res
  }

  /** Searches this sorted sequence for a specific element using binary search,
   *  taking `O(log length)` comparisons.
   *
   *  The sequence should be sorted with the same `Ordering` before calling;
   *  otherwise, the results are undefined.
   *
   *  @tparam B the element type used for searching and ordering (a supertype of `A`)
   *  @param elem the element to find
   *  @param ord the ordering to be used to compare elements
   *  @return a `Found` value containing an index at which `elem` appears in
   *          this sequence (not necessarily the first one), or the
   *          `InsertionPoint` where `elem` would be inserted if it is not in
   *          this sequence
   */
  override def search[B >: A](elem: B)(implicit ord: Ordering[B]): SearchResult =
    binarySearch(elem, 0, length)(using ord)

  /** Searches within an interval in this sorted sequence for a specific element
   *  using binary search.
   *
   *  The sequence should be sorted with the same `Ordering` before calling;
   *  otherwise, the results are undefined. The interval is clamped to the
   *  valid index range of this sequence.
   *
   *  @tparam B the element type used for searching and ordering (a supertype of `A`)
   *  @param elem the element to find
   *  @param from the index where the search starts (treated as `0` if negative)
   *  @param to the index following where the search ends (clamped to `length`)
   *  @param ord the ordering to be used to compare elements
   *  @return a `Found` value containing an index within the interval at which
   *          `elem` appears (not necessarily the first one), or the
   *          `InsertionPoint` where `elem` would be inserted; if the clamped
   *          interval is empty, an `InsertionPoint` at the clamped `from`
   */
  override def search[B >: A](elem: B, from: Int, to: Int)(implicit ord: Ordering[B]): SearchResult =
    binarySearch(elem, from, to)(using ord)

  @tailrec
  private def binarySearch[B >: A](elem: B, from: Int, to: Int)
                                        (implicit ord: Ordering[B]): SearchResult = {
    if (from < 0) binarySearch(elem, 0, to)
    else if (to > length) binarySearch(elem, from, length)
    else if (to <= from) InsertionPoint(from)
    else {
      val idx = from + (to - from - 1) / 2
      math.signum(ord.compare(elem, apply(idx))) match {
        case -1 => binarySearch(elem, from, idx)(using ord)
        case  1 => binarySearch(elem, idx + 1, to)(using ord)
        case  _ => Found(idx)
      }
    }
  }
}

/** A fast sliding iterator for IndexedSeqs which uses the underlying `slice` operation.
 *
 *  @tparam A the element type of the sequence
 *  @tparam CC the type constructor for the resulting collection (e.g., `IndexedSeq`)
 *  @tparam C the type of the concrete collection being sliced
 *  @param s the underlying indexed sequence from which slices are produced
 *  @param size the number of elements in each slice; must be positive
 *  @param step the distance between the starting positions of successive slices; must be positive
 */
private final class IndexedSeqSlidingIterator[A, CC[_], C](s: IndexedSeqOps[A, CC, C]^, size: Int, step: Int)
  extends AbstractIterator[C^{s}] {
  // CC note: seems like the compiler cannot figure out that this class <: Iterator[C^{s}],
  // so we need a cast when upcasting is needed.

  private val len = s.length
  private var pos = 0
  private def chklen: Boolean = len == s.length || {
    throw new java.util.ConcurrentModificationException("collection size changed during iteration")
    false
  }

  /** Returns `true` if at least one more window can be produced.
   *
   *  @throws java.util.ConcurrentModificationException if the length of the
   *          underlying sequence has changed since this iterator was created
   */
  def hasNext: Boolean = chklen && pos < len

  /** Returns the next window, a slice of at most `size` elements starting at
   *  the current position, and advances the position by `step` (or to the end,
   *  if the window reached the end of the sequence).
   *
   *  @throws NoSuchElementException if no more windows remain
   *  @throws java.util.ConcurrentModificationException if the length of the
   *          underlying sequence has changed since this iterator was created
   */
  def next(): C^{s} = if (!chklen || !hasNext) Iterator.empty.next() else {
    val end = { val x = pos + size; if (x < 0 || x > len) len else x } // (pos.toLong + size).min(len).toInt
    val slice = s.slice(pos, end)
    pos =
      if (end >= len) len
      else { val x = pos + step; if (x < 0 || x > len) len else x } // (pos.toLong + step).min(len).toInt
    slice
  }
}
