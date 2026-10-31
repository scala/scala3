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

/** Base trait for linearly accessed sequences that have efficient `head` and
 *  `tail` operations.
 *  Known subclasses: List, LazyList
 *
 *  @tparam A the element type of the sequence
 */
trait LinearSeq[+A] extends Seq[A]
  with LinearSeqOps[A, LinearSeq, LinearSeq[A]]
  with IterableFactoryDefaults[A, LinearSeq] {
  /** The prefix used in the string representation of this sequence, `"LinearSeq"`. */
  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected def stringPrefix: String = "LinearSeq"

  /** The factory used to build linear sequences, the [[LinearSeq$ `LinearSeq`]] companion object. */
  override def iterableFactory: SeqFactory[LinearSeq] = LinearSeq
}

@SerialVersionUID(3L)
object LinearSeq extends SeqFactory.Delegate[LinearSeq](immutable.LinearSeq)

/** Base trait for linear Seq operations.
 *
 *  @tparam A the element type of the sequence
 *  @tparam CC the type constructor for the collection (e.g., `List`, `LazyList`)
 *  @tparam C the concrete collection type
 */
transparent trait LinearSeqOps[+A, +CC[X] <: LinearSeq[X], +C <: LinearSeq[A] & LinearSeqOps[A, CC, C]] extends Any with SeqOps[A, CC, C] with caps.Pure { self =>

  /** @inheritdoc
   *
   *  Note: *Must* be overridden in subclasses. The default implementation that is inherited from [[SeqOps]]
   *     uses `lengthCompare`, which is defined here to use `isEmpty`.
   */
  override def isEmpty: Boolean

  /** @inheritdoc
   *
   *  Note: *Must* be overridden in subclasses. The default implementation is inherited from [[IterableOps]].
   */
  def head: A

  /** @inheritdoc
   *
   *  Note: *Must* be overridden in subclasses. The default implementation is inherited from [[IterableOps]].
   */
  def tail: C

  /** Returns the first element of this sequence wrapped in `Some`, or `None`
   *  if this sequence is empty.
   */
  override def headOption: Option[A] =
    if (isEmpty) None else Some(head)

  /** Returns an iterator over the elements of this sequence.
   *
   *  The iterator steps through the sequence with `head` and `tail`, and does
   *  not evaluate a tail before the corresponding element is requested, so it
   *  is lazy enough for sequences such as [[scala.collection.immutable.LazyList]].
   */
  def iterator: Iterator[A] =
    if (knownSize == 0) Iterator.empty
    else new LinearSeqIterator[A](this)

  /** Returns the number of elements in this sequence.
   *
   *  Computed by traversing the sequence with `tail`, so it takes time
   *  proportional to the number of elements and does not terminate for
   *  infinite sequences.
   */
  def length: Int = {
    var these = coll
    var len = 0
    while (these.nonEmpty) {
      len += 1
      these = these.tail
    }
    len
  }

  /** Returns the last element of this sequence.
   *
   *  Found by traversing the whole sequence, so it takes time proportional to
   *  the number of elements.
   *
   *  @throws NoSuchElementException if this sequence is empty
   */
  override def last: A = {
    if (isEmpty) throw new NoSuchElementException("LinearSeq.last")
    else {
      var these = coll
      var scout = tail
      while (scout.nonEmpty) {
        these = scout
        scout = scout.tail
      }
      these.head
    }
  }

  /** Compares the length of this sequence to a test value.
   *
   *  Traverses at most `len + 1` elements instead of computing the full
   *  length, so the running time is `O(length min len)`.
   *
   *  @param len the test value that gets compared with the length
   *  @return a negative value if `this.length < len`, zero if
   *          `this.length == len`, and a positive value if `this.length > len`
   *          (in particular, a positive value if `len` is negative)
   */
  override def lengthCompare(len: Int): Int = {
    @tailrec def loop(i: Int, xs: LinearSeq[A]): Int = {
      if (i == len)
        if (xs.isEmpty) 0 else 1
      else if (xs.isEmpty)
        -1
      else
        loop(i + 1, xs.tail)
    }
    if (len < 0) 1
    else loop(0, coll)
  }

  /** Compares the length of this sequence to the size of another `Iterable`.
   *
   *  If the size of `that` is known, delegates to `lengthCompare(Int)`;
   *  otherwise both collections are traversed in step and the traversal stops
   *  as soon as either is exhausted, so the running time is
   *  `O(this.length min that.size)`.
   *
   *  @param that the `Iterable` whose size is compared with this sequence's length
   *  @return a negative value if `this.length < that.size`, zero if they are
   *          equal, and a positive value if `this.length > that.size`
   */
  override def lengthCompare(that: Iterable[?]^): Int = {
    val thatKnownSize = that.knownSize

    if (thatKnownSize >= 0) this lengthCompare thatKnownSize
    else that match {
      case that: LinearSeq[?] =>
        var thisSeq = this
        var thatSeq = that
        while (thisSeq.nonEmpty && thatSeq.nonEmpty) {
          thisSeq = thisSeq.tail
          thatSeq = thatSeq.tail
        }
        java.lang.Boolean.compare(thisSeq.nonEmpty, thatSeq.nonEmpty)
      case _                  =>
        var thisSeq = this
        val thatIt = that.iterator
        while (thisSeq.nonEmpty && thatIt.hasNext) {
          thisSeq = thisSeq.tail
          thatIt.next()
        }
        java.lang.Boolean.compare(thisSeq.nonEmpty, thatIt.hasNext)
    }
  }

  /** Tests whether this sequence contains the given index.
   *
   *  Traverses at most `x + 1` elements to make the test, so the full length
   *  is not computed.
   *
   *  @param x the index to test
   *  @return `true` if `x` is non-negative and less than the length of this
   *          sequence, `false` otherwise
   */
  override def isDefinedAt(x: Int): Boolean = x >= 0 && lengthCompare(x) > 0

  // `apply` is defined in terms of `drop`, which is in turn defined in
  //  terms of `tail`.
  /** Returns the element at the specified index.
   *
   *  Found by traversing `n` tails from the start of this sequence, so it
   *  takes time proportional to `n`.
   *
   *  @param n the index of the element to retrieve, zero-based
   *  @return the element at index `n`
   *  @throws IndexOutOfBoundsException if `n` is negative or greater than or
   *          equal to the length of this sequence
   */
  @throws[IndexOutOfBoundsException]
  override def apply(n: Int): A = {
    if (n < 0) throw new IndexOutOfBoundsException(n.toString)
    val skipped = drop(n)
    if (skipped.isEmpty) throw new IndexOutOfBoundsException(n.toString)
    skipped.head
  }

  /** Applies the function `f` to each element of this sequence in order.
   *
   *  Traverses with `head` and `tail` directly instead of creating an
   *  iterator.
   *
   *  @tparam U the result type of `f`, ignored
   *  @param f the function applied to each element for its side effects
   */
  override def foreach[U](f: A => U): Unit = {
    var these: LinearSeq[A] = coll
    while (!these.isEmpty) {
      f(these.head)
      these = these.tail
    }
  }

  /** Tests whether the predicate `p` holds for all elements of this sequence.
   *
   *  Traverses with `head` and `tail` and stops at the first element for
   *  which `p` is false.
   *
   *  @param p the predicate used to test elements
   *  @return `true` if `p` holds for all elements, or this sequence is empty,
   *          `false` otherwise
   */
  override def forall(p: A => Boolean): Boolean = {
    var these: LinearSeq[A] = coll
    while (!these.isEmpty) {
      if (!p(these.head)) return false
      these = these.tail
    }
    true
  }

  /** Tests whether the predicate `p` holds for at least one element of this
   *  sequence.
   *
   *  Traverses with `head` and `tail` and stops at the first element for
   *  which `p` is true.
   *
   *  @param p the predicate used to test elements
   *  @return `true` if `p` holds for some element of this sequence, `false`
   *          otherwise
   */
  override def exists(p: A => Boolean): Boolean = {
    var these: LinearSeq[A] = coll
    while (!these.isEmpty) {
      if (p(these.head)) return true
      these = these.tail
    }
    false
  }

  /** Tests whether this sequence contains a given value as an element.
   *
   *  Traverses with `head` and `tail` and stops at the first element equal to
   *  `elem`.
   *
   *  @tparam A1 the type of the element to test (a supertype of `A`)
   *  @param elem the element to test
   *  @return `true` if some element of this sequence is equal (as determined
   *          by `==`) to `elem`, `false` otherwise
   */
  override def contains[A1 >: A](elem: A1): Boolean = {
    var these: LinearSeq[A] = coll
    while (!these.isEmpty) {
      if (these.head == elem) return true
      these = these.tail
    }
    false
  }

  /** Finds the first element of this sequence satisfying a predicate, if any.
   *
   *  Traverses with `head` and `tail` and stops at the first element for
   *  which `p` is true.
   *
   *  @param p the predicate used to test elements
   *  @return an option value containing the first element of this sequence
   *          that satisfies `p`, or `None` if none exists
   */
  override def find(p: A => Boolean): Option[A] = {
    var these: LinearSeq[A] = coll
    while (!these.isEmpty) {
      if (p(these.head)) return Some(these.head)
      these = these.tail
    }
    None
  }

  /** Applies the binary operator `op` between `z` and all elements of this
   *  sequence, going left to right.
   *
   *  Traverses with `head` and `tail` directly instead of creating an
   *  iterator.
   *
   *  @tparam B the result type of the binary operator
   *  @param z the start value, combined with the first element
   *  @param op the binary operator
   *  @return the result of inserting `op` between consecutive elements of
   *          this sequence, going left to right with the start value `z` on
   *          the left, or `z` if this sequence is empty
   */
  override def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var acc = z
    var these: LinearSeq[A] = coll
    while (!these.isEmpty) {
      acc = op(acc, these.head)
      these = these.tail
    }
    acc
  }

  /** Checks whether corresponding elements of the given collection compare
   *  equal (with respect to `==`) to elements of this sequence.
   *
   *  When `that` is also a linear sequence, the two are traversed in step
   *  with `head` and `tail`, and a comparison step succeeds immediately when
   *  both remainders are the same instance, so shared suffixes are not
   *  traversed. Otherwise falls back to the default iterator-based
   *  comparison.
   *
   *  @tparam B the type of the elements of `that`
   *  @param that the collection to compare
   *  @return `true` if both collections contain equal elements in the same
   *          order, `false` otherwise
   */
  override def sameElements[B >: A](that: IterableOnce[B]^): Boolean = {
    @tailrec def linearSeqEq(a: LinearSeq[B], b: LinearSeq[B]): Boolean =
      (a eq b) || {
        if (a.nonEmpty && b.nonEmpty && a.head == b.head) {
          linearSeqEq(a.tail, b.tail)
        }
        else {
          a.isEmpty && b.isEmpty
        }
      }

    that match {
      case that: LinearSeq[B @unchecked] => linearSeqEq(coll, that)
      case _ => super.sameElements(that)
    }
  }

  /** Computes the length of the longest segment that starts from some index
   *  and whose elements all satisfy some predicate.
   *
   *  Traverses with `head` and `tail` and stops at the first element from
   *  index `from` onwards that does not satisfy `p`. May not terminate for
   *  infinite sequences.
   *
   *  @param p the predicate used to test elements
   *  @param from the index where the search starts
   *  @return the length of the longest segment of this sequence starting from
   *          index `from` such that every element of the segment satisfies `p`
   */
  override def segmentLength(p: A => Boolean, from: Int): Int = {
    var i = 0
    var seq = drop(from)
    while (seq.nonEmpty && p(seq.head)) {
      i += 1
      seq = seq.tail
    }
    i
  }

  /** Finds the index of the first element satisfying some predicate after or
   *  at some start index.
   *
   *  Traverses with `head` and `tail` and stops at the first match. May not
   *  terminate for infinite sequences.
   *
   *  @param p the predicate used to test elements
   *  @param from the start index (treated as `0` if negative)
   *  @return the index `>= from` of the first element of this sequence that
   *          satisfies the predicate `p`, or `-1` if none exists
   */
  override def indexWhere(p: A => Boolean, from: Int): Int = {
    var i = math.max(from, 0)
    var these: LinearSeq[A] = this drop from
    while (these.nonEmpty) {
      if (p(these.head))
        return i

      i += 1
      these = these.tail
    }
    -1
  }

  /** Finds the index of the last element satisfying some predicate before or
   *  at the given end index.
   *
   *  Traverses forwards with `head` and `tail`, remembering the last match,
   *  and stops after index `end`. Will not terminate for infinite sequences
   *  if `end` is `Int.MaxValue`.
   *
   *  @param p the predicate used to test elements
   *  @param end the maximum index to consider (inclusive)
   *  @return the index `<= end` of the last element of this sequence that
   *          satisfies the predicate `p`, or `-1` if none exists
   */
  override def lastIndexWhere(p: A => Boolean, end: Int): Int = {
    var i = 0
    var these: LinearSeq[A] = coll
    var last = -1
    while (!these.isEmpty && i <= end) {
      if (p(these.head)) last = i
      these = these.tail
      i += 1
    }
    last
  }

  /** Finds the last element of this sequence satisfying a predicate, if any.
   *
   *  Traverses the whole sequence with `head` and `tail`, remembering the
   *  last match; does not terminate for infinite sequences.
   *
   *  @param p the predicate used to test elements
   *  @return an option value containing the last element of this sequence
   *          that satisfies `p`, or `None` if none exists
   */
  override def findLast(p: A => Boolean): Option[A] = {
    var these: LinearSeq[A] = coll
    var found = false
    var last: A = null.asInstanceOf[A] // don't use `Option`, to prevent excessive `Some` allocation
    while (these.nonEmpty) {
      val elem = these.head
      if (p(elem)) {
        found = true
        last = elem
      }
      these = these.tail
    }
    if (found) Some(last) else None
  }

  /** Iterates over the tails of this sequence. The first value is this
   *  sequence and the final one is an empty sequence, with the intervening
   *  values the results of successive applications of `tail`.
   *
   *  Each tail is obtained directly with `tail`, so no elements are copied.
   */
  override def tails: Iterator[C] = {
    val end = Iterator.single(empty)
    Iterator.iterate(coll)(_.tail).takeWhile(_.nonEmpty) ++ end
  }
}

transparent trait StrictOptimizedLinearSeqOps[+A, +CC[X] <: LinearSeq[X], +C <: LinearSeq[A] & StrictOptimizedLinearSeqOps[A, CC, C]] extends Any with LinearSeqOps[A, CC, C] with StrictOptimizedSeqOps[A, CC, C] { self =>
  // A more efficient iterator implementation than the default LinearSeqIterator
  /** Returns an iterator over the elements of this sequence.
   *
   *  Steps through the sequence with `head` and `tail` directly. Because this
   *  trait is for strictly evaluated sequences, no lazy indirection is needed,
   *  making this more efficient than the default [[LinearSeqOps]] iterator.
   */
  override def iterator: Iterator[A] = new AbstractIterator[A] {
    private var current = StrictOptimizedLinearSeqOps.this
    def hasNext = !current.isEmpty
    def next() = { val r = current.head; current = current.tail; r }
  }

  // Optimized version of `drop` that avoids copying
  /** Returns the sequence that remains after the first `n` elements are
   *  dropped.
   *
   *  Applies `tail` up to `n` times instead of copying, so the result shares
   *  structure with this sequence.
   *
   *  @param n the number of elements to drop
   *  @return the `n`th tail of this sequence, this sequence itself if `n` is
   *          non-positive, or the empty sequence if `n` is greater than or
   *          equal to the length
   */
  override def drop(n: Int): C = {
    @tailrec def loop(n: Int, s: C): C =
      if (n <= 0 || s.isEmpty) s
      else loop(n - 1, s.tail)
    loop(n, coll)
  }

  /** Drops the longest prefix of elements that satisfy a predicate.
   *
   *  Applies `tail` repeatedly instead of copying, so the result shares
   *  structure with this sequence.
   *
   *  @param p the predicate used to test elements
   *  @return the longest suffix of this sequence whose first element does not
   *          satisfy `p`, or the empty sequence if all elements satisfy `p`
   */
  override def dropWhile(p: A => Boolean): C = {
    @tailrec def loop(s: C): C =
      if (s.nonEmpty && p(s.head)) loop(s.tail)
      else s
    loop(coll)
  }
}

/** A specialized Iterator for LinearSeqs that is lazy enough for Stream and LazyList. This is accomplished by not
 *  evaluating the tail after returning the current head.
 *
 *  @tparam A the element type of the linear sequence being iterated
 *  @param coll the linear sequence to iterate over
 */
private[collection] final class LinearSeqIterator[A](coll: LinearSeqOps[A, LinearSeq, LinearSeq[A]]) extends AbstractIterator[A] {
  // A call-by-need cell
  private final class LazyCell(st: => LinearSeqOps[A, LinearSeq, LinearSeq[A]]) { lazy val v = st }

  private var these: LazyCell = {
    // Reassign reference to avoid creating a private class field and holding a reference to the head.
    // LazyCell would otherwise close over `coll`.
    val initialHead = coll
    new LazyCell(initialHead)
  }

  def hasNext: Boolean = these.v.nonEmpty

  def next(): A =
    if (isEmpty) Iterator.empty.next()
    else {
      val cur    = these.v
      val result = cur.head
      these = new LazyCell(cur.tail)
      result
    }
}
