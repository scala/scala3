/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection.immutable

import scala.language.`2.13`

import scala.collection.Stepper.EfficientSplit
import scala.collection.convert.impl.RangeStepper
import scala.collection.{AbstractIterator, AnyStepper, IterableFactoryDefaults, Iterator, Stepper, StepperShape}
import scala.util.hashing.MurmurHash3

/** The `Range` class represents integer values in range
 *  *[start;end)* with non-zero step value `step`.
 *  It's a special case of an indexed sequence.
 *  For example:
 *
 *  ```
 *     val r1 = 0 until 10
 *     val r2 = r1.start until r1.end by r1.step + 1
 *     println(r2.length) // = 5
 *  ```
 *
 *  Ranges that contain more than `Int.MaxValue` elements can be created, but
 *  these overfull ranges have only limited capabilities. Any method that
 *  could require a collection of over `Int.MaxValue` length to be created, or
 *  could be asked to index beyond `Int.MaxValue` elements will throw an
 *  exception. Overfull ranges can safely be reduced in size by changing
 *  the step size (e.g. `by 3`) or taking/dropping elements. `contains`,
 *  `equals`, and access to the ends of the range (`head`, `last`, `tail`,
 *  `init`) are also permitted on overfull ranges.
 *
 *  @param start       the start of this range.
 *  @param end         the end of the range.  For exclusive ranges, e.g.
 *                     `Range(0,3)` or `(0 until 3)`, this is one
 *                     step past the last one in the range.  For inclusive
 *                     ranges, e.g. `Range.inclusive(0,3)` or `(0 to 3)`,
 *                     it may be in the range if it is not skipped by the step size.
 *                     To find the last element inside a non-empty range,
 *                     use `last` instead.
 *  @param step        the step for the range.
 *
 *  @define coll range
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 *  @define doesNotUseBuilders
 *    **Note:** this method does not use builders to construct a new range,
 *         and its complexity is O(1).
 */
@SerialVersionUID(3L)
sealed abstract class Range(
  /** The start value of this range; its first element when this range is non-empty. */
  val start: Int,
  /** The end value of this range; not necessarily an element of it (see `last` for the last actual element). */
  val end: Int,
  /** The difference between successive elements of this range; never zero. */
  val step: Int
)
  extends AbstractSeq[Int]
    with IndexedSeq[Int]
    with IndexedSeqOps[Int, IndexedSeq, IndexedSeq[Int]]
    with StrictOptimizedSeqOps[Int, IndexedSeq, IndexedSeq[Int]]
    with IterableFactoryDefaults[Int, IndexedSeq]
    with Serializable { range =>

  /** Returns a new iterator over all elements of this range, from first to last. */
  final override def iterator: Iterator[Int] = new RangeIterator(start, step, lastElement, isEmpty)

  /** Returns a [[scala.collection.Stepper]] for the elements of this range.
   *
   *  @tparam S the type of the stepper, determined by `shape`
   *  @param shape an implicit value determining the type of stepper to create:
   *         an `IntStepper` for the primitive `Int` shape, or a boxing
   *         `AnyStepper` for the reference shape
   *  @return a stepper over the elements of this range; it supports efficient splitting
   *  @throws IllegalArgumentException if this range contains more than
   *          `Int.MaxValue` elements
   */
  override final def stepper[S <: Stepper[_]](implicit shape: StepperShape[Int, S]): S with EfficientSplit = {
    val st = new RangeStepper(start, step, 0, length)
    val r =
      if (shape.shape == StepperShape.IntShape) st
      else {
        assert(shape.shape == StepperShape.ReferenceShape, s"unexpected StepperShape: $shape")
        AnyStepper.ofParIntStepper(st)
      }
    r.asInstanceOf[S with EfficientSplit]
  }

  private[this] def gap           = end.toLong - start.toLong
  private[this] def isExact       = gap % step == 0
  private[this] def hasStub       = isInclusive || !isExact
  private[this] def longLength    = gap / step + ( if (hasStub) 1 else 0 )

  /** Returns `true` if this range is inclusive (built with `to` or
   *  `Range.inclusive`), `false` if it is exclusive (built with `until` or
   *  `Range.apply`).
   *
   *  Even in an inclusive range, `end` is an element only if it is reachable
   *  from `start` in `step` increments.
   */
  def isInclusive: Boolean

  /** Whether this range contains no elements.
   *
   *  A range is empty when `step` leads from `start` away from `end`, or when
   *  it is exclusive with `start == end`. This value is computed once at
   *  construction.
   */
  final override val isEmpty: Boolean = (
    (start > end && step > 0)
      || (start < end && step < 0)
      || (start == end && !isInclusive)
    )

  private[this] val numRangeElements: Int = {
    if (step == 0) throw new IllegalArgumentException("step cannot be 0.")
    else if (isEmpty) 0
    else {
      val len = longLength
      if (len > scala.Int.MaxValue) -1
      else len.toInt
    }
  }

  /** Returns the number of elements in this range, in constant time.
   *
   *  @throws IllegalArgumentException if this range contains more than
   *          `Int.MaxValue` elements
   */
  final def length = if (numRangeElements < 0) fail() else numRangeElements

  // This field has a sensible value only for non-empty ranges
  private[this] val lastElement = step match {
    case 1  => if (isInclusive) end else end-1
    case -1 => if (isInclusive) end else end+1
    case _  =>
      val remainder = (gap % step).toInt
      if (remainder != 0) end - remainder
      else if (isInclusive) end
      else end - step
  }

  /** The last element of this range.  This method will return the correct value
   *  even if there are too many elements to iterate over.
   */
  final override def last: Int =
    if (isEmpty) throw Range.emptyRangeError("last") else lastElement
  /** Returns the first element of this range, which is always `start`.
   *
   *  @throws NoSuchElementException if this range is empty
   */
  final override def head: Int =
    if (isEmpty) throw Range.emptyRangeError("head") else start

  /** Creates a new range containing all the elements of this range except the last one.
   *
   *  $doesNotUseBuilders
   *
   *  @return  a new range consisting of all the elements of this range except the last one.
   */
  final override def init: Range =
    if (isEmpty) throw Range.emptyRangeError("init") else dropRight(1)

  /** Creates a new range containing all the elements of this range except the first one.
   *
   *  $doesNotUseBuilders
   *
   *  @return  a new range consisting of all the elements of this range except the first one.
   */
  final override def tail: Range = {
    if (isEmpty) throw Range.emptyRangeError("tail")
    if (numRangeElements == 1) newEmptyRange(end)
    else if(isInclusive) new Range.Inclusive(start + step, end, step)
    else new Range.Exclusive(start + step, end, step)
  }

  /** Builds a new indexed sequence by applying a function to all elements of
   *  this range.
   *
   *  @tparam B the element type of the returned collection
   *  @param f the function to apply to each element
   *  @return a new indexed sequence containing the results of applying `f` to
   *          each element of this range, in order
   *  @throws IllegalArgumentException if this range contains more than
   *          `Int.MaxValue` elements
   */
  override def map[B](f: Int => B): IndexedSeq[B] = {
    validateMaxLength()
    super.map(f)
  }

  /** Creates a new range from the given values, each of which defaults to the
   *  corresponding value of this range.
   *
   *  @param start the start value of the new range
   *  @param end the end value of the new range
   *  @param step the step of the new range; must be non-zero
   *  @param isInclusive whether the new range includes its `end` value
   *  @return a new `Range.Inclusive` if `isInclusive` is `true`, otherwise
   *          a new `Range.Exclusive`
   */
  final protected def copy(start: Int = start, end: Int = end, step: Int = step, isInclusive: Boolean = isInclusive): Range =
    if(isInclusive) new Range.Inclusive(start, end, step) else new Range.Exclusive(start, end, step)

  /** Creates a new range with the `start` and `end` values of this range and
   *  a new `step`.
   *
   *  @param step the new step value for the range; must be non-zero
   *  @return a new range with a different step
   */
  final def by(step: Int): Range = copy(start, end, step)

  // Check cannot be evaluated eagerly because we have a pattern where
  // ranges are constructed like: "x to y by z" The "x to y" piece
  // should not trigger an exception. So the calculation is delayed,
  // which means it will not fail fast for those cases where failing was
  // correct.
  private[this] def validateMaxLength(): Unit = {
    if (numRangeElements < 0)
      fail()
  }
  private[this] def fail() = Range.fail(start, end, step, isInclusive)

  /** Returns the element at index `idx`, that is, `start + step * idx`, in
   *  constant time.
   *
   *  @param idx the index of the element to return
   *  @throws IndexOutOfBoundsException if `idx` is negative or not less than `length`
   *  @throws IllegalArgumentException if this range contains more than
   *          `Int.MaxValue` elements
   */
  @throws[IndexOutOfBoundsException]
  final def apply(idx: Int): Int = {
    validateMaxLength()
    if (idx < 0 || idx >= numRangeElements) throw new IndexOutOfBoundsException(s"$idx is out of bounds (min 0, max ${numRangeElements-1})")
    else start + (step * idx)
  }

  /*@`inline`*/ final override def foreach[@specialized(Unit) U](f: Int => U): Unit = {
    // Implementation chosen on the basis of favorable microbenchmarks
    // Note--initialization catches step == 0 so we don't need to here
    if (!isEmpty) {
      var i = start
      while (true) {
        f(i)
        if (i == lastElement) return
        i += step
      }
    }
  }

  /** Returns the index of the first occurrence of `elem` at or after index
   *  `from`, or `-1` if `elem` does not occur at or after that index.
   *
   *  When `elem` is an `Int`, its position is computed arithmetically in
   *  constant time instead of by searching; this is possible because each
   *  value occurs at most once in a range.
   *
   *  @param elem the element to search for
   *  @param from the start index for the search
   */
  override final def indexOf[@specialized(Int) B >: Int](elem: B, from: Int = 0): Int =
    elem match {
      case i: Int =>
        val pos = posOf(i)
        if (pos >= from) pos else -1
      case _ => super.indexOf(elem, from)
    }

  /** Returns the index of the last occurrence of `elem` at or before index
   *  `end`, or `-1` if `elem` does not occur at or before that index.
   *
   *  When `elem` is an `Int`, its position is computed arithmetically in
   *  constant time instead of by searching; this is possible because each
   *  value occurs at most once in a range.
   *
   *  @param elem the element to search for
   *  @param end the end index for the search
   *  @throws IllegalArgumentException if this range contains more than
   *          `Int.MaxValue` elements
   */
  override final def lastIndexOf[@specialized(Int) B >: Int](elem: B, end: Int = length - 1): Int =
    elem match {
      case i: Int =>
        val pos = posOf(i)
        if (pos <= end) pos else -1
      case _ => super.lastIndexOf(elem, end)
    }

  private[this] def posOf(i: Int): Int =
    if (contains(i)) (i - start) / step else -1

  /** Returns `true` if this range contains the same elements as `that`, in
   *  the same order.
   *
   *  When `that` is also a `Range`, the answer is computed in constant time
   *  from the two ranges' lengths, starts, and steps, without iterating.
   *
   *  @tparam B the element type of `that`
   *  @param that the collection to compare with
   *  @throws IllegalArgumentException if this range contains more than
   *          `Int.MaxValue` elements
   */
  override def sameElements[B >: Int](that: IterableOnce[B]): Boolean = that match {
    case other: Range =>
      (this.length : @annotation.switch) match {
        case 0 => other.isEmpty
        case 1 => other.length == 1 && this.start == other.start
        case n => other.length == n && (
          (this.start == other.start)
            && (this.step == other.step)
        )
      }
    case _ => super.sameElements(that)
  }

  /** Creates a new range containing the first `n` elements of this range.
   *
   *  @param n  the number of elements to take.
   *  @return   a new range consisting of `n` first elements.
   */
  final override def take(n: Int): Range =
    if (n <= 0 || isEmpty) newEmptyRange(start)
    else if (n >= numRangeElements && numRangeElements >= 0) this
    else {
      // May have more than Int.MaxValue elements in range (numRangeElements < 0)
      // but the logic is the same either way: take the first n
      new Range.Inclusive(start, locationAfterN(n - 1), step)
    }

  /** Creates a new range containing all the elements of this range except the first `n` elements.
   *
   *  @param n  the number of elements to drop.
   *  @return   a new range consisting of all the elements of this range except `n` first elements.
   */
  final override def drop(n: Int): Range =
    if (n <= 0 || isEmpty) this
    else if (n >= numRangeElements && numRangeElements >= 0) newEmptyRange(end)
    else {
      // May have more than Int.MaxValue elements (numRangeElements < 0)
      // but the logic is the same either way: go forwards n steps, keep the rest
      copy(locationAfterN(n), end, step)
    }

  /** Creates a new range consisting of the last `n` elements of the range.
   *
   *  $doesNotUseBuilders
   *
   *  @param n the number of elements to take from the end of this range
   *  @return a new range consisting of the last `n` elements, or the entire range if `n` is greater than the range length
   */
  final override def takeRight(n: Int): Range = {
    if (n <= 0) newEmptyRange(start)
    else if (numRangeElements >= 0) drop(numRangeElements - n)
    else {
      // Need to handle over-full range separately
      val y = last
      val x = y - step.toLong*(n-1)
      if ((step > 0 && x < start) || (step < 0 && x > start)) this
      else Range.inclusive(x.toInt, y, step)
    }
  }

  /** Creates a new range consisting of the initial `length - n` elements of the range.
   *
   *  $doesNotUseBuilders
   *
   *  @param n the number of elements to drop from the end of this range
   *  @return a new range consisting of all elements except the last `n`, or an empty range if `n` is greater than the range length
   */
  final override def dropRight(n: Int): Range = {
    if (n <= 0) this
    else if (numRangeElements >= 0) take(numRangeElements - n)
    else {
      // Need to handle over-full range separately
      val y = last - step.toInt*n
      if ((step > 0 && y < start) || (step < 0 && y > start)) newEmptyRange(start)
      else Range.inclusive(start, y.toInt, step)
    }
  }

  // Advance from the start while we meet the given test
  private[this] def argTakeWhile(p: Int => Boolean): Long = {
    if (isEmpty) start
    else {
      var current = start
      val stop = last
      while (current != stop && p(current)) current += step
      if (current != stop || !p(current)) current
      else current.toLong + step
    }
  }

  /** Returns the longest prefix of this range whose elements all satisfy `p`.
   *
   *  The result is itself a range; the predicate is evaluated on successive
   *  elements until it first fails.
   *
   *  @param p the predicate used to test elements
   */
  final override def takeWhile(p: Int => Boolean): Range = {
    val stop = argTakeWhile(p)
    if (stop==start) newEmptyRange(start)
    else {
      val x = (stop - step).toInt
      if (x == last) this
      else Range.inclusive(start, x, step)
    }
  }

  /** Returns the remainder of this range after the longest prefix whose
   *  elements all satisfy `p`.
   *
   *  The result is itself a range; the predicate is evaluated on successive
   *  elements until it first fails.
   *
   *  @param p the predicate used to test elements
   */
  final override def dropWhile(p: Int => Boolean): Range = {
    val stop = argTakeWhile(p)
    if (stop == start) this
    else {
      val x = (stop - step).toInt
      if (x == last) newEmptyRange(last)
      else Range.inclusive(x + step, last, step)
    }
  }

  /** Splits this range into the longest prefix whose elements all satisfy `p`
   *  and the remainder.
   *
   *  Equivalent to `(takeWhile(p), dropWhile(p))`, but more efficient: the
   *  predicate is evaluated at most once per element.
   *
   *  @param p the predicate used to test elements
   *  @return a pair of ranges `(this.takeWhile(p), this.dropWhile(p))`
   */
  final override def span(p: Int => Boolean): (Range, Range) = {
    val border = argTakeWhile(p)
    if (border == start) (newEmptyRange(start), this)
    else {
      val x = (border - step).toInt
      if (x == last) (this, newEmptyRange(last))
      else (Range.inclusive(start, x, step), Range.inclusive(x+step, last, step))
    }
  }

  /** Creates a new range containing the elements starting at `from` up to but not including `until`.
   *
   *  $doesNotUseBuilders
   *
   *  @param from  the element at which to start
   *  @param until  the element at which to end (not included in the range)
   *  @return   a new range consisting of a contiguous interval of values in the old range
   */
  final override def slice(from: Int, until: Int): Range =
    if (from <= 0) take(until)
    else if (until >= numRangeElements && numRangeElements >= 0) drop(from)
    else {
      val fromValue = locationAfterN(from)
      if (from >= until) newEmptyRange(fromValue)
      else Range.inclusive(fromValue, locationAfterN(until-1), step)
    }

  // Overridden only to refine the return type
  /** Splits this range into a prefix/suffix pair at a given index.
   *
   *  $doesNotUseBuilders
   *
   *  @param n the index at which to split
   *  @return a pair of ranges `(this.take(n), this.drop(n))`
   */
  final override def splitAt(n: Int): (Range, Range) = (take(n), drop(n))

  // Methods like apply throw exceptions on invalid n, but methods like take/drop
  // are forgiving: therefore the checks are with the methods.
  private[this] def locationAfterN(n: Int) = start + (step * n)

  // When one drops everything.  Can't ever have unchecked operations
  // like "end + 1" or "end - 1" because ranges involving Int.{ MinValue, MaxValue }
  // will overflow.  This creates an exclusive range where start == end
  // based on the given value.
  private[this] def newEmptyRange(value: Int) = new Range.Exclusive(value, value, step)

  /** Returns the reverse of this range. */
  final override def reverse: Range =
    if (isEmpty) this
    else new Range.Inclusive(last, start, -step)

  /** Makes range inclusive. */
  final def inclusive: Range =
    if (isInclusive) this
    else new Range.Inclusive(start, end, step)

  /** Returns `true` if `x` is an element of this range.
   *
   *  This is a constant-time operation: membership is decided with a bounds
   *  check and a divisibility test rather than a search.
   *
   *  @param x the value to test for membership
   */
  final def contains(x: Int) = {
    if (x == end && !isInclusive) false
    else if (step > 0) {
      if (x < start || x > end) false
      else (step == 1) || (Integer.remainderUnsigned(x - start, step) == 0)
    }
    else {
      if (x < end || x > start) false
      else (step == -1) || (Integer.remainderUnsigned(start - x, -step) == 0)
    }
  }
  /* Seq#contains has a type parameter so the optimised contains above doesn't override it */
  /** Returns `true` if `elem` is an element of this range.
   *
   *  When `elem` is an `Int`, this delegates to the constant-time
   *  `contains(x: Int)` overload; otherwise it falls back to a linear search.
   *
   *  @tparam B the type of `elem`
   *  @param elem the value to test for membership
   */
  override final def contains[B >: Int](elem: B): Boolean = elem match {
    case i: Int => this.contains(i)
    case _      => super.contains(elem)
  }

  /** Returns the sum of the elements of this range.
   *
   *  For the default `Int` numeric, the sum is computed in constant time with
   *  the arithmetic-series formula, and the result wraps around on overflow
   *  exactly as repeated `Int` addition would. For any other `Numeric`, the
   *  elements are added one by one and the result is converted back to `Int`
   *  with `num.toInt`.
   *
   *  @tparam B a supertype of `Int` for which the addition is defined
   *  @param num the numeric instance used to add elements
   *  @return the sum of all elements, or zero if this range is empty
   *  @throws IllegalArgumentException if this range contains more than
   *          `Int.MaxValue` elements
   */
  final override def sum[B >: Int](implicit num: Numeric[B]): Int = {
    if (num eq scala.math.Numeric.IntIsIntegral) {
      // this is normal integer range with usual addition. arithmetic series formula can be used
      if (isEmpty) 0
      else if (size == 1) head
      else ((size * (head.toLong + last)) / 2).toInt
    } else {
      // user provided custom Numeric, we cannot rely on arithmetic series formula
      if (isEmpty) num.toInt(num.zero)
      else {
        var acc = num.zero
        var i = head
        while (true) {
          acc = num.plus(acc, i)
          if (i == lastElement) return num.toInt(acc)
          i = i + step
        }
        0 // Never hit this--just to satisfy compiler since it doesn't know while(true) has type Nothing
      }
    }
  }

  /** Returns the smallest element of this range.
   *
   *  For the standard `Int` ordering or its reverse, the result is `head` or
   *  `last`, depending on the ordering and the sign of `step`, found in
   *  constant time; for any other ordering, the elements are scanned.
   *
   *  @tparam A1 a supertype of `Int` over which `ord` compares
   *  @param ord the ordering used to compare elements
   *  @return the smallest element of this range with respect to `ord`
   *  @throws NoSuchElementException if this range is empty and `ord` is the
   *          `Int` ordering or its reverse
   *  @throws UnsupportedOperationException if this range is empty and `ord` is
   *          any other ordering
   */
  final override def min[A1 >: Int](implicit ord: Ordering[A1]): Int =
    if (ord eq Ordering.Int) {
      if (step > 0) head
      else last
    } else if (Ordering.Int isReverseOf ord) {
      if (step > 0) last
      else head
    } else super.min(ord)

  /** Returns the largest element of this range.
   *
   *  For the standard `Int` ordering or its reverse, the result is `head` or
   *  `last`, depending on the ordering and the sign of `step`, found in
   *  constant time; for any other ordering, the elements are scanned.
   *
   *  @tparam A1 a supertype of `Int` over which `ord` compares
   *  @param ord the ordering used to compare elements
   *  @return the largest element of this range with respect to `ord`
   *  @throws NoSuchElementException if this range is empty and `ord` is the
   *          `Int` ordering or its reverse
   *  @throws UnsupportedOperationException if this range is empty and `ord` is
   *          any other ordering
   */
  final override def max[A1 >: Int](implicit ord: Ordering[A1]): Int =
    if (ord eq Ordering.Int) {
      if (step > 0) last
      else head
    } else if (Ordering.Int isReverseOf ord) {
      if (step > 0) head
      else last
    } else super.max(ord)

  /** Returns an iterator over all the tails of this range, starting with this
   *  range itself and ending with an empty range.
   *
   *  Each tail is itself a range, produced in constant time by `drop`.
   *  @throws IllegalArgumentException if this range contains more than
   *          `Int.MaxValue` elements
   */
  override def tails: Iterator[Range] =
    new AbstractIterator[Range] {
      private[this] var i = 0
      override def hasNext = i <= Range.this.length
      override def next() = {
        if (hasNext) {
          val res = Range.this.drop(i)
          i += 1
          res
        } else {
          Iterator.empty.next()
        }
      }
    }

  /** Returns an iterator over all the inits of this range, starting with this
   *  range itself and ending with an empty range.
   *
   *  Each init is itself a range, produced in constant time by `dropRight`.
   *  @throws IllegalArgumentException if this range contains more than
   *          `Int.MaxValue` elements
   */
  override def inits: Iterator[Range] =
    new AbstractIterator[Range] {
      private[this] var i = 0
      override def hasNext = i <= Range.this.length
      override def next() = {
        if (hasNext) {
          val res = Range.this.dropRight(i)
          i += 1
          res
        } else {
          Iterator.empty.next()
        }
      }
    }
  /** The maximum length below which indexed access via `apply` is preferred
   *  over `iterator` when scanning elements; always `Int.MaxValue` because
   *  `apply` is constant-time for ranges.
   */
  override protected final def applyPreferredMaxLength: Int = Int.MaxValue

  /** Returns `true` if `other` is equal to this range.
   *
   *  Another `Range` is equal to this one if both are empty, or if both have
   *  the same `start` and last element and, unless they contain a single
   *  element, the same `step`; this is decided in constant time and works even
   *  for ranges of more than `Int.MaxValue` elements. Any other object is
   *  compared with generic sequence equality.
   *
   *  @param other the object to compare with
   */
  final override def equals(other: Any) = other match {
    case x: Range =>
      // Note: this must succeed for overfull ranges (length > Int.MaxValue)
      if (isEmpty) x.isEmpty                  // empty sequences are equal
      else                                    // this is non-empty...
        x.nonEmpty && start == x.start && {   // ...so other must contain something and have same start
          val l0 = last
          (l0 == x.last && (                    // And same end
            start == l0 || step == x.step       // And either the same step, or not take any steps
          ))
        }
    case _ =>
      super.equals(other)
  }

  /** Returns a hash code value consistent with `equals`.
   *
   *  For ranges of two or more elements, the hash is computed directly from
   *  `start`, `step`, and the last element; smaller ranges use the generic
   *  sequence hash.
   *
   *  @throws IllegalArgumentException if this range contains more than
   *          `Int.MaxValue` elements
   */
  final override def hashCode: Int =
    if(length >= 2) MurmurHash3.rangeHash(start, step, lastElement)
    else super.hashCode

  /** Returns a string representation of this range, such as
   *  `Range 0 until 10` or `Range 1 to 9 by 2`.
   *
   *  The `by` clause is omitted when `step` is `1`. The prefix `empty ` marks
   *  an empty range, and the prefix `inexact ` marks a non-empty range whose
   *  `end` is not an exact multiple of `step` away from `start`.
   */
  final override def toString: String = {
    val preposition = if (isInclusive) "to" else "until"
    val stepped = if (step == 1) "" else s" by $step"
    val prefix = if (isEmpty) "empty " else if (!isExact) "inexact " else ""
    s"${prefix}Range $start $preposition $end$stepped"
  }

  override protected[this] def className = "Range"

  /** Returns this range: the elements of a range are always pairwise distinct. */
  override def distinct: Range = this

  /** Partitions the elements of this range into fixed-size groups.
   *
   *  Each group is itself a range, produced in constant time by `slice`.
   *
   *  @param size the number of elements per group
   *  @return an iterator over the groups; every group has `size` elements,
   *          except possibly the last, which may have fewer
   *  @throws IllegalArgumentException if `size` is less than `1`
   *  @throws IllegalArgumentException if this range contains more than
   *          `Int.MaxValue` elements
   */
  override def grouped(size: Int): Iterator[Range] = {
    require(size >= 1, f"size=$size%d, but size must be positive")
    if (isEmpty) {
      Iterator.empty
    } else {
      val s = size
      new AbstractIterator[Range] {
        private[this] var i = 0
        override def hasNext = Range.this.length > i
        override def next() =
          if (hasNext) {
            val x = Range.this.slice(i, i + s)
            i += s
            x
          } else {
            Iterator.empty.next()
          }
      }
    }
  }

  /** Returns the elements of this range in sorted order according to an
   *  ordering.
   *
   *  For the standard `Int` ordering, the result is this range itself when
   *  `step` is positive, or its `reverse` when `step` is negative, computed in
   *  constant time; for any other ordering, a generic sort is performed.
   *
   *  @tparam B a supertype of `Int` over which `ord` compares
   *  @param ord the ordering used to compare elements
   *  @return an indexed sequence containing the elements of this range, sorted
   */
  override def sorted[B >: Int](implicit ord: Ordering[B]): IndexedSeq[Int] =
    if (ord eq Ordering.Int) {
      if (step > 0) {
        this
      } else {
        reverse
      }
    } else {
      super.sorted(ord)
    }
}

/** Companion object for ranges.
 *  @define Coll `Range`
 *  @define coll range
 */
object Range {

  private def description(start: Int, end: Int, step: Int, isInclusive: Boolean) =
    start + (if (isInclusive) " to " else " until ") + end + " by " + step

  private def fail(start: Int, end: Int, step: Int, isInclusive: Boolean) =
    throw new IllegalArgumentException(description(start, end, step, isInclusive) +
        ": seqs cannot contain more than Int.MaxValue elements.")

  /** Counts the number of range elements.
   *  precondition:  step != 0
   *  If the size of the range exceeds Int.MaxValue, the
   *  result will be negative.
   *
   *  @param start the first element of the range
   *  @param end the end boundary of the range (inclusive or exclusive depending on `isInclusive`)
   *  @param step the increment between successive elements; must be non-zero
   *  @param isInclusive whether `end` is included in the range
   *  @return the number of elements in the range, or `-1` if the count exceeds `Int.MaxValue`
   */
  def count(start: Int, end: Int, step: Int, isInclusive: Boolean): Int = {
    if (step == 0)
      throw new IllegalArgumentException("step cannot be 0.")

    val isEmpty =
      if (start == end) !isInclusive
      else if (start < end) step < 0
      else step > 0

    if (isEmpty) 0
    else {
      // Counts with Longs so we can recognize too-large ranges.
      val gap: Long    = end.toLong - start.toLong
      val jumps: Long  = gap / step
      // Whether the size of this range is one larger than the
      // number of full-sized jumps.
      val hasStub      = isInclusive || (gap % step != 0)
      val result: Long = jumps + ( if (hasStub) 1 else 0 )

      if (result > scala.Int.MaxValue) -1
      else result.toInt
    }
  }
  /** Counts the number of elements of an exclusive range with the given
   *  start, end, and step.
   *
   *  Equivalent to `count(start, end, step, isInclusive = false)`.
   *
   *  @param start the first element of the range
   *  @param end the exclusive upper bound of the range
   *  @param step the increment between successive elements; must be non-zero
   *  @return the number of elements in the range, `0` if the range is empty,
   *          or `-1` if the count exceeds `Int.MaxValue`
   *  @throws IllegalArgumentException if `step` is `0`
   */
  def count(start: Int, end: Int, step: Int): Int =
    count(start, end, step, isInclusive = false)

  /** Makes a range from `start` until `end` (exclusive) with given step value.
   *  @note step != 0
   *
   *  @param start the first element of the range
   *  @param end the exclusive upper bound of the range
   *  @param step the increment between successive elements; must be non-zero
   *  @return an exclusive `Range` from `start` to `end` stepping by `step`
   */
  def apply(start: Int, end: Int, step: Int): Range.Exclusive = new Range.Exclusive(start, end, step)

  /** Makes a range from `start` until `end` (exclusive) with step value 1.
   *
   *  @param start the first element of the range
   *  @param end the exclusive upper bound of the range
   *  @return an exclusive `Range` from `start` to `end` with step `1`
   */
  def apply(start: Int, end: Int): Range.Exclusive = new Range.Exclusive(start, end, 1)

  /** Makes an inclusive range from `start` to `end` with given step value.
   *  @note step != 0
   *
   *  @param start the first element of the range
   *  @param end the inclusive upper bound of the range
   *  @param step the increment between successive elements; must be non-zero
   *  @return an inclusive `Range` from `start` to `end` stepping by `step`
   */
  def inclusive(start: Int, end: Int, step: Int): Range.Inclusive = new Range.Inclusive(start, end, step)

  /** Makes an inclusive range from `start` to `end` with step value 1.
   *
   *  @param start the first element of the range
   *  @param end the inclusive upper bound of the range
   *  @return an inclusive `Range` from `start` to `end` with step `1`
   */
  def inclusive(start: Int, end: Int): Range.Inclusive = new Range.Inclusive(start, end, 1)

  /** A `Range` that includes its `end` value, as built by `to` or
   *  `Range.inclusive`.
   *
   *  `end` is an element of the range only if it is reachable from `start` in
   *  `step` increments.
   *
   *  @param start the start value of the range
   *  @param end the inclusive end value of the range
   *  @param step the step value between consecutive elements, must be non-zero
   */
  @SerialVersionUID(3L)
  @inline
  final class Inclusive(start: Int, end: Int, step: Int) extends Range(start, end, step) {
    /** Returns `true`: this range includes its `end` value. */
    def isInclusive = true
  }

  /** A `Range` that excludes its `end` value, as built by `until` or
   *  `Range.apply`.
   *
   *  @param start the start value of the range
   *  @param end the exclusive end value of the range
   *  @param step the step value between consecutive elements, must be non-zero
   */
  @SerialVersionUID(3L)
  @inline
  final class Exclusive(start: Int, end: Int, step: Int) extends Range(start, end, step) {
    /** Returns `false`: this range excludes its `end` value. */
    def isInclusive = false
  }

  // BigInt and Long are straightforward generic ranges.
  object BigInt {
    /** Creates an exclusive range of `BigInt` values.
     *
     *  @param start the start value of the range
     *  @param end the exclusive end value of the range
     *  @param step the step value between consecutive elements, must be non-zero
     *  @return an exclusive `NumericRange` from `start` until `end` in increments of `step`
     */
    def apply(start: BigInt, end: BigInt, step: BigInt) = NumericRange(start, end, step)
    /** Creates an inclusive range of `BigInt` values.
     *
     *  @param start the start value of the range
     *  @param end the inclusive end value of the range
     *  @param step the step value between consecutive elements, must be non-zero
     *  @return an inclusive `NumericRange` from `start` to `end` in increments of `step`
     */
    def inclusive(start: BigInt, end: BigInt, step: BigInt) = NumericRange.inclusive(start, end, step)
  }

  object Long {
    /** Creates an exclusive range of `Long` values.
     *
     *  @param start the start value of the range
     *  @param end the exclusive end value of the range
     *  @param step the step value between consecutive elements, must be non-zero
     *  @return an exclusive `NumericRange` from `start` until `end` in increments of `step`
     */
    def apply(start: Long, end: Long, step: Long) = NumericRange(start, end, step)
    /** Creates an inclusive range of `Long` values.
     *
     *  @param start the start value of the range
     *  @param end the inclusive end value of the range
     *  @param step the step value between consecutive elements, must be non-zero
     *  @return an inclusive `NumericRange` from `start` to `end` in increments of `step`
     */
    def inclusive(start: Long, end: Long, step: Long) = NumericRange.inclusive(start, end, step)
  }

  // BigDecimal uses an alternative implementation of Numeric in which
  // it pretends to be Integral[T] instead of Fractional[T].  See Numeric for
  // details.  The intention is for it to throw an exception anytime
  // imprecision or surprises might result from anything, although this may
  // not yet be fully implemented.
  object BigDecimal {
    /** The implicit numeric instance used to build `BigDecimal` ranges; it
     *  treats `BigDecimal` as if it were an integral type (see
     *  `scala.math.Numeric.BigDecimalAsIfIntegral`).
     */
    implicit val bigDecAsIntegral: Numeric.BigDecimalAsIfIntegral = Numeric.BigDecimalAsIfIntegral

    /** Creates an exclusive range of `BigDecimal` values.
     *
     *  @param start the start value of the range
     *  @param end the exclusive end value of the range
     *  @param step the step value between consecutive elements, must be non-zero
     *  @return an exclusive `NumericRange` from `start` until `end` in increments of `step`
     */
    def apply(start: BigDecimal, end: BigDecimal, step: BigDecimal) =
      NumericRange(start, end, step)
    /** Creates an inclusive range of `BigDecimal` values.
     *
     *  @param start the start value of the range
     *  @param end the inclusive end value of the range
     *  @param step the step value between consecutive elements, must be non-zero
     *  @return an inclusive `NumericRange` from `start` to `end` in increments of `step`
     */
    def inclusive(start: BigDecimal, end: BigDecimal, step: BigDecimal) =
      NumericRange.inclusive(start, end, step)
  }

  // As there is no appealing default step size for not-really-integral ranges,
  // we offer a partially constructed object.
  /** A partially constructed range that still lacks its step value.
   *
   *  Range constructions with no sensible default step, such as `until` and
   *  `to` on `BigDecimal`, return a `Partial`; calling `by` supplies the step
   *  and yields the finished range.
   *
   *  @tparam T the type of the step
   *  @tparam U the type of the completed range
   *  @param f the function that builds the completed range from a step value
   */
  class Partial[T, U](private val f: T => U) extends AnyVal {
    /** Completes this partially constructed range with the given step value.
     *
     *  @param x the step value
     *  @return the completed range, `f(x)`
     */
    def by(x: T): U = f(x)
    /** Returns the string `"Range requires step"`, indicating that this is not yet a complete range. */
    override def toString = "Range requires step"
  }

  // Illustrating genericity with Int Range, which should have the same behavior
  // as the original Range class.  However we leave the original Range
  // indefinitely, for performance and because the compiler seems to bootstrap
  // off it and won't do so with our parameterized version without modifications.
  object Int {
    /** Creates an exclusive `NumericRange` of `Int` values, a generic
     *  alternative to `Range` with the same behavior.
     *
     *  @param start the start value of the range
     *  @param end the exclusive end value of the range
     *  @param step the step value between consecutive elements, must be non-zero
     *  @return an exclusive `NumericRange` from `start` until `end` in increments of `step`
     */
    def apply(start: Int, end: Int, step: Int) = NumericRange(start, end, step)
    /** Creates an inclusive `NumericRange` of `Int` values, a generic
     *  alternative to `Range` with the same behavior.
     *
     *  @param start the start value of the range
     *  @param end the inclusive end value of the range
     *  @param step the step value between consecutive elements, must be non-zero
     *  @return an inclusive `NumericRange` from `start` to `end` in increments of `step`
     */
    def inclusive(start: Int, end: Int, step: Int) = NumericRange.inclusive(start, end, step)
  }

  private def emptyRangeError(what: String): Throwable =
    new NoSuchElementException(what + " on empty Range")
}

/**
 *  @param lastElement The last element included in the Range
 *  @param initiallyEmpty Whether the Range was initially empty or not
 */
@SerialVersionUID(3L)
private class RangeIterator(
  start: Int,
  step: Int,
  lastElement: Int,
  initiallyEmpty: Boolean
) extends AbstractIterator[Int] with Serializable {
  private[this] var _hasNext: Boolean = !initiallyEmpty
  private[this] var _next: Int = start
  override def knownSize: Int = if (_hasNext) (lastElement - _next) / step + 1 else 0
  def hasNext: Boolean = _hasNext
  @throws[NoSuchElementException]
  def next(): Int = {
    if (!_hasNext) Iterator.empty.next()
    val value = _next
    _hasNext = value != lastElement
    _next = value + step
    value
  }

  override def drop(n: Int): Iterator[Int] = {
    if (n > 0) {
      val longPos = _next.toLong + step * n
      if (step > 0) {
        _next = Math.min(lastElement, longPos).toInt
        _hasNext = longPos <= lastElement
      }
      else if (step < 0) {
        _next = Math.max(lastElement, longPos).toInt
        _hasNext = longPos >= lastElement
      }
    }
      this
  }
}
