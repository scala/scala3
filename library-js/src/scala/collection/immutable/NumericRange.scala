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

package scala.collection.immutable

import scala.language.`2.13`

import scala.collection.Stepper.EfficientSplit
import scala.collection.{AbstractIterator, AnyStepper, IterableFactoryDefaults, Iterator, Stepper, StepperShape}

/** `NumericRange` is a more generic version of the
 *  `Range` class which works with arbitrary types.
 *  It must be supplied with an `Integral` implementation of the
 *  range type.
 *
 *  Factories for likely types include `Range.BigInt`, `Range.Long`,
 *  and `Range.BigDecimal`.  `Range.Int` exists for completeness, but
 *  the `Int`-based `scala.Range` should be more performant.
 *
 *  ```
 *     val r1 = Range(0, 100, 1)
 *     val veryBig = Int.MaxValue.toLong + 1
 *     val r2 = Range.Long(veryBig, veryBig + 100, 1)
 *     assert(r1 sameElements r2.map(_ - veryBig))
 *  ```
 *
 *  @define Coll `NumericRange`
 *  @define coll numeric range
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
@SerialVersionUID(3L)
sealed class NumericRange[T](
  /** The start value of the range; its first element when the range is non-empty. */
  val start: T,
  /** The end value of the range, a bound that is not necessarily an element itself. */
  val end: T,
  /** The increment between successive elements; may be negative, must not be zero. */
  val step: T,
  /** Whether `end` may be an element: an inclusive range contains `end` when it
   *  is a whole number of steps from `start`; an exclusive range never contains it.
   */
  val isInclusive: Boolean
)(implicit
  num: Integral[T]
)
  extends AbstractSeq[T]
    with IndexedSeq[T]
    with IndexedSeqOps[T, IndexedSeq, IndexedSeq[T]]
    with StrictOptimizedSeqOps[T, IndexedSeq, IndexedSeq[T]]
    with IterableFactoryDefaults[T, IndexedSeq]
    with Serializable { self =>

  /** Returns a new iterator over all elements of this range in order. */
  override def iterator: Iterator[T] = new NumericRange.NumericRangeIterator(this, num)

  /** Returns a stepper for the elements of this range.
   *
   *  Uses a stepper specialized for `Int` or `Long` elements when `shape` has
   *  one of those shapes, and a generic stepper otherwise.
   *
   *  @tparam S the type of the returned stepper, determined by `shape`
   *  @param shape the shape of the stepper for elements of type `T`
   *  @return a stepper over the elements of this range, supporting efficient
   *          splitting for parallel processing
   */
  override def stepper[S <: Stepper[_]](implicit shape: StepperShape[T, S]): S with EfficientSplit = {
    import scala.collection.convert._
    import impl._
    val s = shape.shape match {
      case StepperShape.IntShape    => new IntNumericRangeStepper   (this.asInstanceOf[NumericRange[Int]],    0, length)
      case StepperShape.LongShape   => new LongNumericRangeStepper  (this.asInstanceOf[NumericRange[Long]],   0, length)
      case _         => shape.parUnbox(new AnyNumericRangeStepper[T](this, 0, length).asInstanceOf[AnyStepper[T] with EfficientSplit])
    }
    s.asInstanceOf[S with EfficientSplit]
  }


  /** Note that NumericRange must be invariant so that constructs
   *  such as "1L to 10 by 5" do not infer the range type as AnyVal.
   */
  import num._

  // See comment in Range for why this must be lazy.
  /** The number of elements in this range, computed on first access and cached.
   *
   *  @throws IllegalArgumentException if `step` is zero, if the range has more than
   *          `Int.MaxValue` elements, or, for a `BigDecimal` range, if the endpoints
   *          cannot represent the step accurately enough to count the elements
   */
  override lazy val length: Int     = NumericRange.count(start, end, step, isInclusive)
  /** Whether this range has no elements.
   *
   *  Determined from `start`, `end`, the sign of `step`, and `isInclusive`
   *  alone, so this is safe even on ranges whose `length` would overflow an
   *  `Int`.
   */
  override lazy val isEmpty: Boolean = (
    (num.gt(start, end) && num.gt(step, num.zero))
      || (num.lt(start, end) && num.lt(step, num.zero))
      || (num.equiv(start, end) && !isInclusive)
    )
  /** Returns the last element of this range: the element nearest `end` that
   *  the range contains, which is not necessarily `end` itself.
   *
   *  @throws NoSuchElementException if this range is empty
   */
  override def last: T =
    if (isEmpty) Nil.head
    else locationAfterN(length - 1)
  /** Returns a new range with all elements of this range except the last.
   *
   *  @throws UnsupportedOperationException if this range is empty
   */
  override def init: NumericRange[T] =
    if (isEmpty) Nil.init
    else new NumericRange(start, end - step, step, isInclusive)

  /** Returns the first element of this range, which is `start`.
   *
   *  @throws NoSuchElementException if this range is empty
   */
  override def head: T = if (isEmpty) Nil.head else start
  /** Returns a new range with all elements of this range except the first.
   *
   *  @throws UnsupportedOperationException if this range is empty
   */
  override def tail: NumericRange[T] =
    if (isEmpty) Nil.tail
    else if(isInclusive) new NumericRange.Inclusive(start + step, end, step)
    else new NumericRange.Exclusive(start + step, end, step)

  /** Creates a new range with the start and end values of this range and
   *  a new `step`.
   *
   *  @param newStep the new step value for the resulting range
   *  @return a new `NumericRange` with the same `start` and `end` as this range but with `newStep` as its step
   */
  def by(newStep: T): NumericRange[T] = copy(start, end, newStep)


  /** Creates a copy of this range.
   *
   *  @param start the first value of the new range
   *  @param end the end boundary of the new range (inclusive or exclusive, matching this range)
   *  @param step the step value between successive elements of the new range
   *  @return a new `NumericRange` with the given `start`, `end`, and `step`, preserving this range's inclusivity
   */
  def copy(start: T, end: T, step: T): NumericRange[T] =
    new NumericRange(start, end, step, isInclusive)

  /** Returns the element at the given index, computed as `start + idx * step`.
   *
   *  @param idx the zero-based index of the element
   *  @return the element at index `idx`
   *  @throws IndexOutOfBoundsException if `idx` is negative or not less than `length`
   */
  @throws[IndexOutOfBoundsException]
  def apply(idx: Int): T = {
    if (idx < 0 || idx >= length) throw new IndexOutOfBoundsException(s"$idx is out of bounds (min 0, max ${length - 1})")
    else locationAfterN(idx)
  }

  /** Applies `f` to every element of this range in order.
   *
   *  @param f the function applied to each element; its result is discarded
   */
  override def foreach[@specialized(Specializable.Unit) U](f: T => U): Unit = {
    var count = 0
    var current = start
    while (count < length) {
      f(current)
      current += step
      count += 1
    }
  }

  // TODO: these private methods are straight copies from Range, duplicated
  // to guard against any (most likely illusory) performance drop.  They should
  // be eliminated one way or another.

  // Tests whether a number is within the endpoints, without testing
  // whether it is a member of the sequence (i.e. when step > 1.)
  private def isWithinBoundaries(elem: T) = !isEmpty && (
    (step > zero && start <= elem && elem <= last ) ||
      (step < zero &&  last <= elem && elem <= start)
    )
  // Methods like apply throw exceptions on invalid n, but methods like take/drop
  // are forgiving: therefore the checks are with the methods.
  private def locationAfterN(n: Int): T = start + (step * fromInt(n))

  private def crossesTheEndAfterN(n: Int): Boolean = {
    // if we're sure that subtraction in the context of T won't overflow, we use this function
    // to calculate the length of the range
    def unsafeRangeLength(r: NumericRange[T]): T = {
      val diff = num.minus(r.end, r.start)
      val quotient = num.quot(diff, r.step)
      val remainder = num.rem(diff, r.step)
      if (!r.isInclusive && num.equiv(remainder, num.zero))
        num.max(quotient, num.zero)
      else
        num.max(num.plus(quotient, num.one), num.zero)
    }

    // detects whether value can survive a bidirectional trip to -and then from- Int.
    def fitsInInteger(value: T): Boolean = num.equiv(num.fromInt(num.toInt(value)), value)

    val stepIsInTheSameDirectionAsStartToEndVector =
      (num.gt(end, start) && num.gt(step, num.zero)) || (num.lt(end, start) && num.sign(step) == -num.one)

    if (num.equiv(start, end) || n <= 0 || !stepIsInTheSameDirectionAsStartToEndVector) return n >= 1

    val sameSign = num.equiv(num.sign(start), num.sign(end))

    if (sameSign) { // subtraction is safe
      val len = unsafeRangeLength(this)
      if (fitsInInteger(len)) n >= num.toInt(len) else num.gteq(num.fromInt(n), len)
    } else {
      // split to two ranges, which subtraction is safe in both of them (around zero)
      val stepsRemainderToZero = num.rem(start, step)
      val walksOnZero = num.equiv(stepsRemainderToZero, num.zero)
      val closestToZero = if (walksOnZero) -step else stepsRemainderToZero

      /*
      When splitting into two ranges, we should be super-careful about one of the sides hitting MinValue of T,
      so we take two steps smaller than zero to ensure unsafeRangeLength won't overflow (taking one step may overflow depending on the step).
      Same thing happens for MaxValue from zero, so we take one step further to ensure the safety of unsafeRangeLength.
      After performing such operation, there are some elements remaining in between and around zero,
      which their length is represented by carry.
       */
      val (l: NumericRange[T], r: NumericRange[T], carry: Int) =
        if (num.lt(start, num.zero)) {
          if (walksOnZero) {
            val twoStepsAfterLargestNegativeNumber = num.plus(closestToZero, num.times(step, num.fromInt(2)))
            (NumericRange(start, closestToZero, step), copy(twoStepsAfterLargestNegativeNumber, end, step), 2)
          } else {
            (NumericRange(start, closestToZero, step), copy(num.plus(closestToZero, step), end, step), 1)
          }
        } else {
          if (walksOnZero) {
            val twoStepsAfterZero = num.times(step, num.fromInt(2))
            (copy(twoStepsAfterZero, end, step), NumericRange.inclusive(start, -step, step), 2)
          } else {
            val twoStepsAfterSmallestPositiveNumber = num.plus(closestToZero, num.times(step, num.fromInt(2)))
            (copy(twoStepsAfterSmallestPositiveNumber, end, step), NumericRange.inclusive(start, closestToZero, step), 2)
          }
        }

      val leftLength = unsafeRangeLength(l)
      val rightLength = unsafeRangeLength(r)

      // instead of `n >= rightLength + leftLength + curry` which may cause addition overflow,
      // this can be used `(n - leftLength - curry) >= rightLength` (Both in Int and T, depends on whether the lengths fit in Int)
      if (fitsInInteger(leftLength) && fitsInInteger(rightLength))
        n - num.toInt(leftLength) - carry >= num.toInt(rightLength)
      else
        num.gteq(num.minus(num.minus(num.fromInt(n), leftLength), num.fromInt(carry)), rightLength)
    }
  }

  // When one drops everything.  Can't ever have unchecked operations
  // like "end + 1" or "end - 1" because ranges involving Int.{ MinValue, MaxValue }
  // will overflow.  This creates an exclusive range where start == end
  // based on the given value.
  private def newEmptyRange(value: T) = NumericRange(value, value, step)

  /** Returns a range with the first `n` elements of this range.
   *
   *  @param n the number of elements to take
   *  @return a range of the first `n` elements of this range, this whole
   *          range if `n` is greater than or equal to `length`, or an empty
   *          range if `n <= 0`
   */
  override def take(n: Int): NumericRange[T] = {
    if (n <= 0 || isEmpty) newEmptyRange(start)
    else if (crossesTheEndAfterN(n)) this
    else new NumericRange.Inclusive(start, locationAfterN(n - 1), step)
  }

  /** Returns a range with all elements of this range except the first `n`.
   *
   *  @param n the number of elements to drop
   *  @return a range of the elements of this range after the first `n`, this
   *          whole range if `n <= 0`, or an empty range if `n` is greater
   *          than or equal to `length`
   */
  override def drop(n: Int): NumericRange[T] = {
    if (n <= 0 || isEmpty) this
    else if (crossesTheEndAfterN(n)) newEmptyRange(end)
    else copy(locationAfterN(n), end, step)
  }

  /** Splits this range at a given index.
   *
   *  @param n the index at which to split
   *  @return a pair of ranges `(take(n), drop(n))`
   */
  override def splitAt(n: Int): (NumericRange[T], NumericRange[T]) = (take(n), drop(n))

  /** Returns a new range with the same elements as this range in reverse
   *  order: an inclusive range from `last` to `start` with step `-step`.
   *  Returns this range itself if it is empty.
   *
   *  @throws ArithmeticException if negating the step cannot change its sign, either
   *          because the element type is unsigned or because the step is the minimum
   *          value of a signed fixed-width type
   */
  override def reverse: NumericRange[T] =
    if (isEmpty) this
    else {
      val newStep = -step
      if (num.sign(newStep) == num.sign(step)) {
        throw new ArithmeticException("number type is unsigned, and .reverse requires a negative step")
      } else new NumericRange.Inclusive(last, start, newStep)
    }

  import NumericRange.defaultOrdering

  /** Returns the smallest element of this range under the ordering `ord`.
   *
   *  When `ord` is this range's own `Integral` instance, or the default
   *  ordering of one of the standard fixed-width integral types (`Byte`,
   *  `Short`, `Char`, `Int`, or `Long`), the result is taken directly from an
   *  endpoint: the first element for a positive step, the last element
   *  otherwise. Any other ordering falls back to a linear scan.
   *
   *  @tparam T1 the type on which `ord` orders values, a supertype of `T`
   *  @param ord the ordering used to compare elements
   *  @return the smallest element of this range according to `ord`
   *  @throws NoSuchElementException if this range is empty and the endpoint
   *          shortcut applies
   *  @throws UnsupportedOperationException if this range is empty and the
   *          linear scan applies
   */
  override def min[T1 >: T](implicit ord: Ordering[T1]): T =
  // We can take the fast path:
  // - If the Integral of this NumericRange is also the requested Ordering
  //   (Integral <: Ordering). This can happen for custom Integral types.
  // - The Ordering is the default Ordering of a well-known Integral type.
    if ((ord eq num) || defaultOrdering.get(num).exists(ord eq _)) {
      if (num.sign(step) > zero) head
      else last
    } else super.min(ord)

  /** Returns the largest element of this range under the ordering `ord`.
   *
   *  When `ord` is this range's own `Integral` instance, or the default
   *  ordering of one of the standard fixed-width integral types (`Byte`,
   *  `Short`, `Char`, `Int`, or `Long`), the result is taken directly from an
   *  endpoint: the last element for a positive step, the first element
   *  otherwise. Any other ordering falls back to a linear scan.
   *
   *  @tparam T1 the type on which `ord` orders values, a supertype of `T`
   *  @param ord the ordering used to compare elements
   *  @return the largest element of this range according to `ord`
   *  @throws NoSuchElementException if this range is empty and the endpoint
   *          shortcut applies
   *  @throws UnsupportedOperationException if this range is empty and the
   *          linear scan applies
   */
  override def max[T1 >: T](implicit ord: Ordering[T1]): T =
  // See comment for fast path in min().
    if ((ord eq num) || defaultOrdering.get(num).exists(ord eq _)) {
      if (num.sign(step) > zero) last
      else head
    } else super.max(ord)

  // a well-typed contains method.
  /** Returns `true` if `x` is an element of this range, `false` otherwise.
   *
   *  Computed arithmetically, without examining elements: `x` must lie
   *  between `start` and `last` and be a whole number of steps from `start`.
   *
   *  @param x the value to test
   */
  def containsTyped(x: T): Boolean =
    isWithinBoundaries(x) && (((x - start) % step) == zero)

  /** Returns `true` if `x` is an element of this range, `false` otherwise.
   *
   *  Returns `false` if `x` is not a value of this range's element type;
   *  otherwise the answer is computed arithmetically as in `containsTyped`,
   *  without examining elements.
   *
   *  @tparam A1 the type of the value to test, a supertype of `T`
   *  @param x the value to test
   */
  override def contains[A1 >: T](x: A1): Boolean =
    try containsTyped(x.asInstanceOf[T])
    catch { case _: ClassCastException => false }

  /** Returns the sum of all elements of this range.
   *
   *  For the standard fixed-width integral types (`Byte`, `Short`, `Char`,
   *  `Int`, and `Long`) the sum is computed in constant time using the
   *  arithmetic series formula `n * (head + last) / 2`, with care taken not
   *  to overflow intermediate results. For any other `Numeric` instance the
   *  elements are added one by one.
   *
   *  @tparam B the type in which the sum is computed, a supertype of `T`
   *  @param num the `Numeric` instance used to add elements; its identity
   *         selects the constant-time specializations
   *  @return the sum of all elements, or `num.zero` if this range is empty
   */
  override def sum[B >: T](implicit num: Numeric[B]): B = {
    if (isEmpty) num.zero
    else if (size == 1) head
    else {
      // If there is no overflow, use arithmetic series formula
      //   a + ... (n terms total) ... + b = n*(a+b)/2
      if ((num eq scala.math.Numeric.IntIsIntegral)||
        (num eq scala.math.Numeric.ShortIsIntegral)||
        (num eq scala.math.Numeric.ByteIsIntegral)||
        (num eq scala.math.Numeric.CharIsIntegral)) {
        // We can do math with no overflow in a Long--easy
        val exact = (size * ((num toLong head) + (num toInt last))) / 2
        num fromInt exact.toInt
      }
      else if (num eq scala.math.Numeric.LongIsIntegral) {
        // Uh-oh, might be overflow, so we have to divide before we overflow.
        // Either numRangeElements or (head + last) must be even, so divide the even one before multiplying
        val a = head.toLong
        val b = last.toLong
        val ans =
          if ((size & 1) == 0) (size / 2) * (a + b)
          else size * {
            // Sum is even, but we might overflow it, so divide in pieces and add back remainder
            val ha = a/2
            val hb = b/2
            ha + hb + ((a - 2*ha) + (b - 2*hb)) / 2
          }
        ans.asInstanceOf[B]
      }
      else {
        // User provided custom Numeric, so we cannot rely on arithmetic series formula (e.g. won't work on something like Z_6)
        if (isEmpty) num.zero
        else {
          var acc = num.zero
          var i = head
          var idx = 0
          while(idx < length) {
            acc = num.plus(acc, i)
            i = i + step
            idx = idx + 1
          }
          acc
        }
      }
    }
  }

  /** The hash code of this range, computed on first access and cached.
   *  Consistent with `equals`: equal to the hash code of any sequence with
   *  the same elements.
   */
  override lazy val hashCode: Int = super.hashCode()
  /** Returns `Int.MaxValue`: indexed access via `apply` is cheap at every
   *  length, so element scans (as in `sameElements`) never need to switch to
   *  an iterator.
   */
  override protected final def applyPreferredMaxLength: Int = Int.MaxValue

  /** Compares this range to `other` for equality.
   *
   *  Two numeric ranges are equal if they have the same length and, when
   *  non-empty, the same `start` and the same last element; the elements in
   *  between are then necessarily the same. Comparison with any other kind
   *  of sequence falls back to the element-by-element comparison of the
   *  superclass.
   *
   *  @param other the value to compare this range with
   *  @return `true` if `other` is a sequence with the same elements in the
   *          same order as this range
   */
  override def equals(other: Any): Boolean = other match {
    case x: NumericRange[_] =>
      (x canEqual this) && (length == x.length) && (
        (isEmpty) ||                            // all empty sequences are equal
          (start == x.start && last == x.last)  // same length and same endpoints implies equality
        )
    case _ =>
      super.equals(other)
  }

  /** Returns a string representation of this range, such as
   *  `NumericRange 1 until 10 by 2`: `to` for an inclusive range, `until`
   *  for an exclusive one, prefixed with `empty ` if the range is empty.
   *  The `by` clause is omitted when `step` is 1.
   */
  override def toString: String = {
    val empty = if (isEmpty) "empty " else ""
    val preposition = if (isInclusive) "to" else "until"
    val stepped = if (step == 1) "" else s" by $step"
    s"${empty}NumericRange $start $preposition $end$stepped"
  }

  override protected[this] def className = "NumericRange"
}

/** A companion object for numeric ranges.
 *  @define Coll `NumericRange`
 *  @define coll numeric range
 */
object NumericRange {
  private def bigDecimalCheckUnderflow[T](start: T, end: T, step: T)(implicit num: Integral[T]): Unit = {
    def FAIL(boundary: T, step: T): Unit = {
      val msg = boundary match {
        case bd: BigDecimal => s"Precision ${bd.mc.getPrecision}"
        case _              => "Precision"
      }
      throw new IllegalArgumentException(
        s"$msg inadequate to represent steps of size $step near $boundary"
      )
    }
    if (num.minus(num.plus(start, step), start) != step) FAIL(start, step)
    if (num.minus(end, num.minus(end, step))    != step) FAIL(end,   step)
  }

  /** Calculates the number of elements in a range given start, end, step, and
   *  whether or not it is inclusive.  Throws an exception if step == 0 or
   *  the number of elements exceeds the maximum Int.
   *
   *  @tparam T the numeric type of the range elements, which must have an `Integral` instance
   *  @param start the first value in the range
   *  @param end the end boundary of the range (exclusive or inclusive depending on `isInclusive`)
   *  @param step the increment between successive elements
   *  @param isInclusive whether `end` is included in the range
   *  @param num the `Integral` instance used for arithmetic on `T`
   *  @return the number of elements in the range
   */
  def count[T](start: T, end: T, step: T, isInclusive: Boolean)(implicit num: Integral[T]): Int = {
    val zero    = num.zero
    val upward  = num.lt(start, end)
    val posStep = num.gt(step, zero)

    if (step == zero) throw new IllegalArgumentException("step cannot be 0.")
    else if (start == end) if (isInclusive) 1 else 0
    else if (upward != posStep) 0
    else {
      /* We have to be frightfully paranoid about running out of range.
       * We also can't assume that the numbers will fit in a Long.
       * We will assume that if a > 0, -a can be represented, and if
       * a < 0, -a+1 can be represented.  We also assume that if we
       * can't fit in Int, we can represent 2*Int.MaxValue+3 (at least).
       * And we assume that numbers wrap rather than cap when they overflow.
       */
      // Check whether we can short-circuit by deferring to Int range.
      val startint = num.toInt(start)
      if (start == num.fromInt(startint)) {
        val endint = num.toInt(end)
        if (end == num.fromInt(endint)) {
          val stepint = num.toInt(step)
          if (step == num.fromInt(stepint)) {
            return {
              if (isInclusive) Range.inclusive(startint, endint, stepint).length
              else             Range          (startint, endint, stepint).length
            }
          }
        }
      }
      // If we reach this point, deferring to Int failed.
      // Numbers may be big.
      if (num.isInstanceOf[Numeric.BigDecimalAsIfIntegral]) {
        bigDecimalCheckUnderflow(start, end, step)  // Throw exception if math is inaccurate (including no progress at all)
      }
      val one = num.one
      val limit = num.fromInt(Int.MaxValue)
      def check(t: T): T =
        if (num.gt(t, limit)) throw new IllegalArgumentException("More than Int.MaxValue elements.")
        else t
      // If the range crosses zero, it might overflow when subtracted
      val startside = num.sign(start)
      val endside = num.sign(end)
      num.toInt{
        if (num.gteq(num.times(startside, endside), zero)) {
          // We're sure we can subtract these numbers.
          // Note that we do not use .rem because of different conventions for Long and BigInt
          val diff = num.minus(end, start)
          val quotient = check(num.quot(diff, step))
          val remainder = num.minus(diff, num.times(quotient, step))
          if (!isInclusive && zero == remainder) quotient else check(num.plus(quotient, one))
        }
        else {
          // We might not even be able to subtract these numbers.
          // Jump in three pieces:
          //   * start to -1 or 1, whichever is closer (waypointA)
          //   * one step, which will take us at least to 0 (ends at waypointB)
          //   * (except with really small numbers)
          //   * there to the end
          val negone = num.fromInt(-1)
          val startlim  = if (posStep) negone else one
          //Use start value if the start value is closer to zero than startlim
          //   * e.g. .5 is closer to zero than 1 and -.5 is closer to zero than -1
          val startdiff = {
            if ((posStep && num.lt(startlim, start)) || (!posStep && num.gt(startlim, start)))
              start
            else
              num.minus(startlim, start)
          }
          val startq    = check(num.quot(startdiff, step))
          val waypointA = if (startq == zero) start else num.plus(start, num.times(startq, step))
          val waypointB = num.plus(waypointA, step)
          check {
            if (num.lt(waypointB, end) != upward) {
              // No last piece
              if (isInclusive && waypointB == end) num.plus(startq, num.fromInt(2))
              else num.plus(startq, one)
            }
            else {
              // There is a last piece
              val enddiff = num.minus(end,waypointB)
              val endq    = check(num.quot(enddiff, step))
              val last    = if (endq == zero) waypointB else num.plus(waypointB, num.times(endq, step))
              // Now we have to tally up all the pieces
              //   1 for the initial value
              //   startq steps to waypointA
              //   1 step to waypointB
              //   endq steps to the end (one less if !isInclusive and last==end)
              num.plus(startq, num.plus(endq, if (!isInclusive && last==end) one else num.fromInt(2)))
            }
          }
        }
      }
    }
  }

  /** A numeric range that includes its `end` value, when `end` is a whole
   *  number of steps from `start`.
   *
   *  @tparam T the element type of the range
   *  @param start the start value of the range
   *  @param end the end value, an element of the range when it is a whole
   *         number of steps from `start`
   *  @param step the increment between successive elements; must not be zero
   *  @param num the `Integral` instance providing arithmetic on `T`
   */
  @SerialVersionUID(3L)
  class Inclusive[T](start: T, end: T, step: T)(implicit num: Integral[T])
    extends NumericRange(start, end, step, true) {
    /** Creates an inclusive range with the given start, end, and step.
     *
     *  @param start the start value of the new range
     *  @param end the end value of the new range, included when it is a whole
     *         number of steps from `start`
     *  @param step the step value of the new range
     *  @return a new inclusive range with the given `start`, `end`, and `step`
     */
    override def copy(start: T, end: T, step: T): Inclusive[T] =
      NumericRange.inclusive(start, end, step)

    /** Returns an exclusive range with the same `start`, `end`, and `step` as this range. */
    def exclusive: Exclusive[T] = NumericRange(start, end, step)
  }

  /** A numeric range that excludes its `end` value.
   *
   *  @tparam T the element type of the range
   *  @param start the start value of the range
   *  @param end the end value, never an element of the range itself
   *  @param step the increment between successive elements; must not be zero
   *  @param num the `Integral` instance providing arithmetic on `T`
   */
  @SerialVersionUID(3L)
  class Exclusive[T](start: T, end: T, step: T)(implicit num: Integral[T])
    extends NumericRange(start, end, step, false) {
    /** Creates an exclusive range with the given start, end, and step.
     *
     *  @param start the start value of the new range
     *  @param end the end value of the new range, excluded from it
     *  @param step the step value of the new range
     *  @return a new exclusive range with the given `start`, `end`, and `step`
     */
    override def copy(start: T, end: T, step: T): Exclusive[T] =
      NumericRange(start, end, step)

    /** Returns an inclusive range with the same `start`, `end`, and `step` as this range. */
    def inclusive: Inclusive[T] = NumericRange.inclusive(start, end, step)
  }

  /** Creates an exclusive numeric range, from `start` until `end` (excluded)
   *  in increments of `step`.
   *
   *  @tparam T the element type of the range
   *  @param start the start value of the range
   *  @param end the end value, excluded from the range
   *  @param step the increment between successive elements; must not be zero
   *  @param num the `Integral` instance providing arithmetic on `T`
   *  @return a new exclusive range with the given `start`, `end`, and `step`
   */
  def apply[T](start: T, end: T, step: T)(implicit num: Integral[T]): Exclusive[T] =
    new Exclusive(start, end, step)
  /** Creates an inclusive numeric range, from `start` to `end` in increments
   *  of `step`.
   *
   *  @tparam T the element type of the range
   *  @param start the start value of the range
   *  @param end the end value, an element of the range when it is a whole
   *         number of steps from `start`
   *  @param step the increment between successive elements; must not be zero
   *  @param num the `Integral` instance providing arithmetic on `T`
   *  @return a new inclusive range with the given `start`, `end`, and `step`
   */
  def inclusive[T](start: T, end: T, step: T)(implicit num: Integral[T]): Inclusive[T] =
    new Inclusive(start, end, step)

  private[collection] val defaultOrdering = Map[Numeric[_], Ordering[_]](
    Numeric.IntIsIntegral -> Ordering.Int,
    Numeric.ShortIsIntegral -> Ordering.Short,
    Numeric.ByteIsIntegral -> Ordering.Byte,
    Numeric.CharIsIntegral -> Ordering.Char,
    Numeric.LongIsIntegral -> Ordering.Long
  )

  @SerialVersionUID(3L)
  private final class NumericRangeIterator[T](self: NumericRange[T], num: Integral[T]) extends AbstractIterator[T] with Serializable {
    import num.mkNumericOps

    private[this] var _hasNext = !self.isEmpty
    private[this] var _next: T = self.start
    private[this] val lastElement: T = if (_hasNext) self.last else self.start
    override def knownSize: Int = if (_hasNext) num.toInt((lastElement - _next) / self.step) + 1 else 0
    def hasNext: Boolean = _hasNext
    def next(): T = {
      if (!_hasNext) Iterator.empty.next()
      val value = _next
      _hasNext = value != lastElement
      _next = num.plus(value, self.step)
      value
    }
  }
}
