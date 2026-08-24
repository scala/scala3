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

import java.util.Arrays

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.Stepper.EfficientSplit
import scala.collection.mutable.{ArrayBuffer, ArrayBuilder, Builder, ArraySeq => MutableArraySeq}
import scala.collection.convert.impl._
import scala.reflect.ClassTag
import scala.runtime.ScalaRunTime
import scala.util.Sorting
import scala.util.hashing.MurmurHash3

/** An immutable array.
 *
 *  Supports efficient indexed access and has a small memory footprint.
 *
 *  @define coll immutable array
 *  @define Coll `ArraySeq`
 *
 *  @tparam A the element type of the immutable array
 */
sealed abstract class ArraySeq[+A]
  extends AbstractSeq[A]
    with IndexedSeq[A]
    with IndexedSeqOps[A, ArraySeq, ArraySeq[A]]
    with StrictOptimizedSeqOps[A, ArraySeq, ArraySeq[A]]
    with EvidenceIterableFactoryDefaults[A, ArraySeq, ClassTag]
    with Serializable{

  /** The tag of the element type. This does not have to be equal to the element type of this ArraySeq. A primitive
   *  ArraySeq can be backed by an array of boxed values and a reference ArraySeq can be backed by an array of a supertype
   *  or subtype of the element type. 
   */
  protected def elemTag: ClassTag[?]

  /** Returns the companion factory in its untagged form, which requires no `ClassTag`.
   *
   *  Because no element type is known, the immutable arrays this factory constructs
   *  are backed by an `Array[AnyRef]` and store primitive values boxed. Its `from`
   *  constructs nothing when the source is already an `ArraySeq`, returning it with
   *  whatever backing array it has.
   */
  override def iterableFactory: SeqFactory[ArraySeq] = ArraySeq.untagged

  /** The wrapped mutable `Array` that backs this `ArraySeq`. Any changes to this array will break
   *  the expected immutability. Its element type does not have to be equal to the element type of this ArraySeq.
   *  A primitive ArraySeq can be backed by an array of boxed values and a reference ArraySeq can be backed by an
   *  array of a supertype or subtype of the element type. 
   */
  def unsafeArray: Array[?]

  /** Returns the `ArraySeq` companion object, the `ClassTag`-based factory used by the
   *  default implementations of `fromSpecific`, `newSpecificBuilder`, and `empty`.
   */
  protected def evidenceIterableFactory: ArraySeq.type = ArraySeq
  /** Returns this immutable array's `elemTag` viewed as a `ClassTag[A]`, the evidence
   *  passed to `evidenceIterableFactory` when building collections of the same element
   *  type.
   */
  protected def iterableEvidence: ClassTag[A @uncheckedVariance] = elemTag.asInstanceOf[ClassTag[A]]

  /** Returns a stepper for the elements of this immutable array.
   *
   *  Steppers enable creating a Java stream to operate on the elements. Where the
   *  element type is a primitive with a value-shaped stepper - that is, any except
   *  `Boolean` and `Unit` - the stepper steps over unboxed values unless a
   *  reference-shaped stepper is explicitly requested.
   *
   *  @tparam S the type of the stepper, determined by `shape`
   *  @param shape implicit evidence selecting the stepper type for the element type `A`
   *  @return a stepper over the elements, supporting efficient splitting for parallel
   *          processing
   */
  def stepper[S <: Stepper[?]](implicit shape: StepperShape[A, S]): S & EfficientSplit

  /** Returns the element at the given index of the underlying array.
   *
   *  @param i the zero-based index of the element
   *  @throws ArrayIndexOutOfBoundsException if `i` is negative or not less than
   *          `length`
   */
  @throws[ArrayIndexOutOfBoundsException]
  def apply(i: Int): A

  /** Returns a new immutable array with the element at `index` replaced by `elem`.
   *
   *  The elements are copied into a new boxed array, so the result is backed by an
   *  `Array[AnyRef]` even when this immutable array has a primitive representation;
   *  the specialized subclasses override this to keep the primitive representation
   *  when `elem` fits it.
   *
   *  @tparam B the element type of the returned immutable array, a supertype of `A`
   *  @param index the zero-based index of the element to replace
   *  @param elem the value to store at `index`
   *  @return a new immutable array, identical to this one except at `index`
   *  @throws IndexOutOfBoundsException if `index` is negative or not less than `length`
   */
  override def updated[B >: A](index: Int, elem: B): ArraySeq[B] = {
    val dest = new Array[Any](length)
    Array.copy(unsafeArray, 0, dest, 0, length)
    dest(index) = elem
    ArraySeq.unsafeWrapArray(dest).asInstanceOf[ArraySeq[B]]
  }

  /** Returns a new immutable array resulting from applying `f` to each element of
   *  this immutable array.
   *
   *  The results are collected into a new boxed array, so the returned collection is
   *  backed by an `Array[AnyRef]` regardless of `B`.
   *
   *  @tparam B the element type of the returned immutable array
   *  @param f the function to apply to each element
   *  @return a new immutable array containing the results in the same order
   */
  override def map[B](f: A => B): ArraySeq[B] = {
    val a = new Array[Any](size)
    var i = 0
    while (i < a.length){
      a(i) = f(apply(i))
      i += 1
    }
    ArraySeq.unsafeWrapArray(a).asInstanceOf[ArraySeq[B]]
  }

  /** Returns a new immutable array with `elem` prepended before all elements of this
   *  immutable array.
   *
   *  The elements are copied into a new boxed array, so the result is backed by an
   *  `Array[AnyRef]`; the specialized subclasses override this to keep a primitive
   *  representation when `elem` fits it.
   *
   *  @tparam B the element type of the returned immutable array, a supertype of `A`
   *  @param elem the element to prepend
   *  @return a new immutable array with `elem` as its first element
   */
  override def prepended[B >: A](elem: B): ArraySeq[B] =
    ArraySeq.unsafeWrapArray(unsafeArray.prepended[Any](elem)).asInstanceOf[ArraySeq[B]]

  /** Returns a new immutable array with `elem` appended after all elements of this
   *  immutable array.
   *
   *  The elements are copied into a new boxed array, so the result is backed by an
   *  `Array[AnyRef]`; the specialized subclasses override this to keep a primitive
   *  representation when `elem` fits it.
   *
   *  @tparam B the element type of the returned immutable array, a supertype of `A`
   *  @param elem the element to append
   *  @return a new immutable array with `elem` as its last element
   */
  override def appended[B >: A](elem: B): ArraySeq[B] =
    ArraySeq.unsafeWrapArray(unsafeArray.appended[Any](elem)).asInstanceOf[ArraySeq[B]]

  /** Fast concatenation of two [[ArraySeq]]s.
   *
   *  @tparam B the element type of the resulting sequence, a supertype of `A`
   *  @param that the `ArraySeq` to append to this sequence
   *  @return the concatenated `ArraySeq`, or `null` if optimization is not possible
   */
  private def appendedAllArraySeq[B >: A](that: ArraySeq[B]): ArraySeq[B] | Null = {
    // Optimise concatenation of two ArraySeqs
    // For ArraySeqs with sizes of [100, 1000, 10000] this is [3.5, 4.1, 5.2]x as fast
    if (isEmpty)
      that
    else if (that.isEmpty)
      this
    else {
      val thisIsObj = this.unsafeArray.isInstanceOf[Array[AnyRef]]
      val thatIsObj = that.unsafeArray.isInstanceOf[Array[AnyRef]]
      val mismatch = thisIsObj != thatIsObj
      if (mismatch)
        // Combining primatives and objects: abort
        null
      else if (thisIsObj) {
        // A and B are objects
        val ax = this.unsafeArray.asInstanceOf[Array[A]]
        val ay = that.unsafeArray.asInstanceOf[Array[B]]
        val len = ax.length + ay.length
        val a = new Array[AnyRef](len)
        System.arraycopy(ax, 0, a, 0, ax.length)
        System.arraycopy(ay, 0, a, ax.length, ay.length)
        ArraySeq.unsafeWrapArray(a).asInstanceOf[ArraySeq[B]]
      } else {
        // A is a primative and B = A. Use this instance's protected ClassTag.
        val ax = this.unsafeArray.asInstanceOf[Array[A]]
        val ay = that.unsafeArray.asInstanceOf[Array[A]]
        val len = ax.length + ay.length
        val a = iterableEvidence.newArray(len)
        System.arraycopy(ax, 0, a, 0, ax.length)
        System.arraycopy(ay, 0, a, ax.length, ay.length)
        ArraySeq.unsafeWrapArray(a).asInstanceOf[ArraySeq[B]]
      }
    }
  }

  /** Returns a new immutable array containing the elements of this immutable array
   *  followed by the elements of `suffix`.
   *
   *  When `suffix` is also an `ArraySeq`, an empty operand results in the other
   *  operand being returned unchanged, and two operands backed by the same kind of
   *  array (both object arrays, or both primitive arrays of the same element type)
   *  are concatenated directly, a primitive representation being preserved. In all
   *  other cases the elements are copied into a new boxed array, so the result is
   *  backed by an `Array[AnyRef]`; if `suffix` is known to be empty, this immutable
   *  array is returned unchanged.
   *
   *  @tparam B the element type of the returned immutable array, a supertype of `A`
   *  @param suffix the elements to append
   *  @return the concatenated immutable array
   *  @throws ArrayStoreException if this immutable array and `suffix` are both backed
   *          by primitive arrays but of different element types, as in
   *          `ArraySeq(1) ++ ArraySeq(1L)`: the direct path then writes the elements
   *          of `suffix` into an array of this immutable array's element type
   */
  override def appendedAll[B >: A](suffix: collection.IterableOnce[B]^): ArraySeq[B] = {
    def genericResult = {
      val k = suffix.knownSize
      if (k == 0) this
      else {
        val b = ArrayBuilder.make[Any]
        if(k >= 0) b.sizeHint(k + unsafeArray.length)
        b.addAll(unsafeArray)
        b.addAll(suffix)
        ArraySeq.unsafeWrapArray(b.result()).asInstanceOf[ArraySeq[B]]
      }
    }

    suffix match {
      case that: ArraySeq[?] =>
        val result = appendedAllArraySeq(that.asInstanceOf[ArraySeq[B]])
        if (result == null) genericResult
        else result
      case _ =>
        genericResult
    }
  }

  /** Returns a new immutable array containing the elements of `prefix` followed by
   *  the elements of this immutable array.
   *
   *  When `prefix` is also an `ArraySeq`, an empty operand results in the other
   *  operand being returned unchanged, and two operands backed by the same kind of
   *  array (both object arrays, or both primitive arrays of the same element type)
   *  are concatenated directly, a primitive representation being preserved. In all
   *  other cases the elements are copied into a new boxed array, so the result is
   *  backed by an `Array[AnyRef]`; if `prefix` is known to be empty, this immutable
   *  array is returned unchanged.
   *
   *  @tparam B the element type of the returned immutable array, a supertype of `A`
   *  @param prefix the elements to prepend
   *  @return the concatenated immutable array
   *  @throws ArrayStoreException if `prefix` and this immutable array are both backed
   *          by primitive arrays but of different element types, as in
   *          `ArraySeq(1L).prependedAll(ArraySeq(1))`: the direct path then writes the
   *          elements of this immutable array into an array of `prefix`'s element type
   */
  override def prependedAll[B >: A](prefix: collection.IterableOnce[B]^): ArraySeq[B] = {
    def genericResult = {
      val k = prefix.knownSize
      if (k == 0) this
      else {
        val b = ArrayBuilder.make[Any]
        if(k >= 0) b.sizeHint(k + unsafeArray.length)
        b.addAll(prefix)
        if(k < 0) b.sizeHint(b.length + unsafeArray.length)
        b.addAll(unsafeArray)
        ArraySeq.unsafeWrapArray(b.result()).asInstanceOf[ArraySeq[B]]
      }
    }

    prefix match {
      case that: ArraySeq[?] =>
        val result = that.asInstanceOf[ArraySeq[B]].appendedAllArraySeq(this)
        if (result == null) genericResult
        else result
      case _ =>
        genericResult
    }
  }

  /** Returns a new immutable array of pairs formed from the elements of this
   *  immutable array and the corresponding elements of `that`.
   *
   *  The result is truncated to the length of the shorter operand, and is backed by
   *  an array of tuples.
   *
   *  @tparam B the element type of `that`
   *  @param that the collection providing the second half of each pair
   */
  override def zip[B](that: collection.IterableOnce[B]^): ArraySeq[(A, B)] =
    that match {
      case bs: ArraySeq[B] =>
        ArraySeq.tabulate(length min bs.length) { i =>
          (apply(i), bs(i))
        }
      case _ =>
        strictOptimizedZip[B, ArraySeq[(A, B)]](that, iterableFactory.newBuilder)
    }

  /** Returns an immutable array containing the first `n` elements of this immutable
   *  array.
   *
   *  @param n the number of elements to take
   *  @return this immutable array itself if `n >= length`, otherwise a new immutable
   *          array containing the first `max(n, 0)` elements, copied into an array of
   *          the same element type
   */
  override def take(n: Int): ArraySeq[A] =
    if (unsafeArray.length <= n)
      this
    else
      ArraySeq.unsafeWrapArray(new ArrayOps(unsafeArray).take(n)).asInstanceOf[ArraySeq[A]]

  /** Returns an immutable array containing the last `n` elements of this immutable
   *  array.
   *
   *  @param n the number of elements to take
   *  @return this immutable array itself if `n >= length`, otherwise a new immutable
   *          array containing the last `max(n, 0)` elements, copied into an array of
   *          the same element type
   */
  override def takeRight(n: Int): ArraySeq[A] =
    if (unsafeArray.length <= n)
      this
    else
      ArraySeq.unsafeWrapArray(new ArrayOps(unsafeArray).takeRight(n)).asInstanceOf[ArraySeq[A]]

  /** Returns an immutable array containing all elements of this immutable array
   *  except the first `n`.
   *
   *  @param n the number of elements to drop
   *  @return this immutable array itself if `n <= 0`, otherwise a new immutable array
   *          containing the remaining elements (empty if `n >= length`), copied into
   *          an array of the same element type
   */
  override def drop(n: Int): ArraySeq[A] =
    if (n <= 0)
      this
    else
      ArraySeq.unsafeWrapArray(new ArrayOps(unsafeArray).drop(n)).asInstanceOf[ArraySeq[A]]

  /** Returns an immutable array containing all elements of this immutable array
   *  except the last `n`.
   *
   *  @param n the number of elements to drop
   *  @return this immutable array itself if `n <= 0`, otherwise a new immutable array
   *          containing the remaining elements (empty if `n >= length`), copied into
   *          an array of the same element type
   */
  override def dropRight(n: Int): ArraySeq[A] =
    if (n <= 0)
      this
    else
      ArraySeq.unsafeWrapArray(new ArrayOps(unsafeArray).dropRight(n)).asInstanceOf[ArraySeq[A]]

  /** Returns an immutable array containing the elements of this immutable array from
   *  index `from` (inclusive) up to index `until` (exclusive).
   *
   *  Both indices are clamped to the valid range, so the result is empty when
   *  `until <= from` after clamping.
   *
   *  @param from the index of the first element to include
   *  @param until the index one past the last element to include
   *  @return this immutable array itself if the slice covers it entirely
   *          (`from <= 0` and `until >= length`), otherwise a new immutable array
   *          containing the selected elements, copied into an array of the same
   *          element type
   */
  override def slice(from: Int, until: Int): ArraySeq[A] =
    if (from <= 0 && unsafeArray.length <= until)
      this
    else
      ArraySeq.unsafeWrapArray(new ArrayOps(unsafeArray).slice(from, until)).asInstanceOf[ArraySeq[A]]

  /** Applies a binary operator to a start value and all elements of this immutable
   *  array, going left to right.
   *
   *  @tparam B the result type of the binary operator
   *  @param z the start value
   *  @param f the binary operator, applied to the accumulated value and the next
   *           element
   *  @return the result of inserting `f` between consecutive elements, going left to
   *          right, with the start value `z` on the left; `z` if this immutable array
   *          is empty
   */
  override def foldLeft[B](z: B)(f: (B, A) => B): B = {
    // For ArraySeqs with sizes of [100, 1000, 10000] this is [1.3, 1.8, 1.8]x as fast
    // as the same while-loop over this instead of unsafeArray.
    val array = unsafeArray
    var b = z
    var i = 0
    while (i < array.length) {
      val a = array(i).asInstanceOf[A]
      b = f(b, a)
      i += 1
    }
    b
  }

  /** Applies a binary operator to all elements of this immutable array and a start
   *  value, going right to left.
   *
   *  @tparam B the result type of the binary operator
   *  @param z the start value
   *  @param f the binary operator, applied to the next element and the accumulated
   *           value
   *  @return the result of inserting `f` between consecutive elements, going right to
   *          left, with the start value `z` on the right; `z` if this immutable array
   *          is empty
   */
  override def foldRight[B](z: B)(f: (A, B) => B): B = {
    // For ArraySeqs with sizes of [100, 1000, 10000] this is [1.6, 1.8, 2.7]x as fast
    // as the same while-loop over this instead of unsafeArray.
    val array = unsafeArray
    var b = z
    var i = array.length
    while (i > 0) {
      i -= 1
      val a = array(i).asInstanceOf[A]
      b = f(a, b)
    }
    b
  }

  /** Returns a new immutable array containing all elements of this immutable array
   *  except the first.
   *
   *  @throws UnsupportedOperationException if this immutable array is empty
   */
  override def tail: ArraySeq[A] = ArraySeq.unsafeWrapArray(new ArrayOps(unsafeArray).tail).asInstanceOf[ArraySeq[A]]

  /** Returns a new immutable array with the elements of this immutable array in
   *  reverse order.
   */
  override def reverse: ArraySeq[A] = ArraySeq.unsafeWrapArray(new ArrayOps(unsafeArray).reverse).asInstanceOf[ArraySeq[A]]

  /** Returns `"ArraySeq"`, the name of this collection type used in `toString` output. */
  override protected def className = "ArraySeq"

  /** Copies elements of this immutable array to another array, beginning at index
   *  `start` of `xs`.
   *
   *  The number of elements copied is the minimum of `len`, the length of this
   *  immutable array, and the remaining capacity of `xs` from `start`; if that
   *  minimum is not positive, nothing is copied. Copying is performed by a single
   *  `Array.copy` of the underlying array.
   *
   *  A negative `start` is not rejected before that count is computed: the whole
   *  length of `xs` is taken as the capacity, and the `Array.copy` that follows a
   *  positive count throws.
   *
   *  @tparam B the element type of the destination array, a supertype of `A`
   *  @param xs the destination array
   *  @param start the index of `xs` at which to write the first element
   *  @param len the maximum number of elements to copy
   *  @return the number of elements actually copied
   *  @throws ArrayIndexOutOfBoundsException if `start` is negative and at least one
   *          element would be copied
   */
  override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Int = {
    val copied = IterableOnce.elemsToCopyToArray(length, xs.length, start, len)
    if(copied > 0) {
      Array.copy(unsafeArray, 0, xs, start, copied)
    }
    copied
  }

  /** Returns `Int.MaxValue`, indicating that indexed access via `apply` is never more
   *  expensive than `iterator`, so element scans such as `sameElements` always use
   *  indexed access.
   */
  override protected final def applyPreferredMaxLength: Int = Int.MaxValue

  /** Returns an immutable array with the elements of this immutable array sorted
   *  according to the given ordering.
   *
   *  Returns this immutable array itself if it has fewer than two elements. Otherwise
   *  the elements are copied into a boxed array and sorted with a stable sort, so the
   *  result is backed by an `Array[AnyRef]`; several specialized subclasses override
   *  this to keep a primitive representation when sorting by the natural ordering.
   *
   *  @tparam B the type on which `ord` is defined, a supertype of `A`
   *  @param ord the ordering used to compare elements
   */
  override def sorted[B >: A](implicit ord: Ordering[B]): ArraySeq[A] =
    if(unsafeArray.length <= 1) this
    else {
      val a = Array.copyAs[AnyRef](unsafeArray, length)
      Arrays.sort(a, ord.asInstanceOf[Ordering[AnyRef]])
      new ArraySeq.ofRef[AnyRef](a).asInstanceOf[ArraySeq[A]]
    }
}

/** $factoryInfo
 *  @define coll immutable array
 *  @define Coll `ArraySeq`
 */
@SerialVersionUID(3L)
object ArraySeq extends StrictOptimizedClassTagSeqFactory[ArraySeq] { self =>
  /** A factory for immutable `ArraySeq`s that requires no `ClassTag`.
   *
   *  Because no element type is known, the immutable arrays this factory constructs
   *  are backed by an `Array[AnyRef]` and store primitive values boxed. Its `from`
   *  constructs nothing when the source is already an `ArraySeq`, returning it with
   *  whatever backing array it has.
   */
  val untagged: SeqFactory[ArraySeq] = new ClassTagSeqFactory.AnySeqDelegate(self)

  private lazy val emptyImpl = new ArraySeq.ofRef[Nothing](new Array[Nothing](0))

  /** Returns the empty immutable array.
   *
   *  The same instance, backed by an empty array, is shared for all element types;
   *  the `ClassTag` is never consulted.
   *
   *  @tparam A the element type
   */
  def empty[A : ClassTag]: ArraySeq[A] = emptyImpl

  /** Returns an immutable array containing the elements of the given collection.
   *
   *  If `it` is already an immutable `ArraySeq`, it is returned unchanged and `tag`
   *  is not consulted. Otherwise the elements are copied into a new array whose
   *  element type is determined by `tag`, and the result wraps that array.
   *
   *  @tparam A the element type
   *  @param it the source collection
   *  @param tag the class tag determining the element type of the backing array when
   *             a new one is created
   */
  def from[A](it: scala.collection.IterableOnce[A]^)(implicit tag: ClassTag[A]): ArraySeq[A] = it match {
    case as: ArraySeq[A] => as
    case _ => unsafeWrapArray(Array.from[A](it))
  }

  /** Returns a new builder for an immutable `ArraySeq`.
   *
   *  The builder collects elements into an `ArrayBuffer`; when the result is
   *  requested they are copied into a new array whose element type is determined by
   *  the `ClassTag`, and the result wraps that array.
   *
   *  @tparam A the element type
   */
  def newBuilder[A : ClassTag]: Builder[A, ArraySeq[A]] =
    ArrayBuffer.newBuilder[A].mapResult(b => unsafeWrapArray[A](b.toArray))

  /** Returns an immutable array of `n` elements, or an empty one if `n` is not
   *  positive, where each element is the result of one evaluation of `elem`.
   *
   *  `elem` is evaluated once per element of the result, so not at all when `n` is
   *  not positive. The element type of the backing array is determined by the
   *  `ClassTag`.
   *
   *  @tparam A the element type
   *  @param n the number of elements
   *  @param elem the by-name expression computing each element
   *  @return an immutable array with `n` evaluations of `elem`, or an empty immutable
   *          array if `n` is not positive
   */
  override def fill[A : ClassTag](n: Int)(elem: => A): ArraySeq[A] = tabulate(n)(_ => elem)

  /** Returns an immutable array of `n` elements, or an empty one if `n` is not
   *  positive, where the element at each index is the result of applying `f` to
   *  that index.
   *
   *  `f` is applied to the indices `0` to `n - 1` in ascending order, so not at all
   *  when `n` is not positive. The element type of the backing array is determined
   *  by the `ClassTag`.
   *
   *  @tparam A the element type
   *  @param n the number of elements
   *  @param f the function computing the element at each index
   *  @return an immutable array with the values `f(0), ..., f(n - 1)`, or an empty
   *          immutable array if `n` is not positive
   */
  override def tabulate[A : ClassTag](n: Int)(f: Int => A): ArraySeq[A] = {
    val elements = Array.ofDim[A](scala.math.max(n, 0))
    var i = 0
    while (i < n) {
      ScalaRunTime.array_update(elements, i, f(i))
      i = i + 1
    }
    ArraySeq.unsafeWrapArray(elements)
  }

  /** Wraps an existing `Array` into an `ArraySeq` of the proper primitive specialization type
   *  without copying. Any changes to wrapped array will break the expected immutability.
   *
   *  Note that an array containing boxed primitives can be wrapped in an `ArraySeq` without
   *  copying. For example, `val a: Array[Any] = Array(1)` is an array of `Object` at runtime,
   *  containing `Integer`s. An `ArraySeq[Int]` can be obtained with a cast:
   *  `ArraySeq.unsafeWrapArray(a).asInstanceOf[ArraySeq[Int]]`. The values are still
   *  boxed, the resulting instance is an [[ArraySeq.ofRef]]. Writing
   *  `ArraySeq.unsafeWrapArray(a.asInstanceOf[Array[Int]])` does not work, it throws a
   *  `ClassCastException` at runtime.
   *
   *  @tparam T the element type of the array to wrap
   *  @param x the array to wrap, which must not be modified after wrapping
   *  @return an `ArraySeq` backed by the given array, using the appropriate primitive specialization
   */
  def unsafeWrapArray[T](x: Array[T]): ArraySeq[T] = ((x: @unchecked) match {
    case null              => null
    case x: Array[AnyRef]  => new ofRef[AnyRef](x)
    case x: Array[Int]     => new ofInt(x)
    case x: Array[Double]  => new ofDouble(x)
    case x: Array[Long]    => new ofLong(x)
    case x: Array[Float]   => new ofFloat(x)
    case x: Array[Char]    => new ofChar(x)
    case x: Array[Byte]    => new ofByte(x)
    case x: Array[Short]   => new ofShort(x)
    case x: Array[Boolean] => new ofBoolean(x)
    case x: Array[Unit]    => new ofUnit(x)
  }).asInstanceOf[ArraySeq[T]]

  /** An immutable `ArraySeq` backed by an array of reference values.
   *
   *  The array given at construction is wrapped directly, not copied, and must not
   *  be mutated afterwards.
   *
   *  @tparam T the element type, a reference type
   *  @param unsafeArray the underlying array, which must not be mutated after
   *                     wrapping
   */
  @SerialVersionUID(3L)
  final class ofRef[T <: AnyRef | Null](val unsafeArray: Array[T]) extends ArraySeq[T] {
    /** Returns a class tag for the runtime component type of the underlying array,
     *  which may be a subtype or supertype of `T`.
     */
    def elemTag: ClassTag[T] = ClassTag[T](unsafeArray.getClass.getComponentType)
    /** Returns the number of elements in this immutable array. */
    def length: Int = unsafeArray.length
    /** Returns the element at the given index of the underlying array.
     *
     *  @param i the zero-based index of the element
     *  @throws ArrayIndexOutOfBoundsException if `i` is negative or not less than
     *          `length`
     */
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): T = unsafeArray(i)
    /** Returns a hash code computed from the array elements, consistent with the
     *  hashing of other sequences.
     */
    override def hashCode() = MurmurHash3.arraySeqHash(unsafeArray)
    /** Compares this immutable array with another object for equality.
     *
     *  Two `ofRef` instances are equal if their underlying arrays contain equal
     *  elements (compared with `equals`) in the same order. Comparison with
     *  anything else falls back to general sequence equality.
     *
     *  @param that the object to compare with
     *  @return `true` if `that` is an `ofRef` whose underlying array has equal
     *          elements in the same order, or is otherwise an equal sequence
     */
    override def equals(that: Any): Boolean = that match {
      case that: ofRef[?] =>
        Array.equals(
          this.unsafeArray.asInstanceOf[Array[AnyRef]],
          that.unsafeArray.asInstanceOf[Array[AnyRef]])
      case _ => super.equals(that)
    }
    /** Returns an immutable array with the elements of this immutable array sorted
     *  according to the given ordering.
     *
     *  Returns this immutable array itself if it has fewer than two elements;
     *  otherwise a clone of the underlying array is sorted with a stable sort and
     *  wrapped in a new `ofRef`, preserving the runtime element type of the
     *  underlying array.
     *
     *  @tparam B the type on which `ord` is defined, a supertype of `T`
     *  @param ord the ordering used to compare elements
     */
    override def sorted[B >: T](implicit ord: Ordering[B]): ArraySeq.ofRef[T] = {
      if(unsafeArray.length <= 1) this
      else {
        val a = unsafeArray.clone()
        Arrays.sort(a, ord.asInstanceOf[Ordering[T]])
        new ArraySeq.ofRef(a)
      }
    }
    /** Returns an iterator over the elements of the underlying array. */
    override def iterator: Iterator[T] = new ArrayOps.ArrayIterator[T](unsafeArray)
    /** Returns a stepper for the elements of this immutable array.
     *
     *  If a value-shaped stepper is requested for an element type that boxes a
     *  primitive, the stepper unboxes the values; otherwise it steps over the
     *  reference values directly.
     *
     *  @tparam S the type of the stepper, determined by `shape`
     *  @param shape implicit evidence selecting the stepper type for the element type `T`
     *  @return a stepper over the elements, supporting efficient splitting for
     *          parallel processing
     */
    override def stepper[S <: Stepper[?]](implicit shape: StepperShape[T, S]): S & EfficientSplit = (
      if(shape.shape == StepperShape.ReferenceShape)
        new ObjectArrayStepper(unsafeArray, 0, unsafeArray.length)
      else shape.parUnbox(new ObjectArrayStepper(unsafeArray, 0, unsafeArray.length).asInstanceOf[AnyStepper[T] & EfficientSplit])
    ).asInstanceOf[S & EfficientSplit]
  }

  /** An immutable `ArraySeq` backed by an `Array[Byte]`, storing its elements
   *  unboxed.
   *
   *  The array given at construction is wrapped directly, not copied, and must not
   *  be mutated afterwards.
   *
   *  @param unsafeArray the underlying array, which must not be mutated after
   *                     wrapping
   */
  @SerialVersionUID(3L)
  final class ofByte(val unsafeArray: Array[Byte]) extends ArraySeq[Byte] {
    // Type erases to `ManifestFactory.ByteManifest`, but can't annotate that because it's not accessible
    /** Returns `ClassTag.Byte`, the tag of the unboxed element type. */
    protected def elemTag: ClassTag.Byte.type = ClassTag.Byte
    /** Returns the number of elements in this immutable array. */
    def length: Int = unsafeArray.length
    /** Returns the element at the given index of the underlying array.
     *
     *  @param i the zero-based index of the element
     *  @throws ArrayIndexOutOfBoundsException if `i` is negative or not less than
     *          `length`
     */
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Byte = unsafeArray(i)
    /** Returns a hash code computed from the array elements, consistent with the
     *  hashing of other sequences.
     */
    override def hashCode() = MurmurHash3.arraySeqHash(unsafeArray)
    /** Compares this immutable array with another object for equality.
     *
     *  Two `ofByte` instances are equal if their underlying arrays contain the same
     *  elements in the same order. Comparison with anything else falls back to
     *  general sequence equality.
     *
     *  @param that the object to compare with
     *  @return `true` if `that` is an `ofByte` whose underlying array has the same
     *          elements in the same order, or is otherwise an equal sequence
     */
    override def equals(that: Any) = that match {
      case that: ofByte => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
    /** Returns an immutable array with the elements of this immutable array sorted
     *  according to the given ordering.
     *
     *  Returns this immutable array itself if it has fewer than two elements. If
     *  `ord` is `Ordering.Byte`, a copy of the underlying array is sorted and
     *  wrapped in a new `ofByte`; for any other ordering the generic implementation
     *  is used, returning an immutable array of boxed values.
     *
     *  @tparam B the type on which `ord` is defined, a supertype of `Byte`
     *  @param ord the ordering used to compare elements
     */
    override def sorted[B >: Byte](implicit ord: Ordering[B]): ArraySeq[Byte] =
      if(length <= 1) this
      else if(ord eq Ordering.Byte) {
        val a = unsafeArray.clone()
        Arrays.sort(a)
        new ArraySeq.ofByte(a)
      } else super.sorted[B]
    /** Returns an iterator over the elements of the underlying array. */
    override def iterator: Iterator[Byte] = new ArrayOps.ArrayIterator[Byte](unsafeArray)
    /** Returns a stepper for the elements of this immutable array.
     *
     *  The stepper steps over the unboxed `Byte` values, widened to `Int`, unless
     *  a reference-shaped stepper is explicitly requested, in which case the
     *  values are boxed.
     *
     *  @tparam S the type of the stepper, determined by `shape`
     *  @param shape implicit evidence selecting the stepper type for the element type `Byte`
     *  @return a stepper over the elements, supporting efficient splitting for
     *          parallel processing
     */
    override def stepper[S <: Stepper[?]](implicit shape: StepperShape[Byte, S]): S & EfficientSplit = (
      if(shape.shape == StepperShape.ReferenceShape)
        AnyStepper.ofParIntStepper(new WidenedByteArrayStepper(unsafeArray, 0, unsafeArray.length))
      else new WidenedByteArrayStepper(unsafeArray, 0, unsafeArray.length)
    ).asInstanceOf[S & EfficientSplit]
    /** Returns a new immutable array with the element at `index` replaced by `elem`.
     *
     *  If `elem` is a `Byte`, the result is an `ofByte` backed by a new
     *  `Array[Byte]`; otherwise the generic implementation is used and the result
     *  is backed by a boxed array.
     *
     *  @tparam B the element type of the returned immutable array, a supertype of `Byte`
     *  @param index the zero-based index of the element to replace
     *  @param elem the value to store at `index`
     *  @throws IndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    override def updated[B >: Byte](index: Int, elem: B): ArraySeq[B] =
      elem match {
        case b: Byte => new ArraySeq.ofByte(unsafeArray.updated(index, b))
        case _ => super.updated(index, elem)
      }
    /** Returns a new immutable array with `elem` appended after all elements of
     *  this immutable array.
     *
     *  If `elem` is a `Byte`, the result is an `ofByte` backed by a new
     *  `Array[Byte]`; otherwise the generic implementation is used and the result
     *  is backed by a boxed array.
     *
     *  @tparam B the element type of the returned immutable array, a supertype of `Byte`
     *  @param elem the element to append
     */
    override def appended[B >: Byte](elem: B): ArraySeq[B] =
      elem match {
        case b: Byte => new ArraySeq.ofByte(unsafeArray.appended(b))
        case _ => super.appended(elem)
      }
    /** Returns a new immutable array with `elem` prepended before all elements of
     *  this immutable array.
     *
     *  If `elem` is a `Byte`, the result is an `ofByte` backed by a new
     *  `Array[Byte]`; otherwise the generic implementation is used and the result
     *  is backed by a boxed array.
     *
     *  @tparam B the element type of the returned immutable array, a supertype of `Byte`
     *  @param elem the element to prepend
     */
    override def prepended[B >: Byte](elem: B): ArraySeq[B] =
      elem match {
        case b: Byte => new ArraySeq.ofByte(unsafeArray.prepended(b))
        case _ => super.prepended(elem)
      }
  }

  /** An immutable `ArraySeq` backed by an `Array[Short]`, storing its elements
   *  unboxed.
   *
   *  The array given at construction is wrapped directly, not copied, and must not
   *  be mutated afterwards.
   *
   *  @param unsafeArray the underlying array, which must not be mutated after
   *                     wrapping
   */
  @SerialVersionUID(3L)
  final class ofShort(val unsafeArray: Array[Short]) extends ArraySeq[Short] {
    // Type erases to `ManifestFactory.ShortManifest`, but can't annotate that because it's not accessible
    /** Returns `ClassTag.Short`, the tag of the unboxed element type. */
    protected def elemTag: ClassTag.Short.type = ClassTag.Short
    /** Returns the number of elements in this immutable array. */
    def length: Int = unsafeArray.length
    /** Returns the element at the given index of the underlying array.
     *
     *  @param i the zero-based index of the element
     *  @throws ArrayIndexOutOfBoundsException if `i` is negative or not less than
     *          `length`
     */
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Short = unsafeArray(i)
    /** Returns a hash code computed from the array elements, consistent with the
     *  hashing of other sequences.
     */
    override def hashCode() = MurmurHash3.arraySeqHash(unsafeArray)
    /** Compares this immutable array with another object for equality.
     *
     *  Two `ofShort` instances are equal if their underlying arrays contain the same
     *  elements in the same order. Comparison with anything else falls back to
     *  general sequence equality.
     *
     *  @param that the object to compare with
     *  @return `true` if `that` is an `ofShort` whose underlying array has the same
     *          elements in the same order, or is otherwise an equal sequence
     */
    override def equals(that: Any) = that match {
      case that: ofShort => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
    /** Returns an immutable array with the elements of this immutable array sorted
     *  according to the given ordering.
     *
     *  Returns this immutable array itself if it has fewer than two elements. If
     *  `ord` is `Ordering.Short`, a copy of the underlying array is sorted and
     *  wrapped in a new `ofShort`; for any other ordering the generic implementation
     *  is used, returning an immutable array of boxed values.
     *
     *  @tparam B the type on which `ord` is defined, a supertype of `Short`
     *  @param ord the ordering used to compare elements
     */
    override def sorted[B >: Short](implicit ord: Ordering[B]): ArraySeq[Short] =
      if(length <= 1) this
      else if(ord eq Ordering.Short) {
        val a = unsafeArray.clone()
        Arrays.sort(a)
        new ArraySeq.ofShort(a)
      } else super.sorted[B]
    /** Returns an iterator over the elements of the underlying array. */
    override def iterator: Iterator[Short] = new ArrayOps.ArrayIterator[Short](unsafeArray)
    /** Returns a stepper for the elements of this immutable array.
     *
     *  The stepper steps over the unboxed `Short` values, widened to `Int`, unless
     *  a reference-shaped stepper is explicitly requested, in which case the
     *  values are boxed.
     *
     *  @tparam S the type of the stepper, determined by `shape`
     *  @param shape implicit evidence selecting the stepper type for the element type `Short`
     *  @return a stepper over the elements, supporting efficient splitting for
     *          parallel processing
     */
    override def stepper[S <: Stepper[?]](implicit shape: StepperShape[Short, S]): S & EfficientSplit = (
      if(shape.shape == StepperShape.ReferenceShape)
        AnyStepper.ofParIntStepper(new WidenedShortArrayStepper(unsafeArray, 0, unsafeArray.length))
      else new WidenedShortArrayStepper(unsafeArray, 0, unsafeArray.length)
    ).asInstanceOf[S & EfficientSplit]
    /** Returns a new immutable array with the element at `index` replaced by `elem`.
     *
     *  If `elem` is a `Short`, the result is an `ofShort` backed by a new
     *  `Array[Short]`; otherwise the generic implementation is used and the result
     *  is backed by a boxed array.
     *
     *  @tparam B the element type of the returned immutable array, a supertype of `Short`
     *  @param index the zero-based index of the element to replace
     *  @param elem the value to store at `index`
     *  @throws IndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    override def updated[B >: Short](index: Int, elem: B): ArraySeq[B] =
      elem match {
        case b: Short => new ArraySeq.ofShort(unsafeArray.updated(index, b))
        case _ => super.updated(index, elem)
      }
    /** Returns a new immutable array with `elem` appended after all elements of
     *  this immutable array.
     *
     *  If `elem` is a `Short`, the result is an `ofShort` backed by a new
     *  `Array[Short]`; otherwise the generic implementation is used and the result
     *  is backed by a boxed array.
     *
     *  @tparam B the element type of the returned immutable array, a supertype of `Short`
     *  @param elem the element to append
     */
    override def appended[B >: Short](elem: B): ArraySeq[B] =
      elem match {
        case b: Short => new ArraySeq.ofShort(unsafeArray.appended(b))
        case _ => super.appended(elem)
      }
    /** Returns a new immutable array with `elem` prepended before all elements of
     *  this immutable array.
     *
     *  If `elem` is a `Short`, the result is an `ofShort` backed by a new
     *  `Array[Short]`; otherwise the generic implementation is used and the result
     *  is backed by a boxed array.
     *
     *  @tparam B the element type of the returned immutable array, a supertype of `Short`
     *  @param elem the element to prepend
     */
    override def prepended[B >: Short](elem: B): ArraySeq[B] =
      elem match {
        case b: Short => new ArraySeq.ofShort(unsafeArray.prepended(b))
        case _ => super.prepended(elem)
      }
  }

  /** An immutable `ArraySeq` backed by an `Array[Char]`, storing its elements
   *  unboxed.
   *
   *  The array given at construction is wrapped directly, not copied, and must not
   *  be mutated afterwards.
   *
   *  @param unsafeArray the underlying array, which must not be mutated after
   *                     wrapping
   */
  @SerialVersionUID(3L)
  final class ofChar(val unsafeArray: Array[Char]) extends ArraySeq[Char] {
    // Type erases to `ManifestFactory.CharManifest`, but can't annotate that because it's not accessible
    /** Returns `ClassTag.Char`, the tag of the unboxed element type. */
    protected def elemTag: ClassTag.Char.type = ClassTag.Char
    /** Returns the number of elements in this immutable array. */
    def length: Int = unsafeArray.length
    /** Returns the element at the given index of the underlying array.
     *
     *  @param i the zero-based index of the element
     *  @throws ArrayIndexOutOfBoundsException if `i` is negative or not less than
     *          `length`
     */
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Char = unsafeArray(i)
    /** Returns a hash code computed from the array elements, consistent with the
     *  hashing of other sequences.
     */
    override def hashCode() = MurmurHash3.arraySeqHash(unsafeArray)
    /** Compares this immutable array with another object for equality.
     *
     *  Two `ofChar` instances are equal if their underlying arrays contain the same
     *  elements in the same order. Comparison with anything else falls back to
     *  general sequence equality.
     *
     *  @param that the object to compare with
     *  @return `true` if `that` is an `ofChar` whose underlying array has the same
     *          elements in the same order, or is otherwise an equal sequence
     */
    override def equals(that: Any) = that match {
      case that: ofChar => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
    /** Returns an immutable array with the elements of this immutable array sorted
     *  according to the given ordering.
     *
     *  Returns this immutable array itself if it has fewer than two elements. If
     *  `ord` is `Ordering.Char`, a copy of the underlying array is sorted and
     *  wrapped in a new `ofChar`; for any other ordering the generic implementation
     *  is used, returning an immutable array of boxed values.
     *
     *  @tparam B the type on which `ord` is defined, a supertype of `Char`
     *  @param ord the ordering used to compare elements
     */
    override def sorted[B >: Char](implicit ord: Ordering[B]): ArraySeq[Char] =
      if(length <= 1) this
      else if(ord eq Ordering.Char) {
        val a = unsafeArray.clone()
        Arrays.sort(a)
        new ArraySeq.ofChar(a)
      } else super.sorted[B]
    /** Returns an iterator over the elements of the underlying array. */
    override def iterator: Iterator[Char] = new ArrayOps.ArrayIterator[Char](unsafeArray)
    /** Returns a stepper for the elements of this immutable array.
     *
     *  The stepper steps over the unboxed `Char` values, widened to `Int`, unless
     *  a reference-shaped stepper is explicitly requested, in which case the
     *  values are boxed.
     *
     *  @tparam S the type of the stepper, determined by `shape`
     *  @param shape implicit evidence selecting the stepper type for the element type `Char`
     *  @return a stepper over the elements, supporting efficient splitting for
     *          parallel processing
     */
    override def stepper[S <: Stepper[?]](implicit shape: StepperShape[Char, S]): S & EfficientSplit = (
      if(shape.shape == StepperShape.ReferenceShape)
        AnyStepper.ofParIntStepper(new WidenedCharArrayStepper(unsafeArray, 0, unsafeArray.length))
      else new WidenedCharArrayStepper(unsafeArray, 0, unsafeArray.length)
    ).asInstanceOf[S & EfficientSplit]
    /** Returns a new immutable array with the element at `index` replaced by `elem`.
     *
     *  If `elem` is a `Char`, the result is an `ofChar` backed by a new
     *  `Array[Char]`; otherwise the generic implementation is used and the result
     *  is backed by a boxed array.
     *
     *  @tparam B the element type of the returned immutable array, a supertype of `Char`
     *  @param index the zero-based index of the element to replace
     *  @param elem the value to store at `index`
     *  @throws IndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    override def updated[B >: Char](index: Int, elem: B): ArraySeq[B] =
      elem match {
        case b: Char => new ArraySeq.ofChar(unsafeArray.updated(index, b))
        case _ => super.updated(index, elem)
      }
    /** Returns a new immutable array with `elem` appended after all elements of
     *  this immutable array.
     *
     *  If `elem` is a `Char`, the result is an `ofChar` backed by a new
     *  `Array[Char]`; otherwise the generic implementation is used and the result
     *  is backed by a boxed array.
     *
     *  @tparam B the element type of the returned immutable array, a supertype of `Char`
     *  @param elem the element to append
     */
    override def appended[B >: Char](elem: B): ArraySeq[B] =
      elem match {
        case b: Char => new ArraySeq.ofChar(unsafeArray.appended(b))
        case _ => super.appended(elem)
      }
    /** Returns a new immutable array with `elem` prepended before all elements of
     *  this immutable array.
     *
     *  If `elem` is a `Char`, the result is an `ofChar` backed by a new
     *  `Array[Char]`; otherwise the generic implementation is used and the result
     *  is backed by a boxed array.
     *
     *  @tparam B the element type of the returned immutable array, a supertype of `Char`
     *  @param elem the element to prepend
     */
    override def prepended[B >: Char](elem: B): ArraySeq[B] =
      elem match {
        case b: Char => new ArraySeq.ofChar(unsafeArray.prepended(b))
        case _ => super.prepended(elem)
      }

    /** Appends the characters of this immutable array to a string builder, preceded
     *  by the string `start`, separated by the string `sep`, and followed by the
     *  string `end`.
     *
     *  Delegates to the implementation in [[scala.collection.mutable.ArraySeq.ofChar]],
     *  which is optimized for the underlying `Array[Char]`.
     *
     *  @param sb the string builder to append to
     *  @param start the starting string
     *  @param sep the separator string
     *  @param end the ending string
     *  @return the string builder `sb` to which elements were appended
     */
    override def addString(sb: StringBuilder, start: String, sep: String, end: String): sb.type =
      (new MutableArraySeq.ofChar(unsafeArray)).addString(sb, start, sep, end)
  }

  /** An immutable `ArraySeq` backed by an `Array[Int]`, storing its elements
   *  unboxed.
   *
   *  The array given at construction is wrapped directly, not copied, and must not
   *  be mutated afterwards.
   *
   *  @param unsafeArray the underlying array, which must not be mutated after
   *                     wrapping
   */
  @SerialVersionUID(3L)
  final class ofInt(val unsafeArray: Array[Int]) extends ArraySeq[Int] {
    // Type erases to `ManifestFactory.IntManifest`, but can't annotate that because it's not accessible
    /** Returns `ClassTag.Int`, the tag of the unboxed element type. */
    protected def elemTag: ClassTag.Int.type = ClassTag.Int
    /** Returns the number of elements in this immutable array. */
    def length: Int = unsafeArray.length
    /** Returns the element at the given index of the underlying array.
     *
     *  @param i the zero-based index of the element
     *  @throws ArrayIndexOutOfBoundsException if `i` is negative or not less than
     *          `length`
     */
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Int = unsafeArray(i)
    /** Returns a hash code computed from the array elements, consistent with the
     *  hashing of other sequences.
     */
    override def hashCode() = MurmurHash3.arraySeqHash(unsafeArray)
    /** Compares this immutable array with another object for equality.
     *
     *  Two `ofInt` instances are equal if their underlying arrays contain the same
     *  elements in the same order. Comparison with anything else falls back to
     *  general sequence equality.
     *
     *  @param that the object to compare with
     *  @return `true` if `that` is an `ofInt` whose underlying array has the same
     *          elements in the same order, or is otherwise an equal sequence
     */
    override def equals(that: Any) = that match {
      case that: ofInt => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
    /** Returns an immutable array with the elements of this immutable array sorted
     *  according to the given ordering.
     *
     *  Returns this immutable array itself if it has fewer than two elements. If
     *  `ord` is `Ordering.Int`, a copy of the underlying array is sorted and
     *  wrapped in a new `ofInt`; for any other ordering the generic implementation
     *  is used, returning an immutable array of boxed values.
     *
     *  @tparam B the type on which `ord` is defined, a supertype of `Int`
     *  @param ord the ordering used to compare elements
     */
    override def sorted[B >: Int](implicit ord: Ordering[B]): ArraySeq[Int] =
      if(length <= 1) this
      else if(ord eq Ordering.Int) {
        val a = unsafeArray.clone()
        Arrays.sort(a)
        new ArraySeq.ofInt(a)
      } else super.sorted[B]
    /** Returns an iterator over the elements of the underlying array. */
    override def iterator: Iterator[Int] = new ArrayOps.ArrayIterator[Int](unsafeArray)
    /** Returns a stepper for the elements of this immutable array.
     *
     *  The stepper steps over the unboxed `Int` values unless a reference-shaped
     *  stepper is explicitly requested, in which case the values are boxed.
     *
     *  @tparam S the type of the stepper, determined by `shape`
     *  @param shape implicit evidence selecting the stepper type for the element type `Int`
     *  @return a stepper over the elements, supporting efficient splitting for
     *          parallel processing
     */
    override def stepper[S <: Stepper[?]](implicit shape: StepperShape[Int, S]): S & EfficientSplit = (
      if(shape.shape == StepperShape.ReferenceShape)
        AnyStepper.ofParIntStepper(new IntArrayStepper(unsafeArray, 0, unsafeArray.length))
      else new IntArrayStepper(unsafeArray, 0, unsafeArray.length)
    ).asInstanceOf[S & EfficientSplit]
    /** Returns a new immutable array with the element at `index` replaced by `elem`.
     *
     *  If `elem` is an `Int`, the result is an `ofInt` backed by a new
     *  `Array[Int]`; otherwise the generic implementation is used and the result
     *  is backed by a boxed array.
     *
     *  @tparam B the element type of the returned immutable array, a supertype of `Int`
     *  @param index the zero-based index of the element to replace
     *  @param elem the value to store at `index`
     *  @throws IndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    override def updated[B >: Int](index: Int, elem: B): ArraySeq[B] =
      elem match {
        case b: Int => new ArraySeq.ofInt(unsafeArray.updated(index, b))
        case _ => super.updated(index, elem)
      }
    /** Returns a new immutable array with `elem` appended after all elements of
     *  this immutable array.
     *
     *  If `elem` is an `Int`, the result is an `ofInt` backed by a new
     *  `Array[Int]`; otherwise the generic implementation is used and the result
     *  is backed by a boxed array.
     *
     *  @tparam B the element type of the returned immutable array, a supertype of `Int`
     *  @param elem the element to append
     */
    override def appended[B >: Int](elem: B): ArraySeq[B] =
      elem match {
        case b: Int => new ArraySeq.ofInt(unsafeArray.appended(b))
        case _ => super.appended(elem)
      }
    /** Returns a new immutable array with `elem` prepended before all elements of
     *  this immutable array.
     *
     *  If `elem` is an `Int`, the result is an `ofInt` backed by a new
     *  `Array[Int]`; otherwise the generic implementation is used and the result
     *  is backed by a boxed array.
     *
     *  @tparam B the element type of the returned immutable array, a supertype of `Int`
     *  @param elem the element to prepend
     */
    override def prepended[B >: Int](elem: B): ArraySeq[B] =
      elem match {
        case b: Int => new ArraySeq.ofInt(unsafeArray.prepended(b))
        case _ => super.prepended(elem)
      }
  }

  /** An immutable `ArraySeq` backed by an `Array[Long]`, storing its elements
   *  unboxed.
   *
   *  The array given at construction is wrapped directly, not copied, and must not
   *  be mutated afterwards.
   *
   *  @param unsafeArray the underlying array, which must not be mutated after
   *                     wrapping
   */
  @SerialVersionUID(3L)
  final class ofLong(val unsafeArray: Array[Long]) extends ArraySeq[Long] {
    // Type erases to `ManifestFactory.LongManifest`, but can't annotate that because it's not accessible
    /** Returns `ClassTag.Long`, the tag of the unboxed element type. */
    protected def elemTag: ClassTag.Long.type = ClassTag.Long
    /** Returns the number of elements in this immutable array. */
    def length: Int = unsafeArray.length
    /** Returns the element at the given index of the underlying array.
     *
     *  @param i the zero-based index of the element
     *  @throws ArrayIndexOutOfBoundsException if `i` is negative or not less than
     *          `length`
     */
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Long = unsafeArray(i)
    /** Returns a hash code computed from the array elements, consistent with the
     *  hashing of other sequences.
     */
    override def hashCode() = MurmurHash3.arraySeqHash(unsafeArray)
    /** Compares this immutable array with another object for equality.
     *
     *  Two `ofLong` instances are equal if their underlying arrays contain the same
     *  elements in the same order. Comparison with anything else falls back to
     *  general sequence equality.
     *
     *  @param that the object to compare with
     *  @return `true` if `that` is an `ofLong` whose underlying array has the same
     *          elements in the same order, or is otherwise an equal sequence
     */
    override def equals(that: Any) = that match {
      case that: ofLong => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
    /** Returns an immutable array with the elements of this immutable array sorted
     *  according to the given ordering.
     *
     *  Returns this immutable array itself if it has fewer than two elements. If
     *  `ord` is `Ordering.Long`, a copy of the underlying array is sorted and
     *  wrapped in a new `ofLong`; for any other ordering the generic implementation
     *  is used, returning an immutable array of boxed values.
     *
     *  @tparam B the type on which `ord` is defined, a supertype of `Long`
     *  @param ord the ordering used to compare elements
     */
    override def sorted[B >: Long](implicit ord: Ordering[B]): ArraySeq[Long] =
      if(length <= 1) this
      else if(ord eq Ordering.Long) {
        val a = unsafeArray.clone()
        Arrays.sort(a)
        new ArraySeq.ofLong(a)
      } else super.sorted[B]
    /** Returns an iterator over the elements of the underlying array. */
    override def iterator: Iterator[Long] = new ArrayOps.ArrayIterator[Long](unsafeArray)
    /** Returns a stepper for the elements of this immutable array.
     *
     *  The stepper steps over the unboxed `Long` values unless a reference-shaped
     *  stepper is explicitly requested, in which case the values are boxed.
     *
     *  @tparam S the type of the stepper, determined by `shape`
     *  @param shape implicit evidence selecting the stepper type for the element type `Long`
     *  @return a stepper over the elements, supporting efficient splitting for
     *          parallel processing
     */
    override def stepper[S <: Stepper[?]](implicit shape: StepperShape[Long, S]): S & EfficientSplit = (
      if(shape.shape == StepperShape.ReferenceShape)
        AnyStepper.ofParLongStepper(new LongArrayStepper(unsafeArray, 0, unsafeArray.length))
      else new LongArrayStepper(unsafeArray, 0, unsafeArray.length)
    ).asInstanceOf[S & EfficientSplit]
    /** Returns a new immutable array with the element at `index` replaced by `elem`.
     *
     *  If `elem` is a `Long`, the result is an `ofLong` backed by a new
     *  `Array[Long]`; otherwise the generic implementation is used and the result
     *  is backed by a boxed array.
     *
     *  @tparam B the element type of the returned immutable array, a supertype of `Long`
     *  @param index the zero-based index of the element to replace
     *  @param elem the value to store at `index`
     *  @throws IndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    override def updated[B >: Long](index: Int, elem: B): ArraySeq[B] =
      elem match {
        case b: Long => new ArraySeq.ofLong(unsafeArray.updated(index, b))
        case _ => super.updated(index, elem)
      }
    /** Returns a new immutable array with `elem` appended after all elements of
     *  this immutable array.
     *
     *  If `elem` is a `Long`, the result is an `ofLong` backed by a new
     *  `Array[Long]`; otherwise the generic implementation is used and the result
     *  is backed by a boxed array.
     *
     *  @tparam B the element type of the returned immutable array, a supertype of `Long`
     *  @param elem the element to append
     */
    override def appended[B >: Long](elem: B): ArraySeq[B] =
      elem match {
        case b: Long => new ArraySeq.ofLong(unsafeArray.appended(b))
        case _ => super.appended(elem)
      }
    /** Returns a new immutable array with `elem` prepended before all elements of
     *  this immutable array.
     *
     *  If `elem` is a `Long`, the result is an `ofLong` backed by a new
     *  `Array[Long]`; otherwise the generic implementation is used and the result
     *  is backed by a boxed array.
     *
     *  @tparam B the element type of the returned immutable array, a supertype of `Long`
     *  @param elem the element to prepend
     */
    override def prepended[B >: Long](elem: B): ArraySeq[B] =
      elem match {
        case b: Long => new ArraySeq.ofLong(unsafeArray.prepended(b))
        case _ => super.prepended(elem)
      }
  }

  /** An immutable `ArraySeq` backed by an `Array[Float]`, storing its elements
   *  unboxed.
   *
   *  The array given at construction is wrapped directly, not copied, and must not
   *  be mutated afterwards.
   *
   *  @param unsafeArray the underlying array, which must not be mutated after
   *                     wrapping
   */
  @SerialVersionUID(3L)
  final class ofFloat(val unsafeArray: Array[Float]) extends ArraySeq[Float] {
    // Type erases to `ManifestFactory.FloatManifest`, but can't annotate that because it's not accessible
    /** Returns `ClassTag.Float`, the tag of the unboxed element type. */
    protected def elemTag: ClassTag.Float.type = ClassTag.Float
    /** Returns the number of elements in this immutable array. */
    def length: Int = unsafeArray.length
    /** Returns the element at the given index of the underlying array.
     *
     *  @param i the zero-based index of the element
     *  @throws ArrayIndexOutOfBoundsException if `i` is negative or not less than
     *          `length`
     */
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Float = unsafeArray(i)
    /** Returns a hash code computed from the array elements, consistent with the
     *  hashing of other sequences.
     */
    override def hashCode() = MurmurHash3.arraySeqHash(unsafeArray)
    /** Compares this immutable array with another object for equality.
     *
     *  Two `ofFloat` instances are equal if they share the same underlying array,
     *  or if their underlying arrays contain the same elements in the same order,
     *  compared with `==`. A `NaN` element is not `==` to anything, so two
     *  distinct arrays containing `NaN` are never equal. Comparison with anything
     *  else falls back to general sequence equality.
     *
     *  @param that the object to compare with
     *  @return `true` if `that` is an `ofFloat` sharing this underlying array or
     *          holding the same elements in the same order, or is otherwise an
     *          equal sequence
     */
    override def equals(that: Any) = that match {
      case that: ofFloat =>
        val array = unsafeArray
        val thatArray = that.unsafeArray
        (array eq thatArray) || array.length == thatArray.length && {
          var i = 0
          while (i < array.length && array(i) == thatArray(i)) i += 1
          i >= array.length
        }
      case _ => super.equals(that)
    }
    /** Returns an iterator over the elements of the underlying array. */
    override def iterator: Iterator[Float] = new ArrayOps.ArrayIterator[Float](unsafeArray)
    /** Returns a stepper for the elements of this immutable array.
     *
     *  The stepper steps over the unboxed `Float` values, widened to `Double`,
     *  unless a reference-shaped stepper is explicitly requested, in which case
     *  the values are boxed.
     *
     *  @tparam S the type of the stepper, determined by `shape`
     *  @param shape implicit evidence selecting the stepper type for the element type `Float`
     *  @return a stepper over the elements, supporting efficient splitting for
     *          parallel processing
     */
    override def stepper[S <: Stepper[?]](implicit shape: StepperShape[Float, S]): S & EfficientSplit = (
      if(shape.shape == StepperShape.ReferenceShape)
        AnyStepper.ofParDoubleStepper(new WidenedFloatArrayStepper(unsafeArray, 0, unsafeArray.length))
      else new WidenedFloatArrayStepper(unsafeArray, 0, unsafeArray.length)
    ).asInstanceOf[S & EfficientSplit]
    /** Returns a new immutable array with the element at `index` replaced by `elem`.
     *
     *  If `elem` is a `Float`, the result is an `ofFloat` backed by a new
     *  `Array[Float]`; otherwise the generic implementation is used and the result
     *  is backed by a boxed array.
     *
     *  @tparam B the element type of the returned immutable array, a supertype of `Float`
     *  @param index the zero-based index of the element to replace
     *  @param elem the value to store at `index`
     *  @throws IndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    override def updated[B >: Float](index: Int, elem: B): ArraySeq[B] =
      elem match {
        case b: Float => new ArraySeq.ofFloat(unsafeArray.updated(index, b))
        case _ => super.updated(index, elem)
      }
    /** Returns a new immutable array with `elem` appended after all elements of
     *  this immutable array.
     *
     *  If `elem` is a `Float`, the result is an `ofFloat` backed by a new
     *  `Array[Float]`; otherwise the generic implementation is used and the result
     *  is backed by a boxed array.
     *
     *  @tparam B the element type of the returned immutable array, a supertype of `Float`
     *  @param elem the element to append
     */
    override def appended[B >: Float](elem: B): ArraySeq[B] =
      elem match {
        case b: Float => new ArraySeq.ofFloat(unsafeArray.appended(b))
        case _ => super.appended(elem)
      }
    /** Returns a new immutable array with `elem` prepended before all elements of
     *  this immutable array.
     *
     *  If `elem` is a `Float`, the result is an `ofFloat` backed by a new
     *  `Array[Float]`; otherwise the generic implementation is used and the result
     *  is backed by a boxed array.
     *
     *  @tparam B the element type of the returned immutable array, a supertype of `Float`
     *  @param elem the element to prepend
     */
    override def prepended[B >: Float](elem: B): ArraySeq[B] =
      elem match {
        case b: Float => new ArraySeq.ofFloat(unsafeArray.prepended(b))
        case _ => super.prepended(elem)
      }
  }

  /** An immutable `ArraySeq` backed by an `Array[Double]`, storing its elements
   *  unboxed.
   *
   *  The array given at construction is wrapped directly, not copied, and must not
   *  be mutated afterwards.
   *
   *  @param unsafeArray the underlying array, which must not be mutated after
   *                     wrapping
   */
  @SerialVersionUID(3L)
  final class ofDouble(val unsafeArray: Array[Double]) extends ArraySeq[Double] {
    // Type erases to `ManifestFactory.DoubleManifest`, but can't annotate that because it's not accessible
    /** Returns `ClassTag.Double`, the tag of the unboxed element type. */
    protected def elemTag: ClassTag.Double.type = ClassTag.Double
    /** Returns the number of elements in this immutable array. */
    def length: Int = unsafeArray.length
    /** Returns the element at the given index of the underlying array.
     *
     *  @param i the zero-based index of the element
     *  @throws ArrayIndexOutOfBoundsException if `i` is negative or not less than
     *          `length`
     */
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Double = unsafeArray(i)
    /** Returns a hash code computed from the array elements, consistent with the
     *  hashing of other sequences.
     */
    override def hashCode() = MurmurHash3.arraySeqHash(unsafeArray)
    /** Compares this immutable array with another object for equality.
     *
     *  Two `ofDouble` instances are equal if they share the same underlying array,
     *  or if their underlying arrays contain the same elements in the same order,
     *  compared with `==`. A `NaN` element is not `==` to anything, so two
     *  distinct arrays containing `NaN` are never equal. Comparison with anything
     *  else falls back to general sequence equality.
     *
     *  @param that the object to compare with
     *  @return `true` if `that` is an `ofDouble` sharing this underlying array or
     *          holding the same elements in the same order, or is otherwise an
     *          equal sequence
     */
    override def equals(that: Any) = that match {
      case that: ofDouble =>
        val array = unsafeArray
        val thatArray = that.unsafeArray
        (array eq thatArray) || array.length == thatArray.length && {
          var i = 0
          while (i < array.length && array(i) == thatArray(i)) i += 1
          i >= array.length
        }
      case _ => super.equals(that)
    }
    /** Returns an iterator over the elements of the underlying array. */
    override def iterator: Iterator[Double] = new ArrayOps.ArrayIterator[Double](unsafeArray)
    /** Returns a stepper for the elements of this immutable array.
     *
     *  The stepper steps over the unboxed `Double` values unless a
     *  reference-shaped stepper is explicitly requested, in which case the
     *  values are boxed.
     *
     *  @tparam S the type of the stepper, determined by `shape`
     *  @param shape implicit evidence selecting the stepper type for the element type `Double`
     *  @return a stepper over the elements, supporting efficient splitting for
     *          parallel processing
     */
    override def stepper[S <: Stepper[?]](implicit shape: StepperShape[Double, S]): S & EfficientSplit = (
      if(shape.shape == StepperShape.ReferenceShape)
        AnyStepper.ofParDoubleStepper(new DoubleArrayStepper(unsafeArray, 0, unsafeArray.length))
      else new DoubleArrayStepper(unsafeArray, 0, unsafeArray.length)
    ).asInstanceOf[S & EfficientSplit]
    /** Returns a new immutable array with the element at `index` replaced by `elem`.
     *
     *  If `elem` is a `Double`, the result is an `ofDouble` backed by a new
     *  `Array[Double]`; otherwise the generic implementation is used and the result
     *  is backed by a boxed array.
     *
     *  @tparam B the element type of the returned immutable array, a supertype of `Double`
     *  @param index the zero-based index of the element to replace
     *  @param elem the value to store at `index`
     *  @throws IndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    override def updated[B >: Double](index: Int, elem: B): ArraySeq[B] =
      elem match {
        case b: Double => new ArraySeq.ofDouble(unsafeArray.updated(index, b))
        case _ => super.updated(index, elem)
      }
    /** Returns a new immutable array with `elem` appended after all elements of
     *  this immutable array.
     *
     *  If `elem` is a `Double`, the result is an `ofDouble` backed by a new
     *  `Array[Double]`; otherwise the generic implementation is used and the result
     *  is backed by a boxed array.
     *
     *  @tparam B the element type of the returned immutable array, a supertype of `Double`
     *  @param elem the element to append
     */
    override def appended[B >: Double](elem: B): ArraySeq[B] =
      elem match {
        case b: Double => new ArraySeq.ofDouble(unsafeArray.appended(b))
        case _ => super.appended(elem)
      }
    /** Returns a new immutable array with `elem` prepended before all elements of
     *  this immutable array.
     *
     *  If `elem` is a `Double`, the result is an `ofDouble` backed by a new
     *  `Array[Double]`; otherwise the generic implementation is used and the result
     *  is backed by a boxed array.
     *
     *  @tparam B the element type of the returned immutable array, a supertype of `Double`
     *  @param elem the element to prepend
     */
    override def prepended[B >: Double](elem: B): ArraySeq[B] =
      elem match {
        case b: Double => new ArraySeq.ofDouble(unsafeArray.prepended(b))
        case _ => super.prepended(elem)
      }
  }

  /** An immutable `ArraySeq` backed by an `Array[Boolean]`, storing its elements
   *  unboxed.
   *
   *  The array given at construction is wrapped directly, not copied, and must not
   *  be mutated afterwards.
   *
   *  @param unsafeArray the underlying array, which must not be mutated after
   *                     wrapping
   */
  @SerialVersionUID(3L)
  final class ofBoolean(val unsafeArray: Array[Boolean]) extends ArraySeq[Boolean] {
    // Type erases to `ManifestFactory.BooleanManifest`, but can't annotate that because it's not accessible
    /** Returns `ClassTag.Boolean`, the tag of the unboxed element type. */
    protected def elemTag: ClassTag.Boolean.type = ClassTag.Boolean
    /** Returns the number of elements in this immutable array. */
    def length: Int = unsafeArray.length
    /** Returns the element at the given index of the underlying array.
     *
     *  @param i the zero-based index of the element
     *  @throws ArrayIndexOutOfBoundsException if `i` is negative or not less than
     *          `length`
     */
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Boolean = unsafeArray(i)
    /** Returns a hash code computed from the array elements, consistent with the
     *  hashing of other sequences.
     */
    override def hashCode() = MurmurHash3.arraySeqHash(unsafeArray)
    /** Compares this immutable array with another object for equality.
     *
     *  Two `ofBoolean` instances are equal if their underlying arrays contain the
     *  same elements in the same order. Comparison with anything else falls back to
     *  general sequence equality.
     *
     *  @param that the object to compare with
     *  @return `true` if `that` is an `ofBoolean` whose underlying array has the
     *          same elements in the same order, or is otherwise an equal sequence
     */
    override def equals(that: Any) = that match {
      case that: ofBoolean => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
    /** Returns an immutable array with the elements of this immutable array sorted
     *  according to the given ordering.
     *
     *  Returns this immutable array itself if it has fewer than two elements. If
     *  `ord` is `Ordering.Boolean`, a copy of the underlying array is sorted (`false`
     *  before `true`) and wrapped in a new `ofBoolean`; for any other ordering the
     *  generic implementation is used, returning an immutable array of boxed values.
     *
     *  @tparam B the type on which `ord` is defined, a supertype of `Boolean`
     *  @param ord the ordering used to compare elements
     */
    override def sorted[B >: Boolean](implicit ord: Ordering[B]): ArraySeq[Boolean] =
      if(length <= 1) this
      else if(ord eq Ordering.Boolean) {
        val a = unsafeArray.clone()
        Sorting.stableSort(a)
        new ArraySeq.ofBoolean(a)
      } else super.sorted[B]
    /** Returns an iterator over the elements of the underlying array. */
    override def iterator: Iterator[Boolean] = new ArrayOps.ArrayIterator[Boolean](unsafeArray)
    /** Returns a stepper for the elements of this immutable array.
     *
     *  There is no primitive stepper shape for `Boolean`, so the stepper always
     *  steps over boxed values, and `shape` is never consulted.
     *
     *  @tparam S the type of the stepper, determined by `shape`
     *  @param shape implicit evidence selecting the stepper type for the element type
     *               `Boolean`; never used at runtime
     *  @return a stepper over the elements, supporting efficient splitting for
     *          parallel processing
     */
    override def stepper[S <: Stepper[?]](implicit shape: StepperShape[Boolean, S]): S & EfficientSplit =
      new BoxedBooleanArrayStepper(unsafeArray, 0, unsafeArray.length).asInstanceOf[S & EfficientSplit]
    /** Returns a new immutable array with the element at `index` replaced by `elem`.
     *
     *  If `elem` is a `Boolean`, the result is an `ofBoolean` backed by a new
     *  `Array[Boolean]`; otherwise the generic implementation is used and the result
     *  is backed by a boxed array.
     *
     *  @tparam B the element type of the returned immutable array, a supertype of `Boolean`
     *  @param index the zero-based index of the element to replace
     *  @param elem the value to store at `index`
     *  @throws IndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    override def updated[B >: Boolean](index: Int, elem: B): ArraySeq[B] =
      elem match {
        case b: Boolean => new ArraySeq.ofBoolean(unsafeArray.updated(index, b))
        case _ => super.updated(index, elem)
      }
    /** Returns a new immutable array with `elem` appended after all elements of
     *  this immutable array.
     *
     *  If `elem` is a `Boolean`, the result is an `ofBoolean` backed by a new
     *  `Array[Boolean]`; otherwise the generic implementation is used and the result
     *  is backed by a boxed array.
     *
     *  @tparam B the element type of the returned immutable array, a supertype of `Boolean`
     *  @param elem the element to append
     */
    override def appended[B >: Boolean](elem: B): ArraySeq[B] =
      elem match {
        case b: Boolean => new ArraySeq.ofBoolean(unsafeArray.appended(b))
        case _ => super.appended(elem)
      }
    /** Returns a new immutable array with `elem` prepended before all elements of
     *  this immutable array.
     *
     *  If `elem` is a `Boolean`, the result is an `ofBoolean` backed by a new
     *  `Array[Boolean]`; otherwise the generic implementation is used and the result
     *  is backed by a boxed array.
     *
     *  @tparam B the element type of the returned immutable array, a supertype of `Boolean`
     *  @param elem the element to prepend
     */
    override def prepended[B >: Boolean](elem: B): ArraySeq[B] =
      elem match {
        case b: Boolean => new ArraySeq.ofBoolean(unsafeArray.prepended(b))
        case _ => super.prepended(elem)
      }
  }

  /** An immutable `ArraySeq` backed by an `Array[Unit]` (at runtime, an array of
   *  boxed unit values).
   *
   *  The array given at construction is wrapped directly, not copied, and must not
   *  be mutated afterwards.
   *
   *  @param unsafeArray the underlying array, which must not be mutated after
   *                     wrapping
   */
  @SerialVersionUID(3L)
  final class ofUnit(val unsafeArray: Array[Unit]) extends ArraySeq[Unit] {
    // Type erases to `ManifestFactory.UnitManifest`, but can't annotate that because it's not accessible
    /** Returns `ClassTag.Unit`, the tag of the element type. */
    protected def elemTag: ClassTag.Unit.type = ClassTag.Unit
    /** Returns the number of elements in this immutable array. */
    def length: Int = unsafeArray.length
    /** Returns the unit value at the given index of the underlying array.
     *
     *  @param i the zero-based index of the element
     *  @throws ArrayIndexOutOfBoundsException if `i` is negative or not less than
     *          `length`
     */
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Unit = unsafeArray(i)
    /** Returns a hash code computed from the array elements, consistent with the
     *  hashing of other sequences.
     */
    override def hashCode() = MurmurHash3.arraySeqHash(unsafeArray)
    /** Compares this immutable array with another object for equality.
     *
     *  Two `ofUnit` instances are equal if their underlying arrays have the same
     *  length, since all unit values are equal. Comparison with anything else
     *  falls back to general sequence equality.
     *
     *  @param that the object to compare with
     *  @return `true` if `that` is an `ofUnit` of the same length, or is otherwise
     *          an equal sequence
     */
    override def equals(that: Any) = that match {
      case that: ofUnit => unsafeArray.length == that.unsafeArray.length
      case _ => super.equals(that)
    }
    /** Returns an iterator over the elements of the underlying array. */
    override def iterator: Iterator[Unit] = new ArrayOps.ArrayIterator[Unit](unsafeArray)
    /** Returns a stepper for the elements of this immutable array.
     *
     *  There is no primitive stepper shape for `Unit`, so the stepper always steps
     *  over the boxed unit values, and `shape` is never consulted.
     *
     *  @tparam S the type of the stepper, determined by `shape`
     *  @param shape implicit evidence selecting the stepper type for the element type
     *               `Unit`; never used at runtime
     *  @return a stepper over the elements, supporting efficient splitting for
     *          parallel processing
     */
    override def stepper[S <: Stepper[?]](implicit shape: StepperShape[Unit, S]): S & EfficientSplit =
      new ObjectArrayStepper[AnyRef](unsafeArray.asInstanceOf[Array[AnyRef]], 0, unsafeArray.length).asInstanceOf[S & EfficientSplit]
  }
}
