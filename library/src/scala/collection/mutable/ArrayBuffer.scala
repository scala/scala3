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
import java.util.Arrays
import scala.annotation.{nowarn, tailrec}
import scala.collection.Stepper.EfficientSplit
import scala.collection.generic.{CommonErrors, DefaultSerializable}
import scala.runtime.PStatics.VM_MaxArraySize

/** An implementation of the `Buffer` class using an array to
 *  represent the assembled sequence internally. Append, update and random
 *  access take constant time (amortized time). Prepends and removes are
 *  linear in the buffer size.
 *
 *  @see ["Scala's Collection Library overview"](https://docs.scala-lang.org/overviews/collections-2.13/concrete-mutable-collection-classes.html#array-buffers)
 *  section on `Array Buffers` for more information.
 *
 *  @tparam A    the type of this arraybuffer's elements.
 *
 *  @define Coll `mutable.ArrayBuffer`
 *  @define coll array buffer
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
@SerialVersionUID(-1582447879429021880L)
class ArrayBuffer[A] private (initialElements: Array[AnyRef], initialSize: Int)
  extends AbstractBuffer[A]
    with IndexedBuffer[A]
    with IndexedSeqOps[A, ArrayBuffer, ArrayBuffer[A]]
    with StrictOptimizedSeqOps[A, ArrayBuffer, ArrayBuffer[A]]
    with IterableFactoryDefaults[A, ArrayBuffer]
    with DefaultSerializable {

  /** Creates an empty array buffer whose backing array has the default initial
   *  capacity of `ArrayBuffer.DefaultInitialSize` (16) elements.
   */
  def this() = this(new Array[AnyRef](ArrayBuffer.DefaultInitialSize), 0)

  /** Creates an empty array buffer with the given initial capacity.
   *
   *  @param initialSize the initial capacity of the backing array, not the size
   *                     of the buffer, which starts empty; values less than 1
   *                     are treated as 1
   */
  def this(initialSize: Int) = this(new Array[AnyRef](initialSize max 1), 0)

  @transient private var mutationCount: Int = 0

  // needs to be `private[collection]` or `protected[collection]` for parallel-collections
  /** The backing array, holding the buffer's elements at indices `0` until
   *  `size0`, erased to `AnyRef`. The remaining slots are unused and `null`.
   */
  protected[collection] var array: Array[AnyRef] = initialElements
  /** The number of elements in this buffer. */
  protected var size0 = initialSize

  /** Returns a stepper for the elements of this buffer, which enables creating
   *  a Java stream over them (see [[scala.jdk.StreamConverters]]).
   *
   *  The stepper reads the backing array directly and performs no mutation
   *  checking.
   *
   *  @tparam S the type of the stepper
   *  @param shape the stepper shape corresponding to the element type
   *  @return a stepper, additionally marked `EfficientSplit` as it splits
   *          efficiently for parallel processing
   */
  override def stepper[S <: Stepper[?]](implicit shape: StepperShape[A, S]): S & EfficientSplit = {
    import scala.collection.convert.impl._
    shape.parUnbox(new ObjectArrayStepper(array, 0, length).asInstanceOf[AnyStepper[A] & EfficientSplit])
  }

  /** The number of elements in this $coll. Never `-1`, as the size is always known.
   *
   *  Overridden to select `IndexedSeqOps`' implementation, which returns `length`.
   */
  override def knownSize: Int = super[IndexedSeqOps].knownSize

  /** Ensures that the internal array has at least `n` cells.
   *
   *  @param n the minimum number of cells required in the internal array
   */
  protected def ensureSize(n: Int): Unit = {
    array = ArrayBuffer.ensureSize(array, size0, n)
  }

  /** Uses the given size to resize internal storage, if necessary.
   *
   *  @param size Expected maximum number of elements.
   */
  def sizeHint(size: Int): Unit =
    if(size > length && size >= 1) ensureSize(size)

  /** Reduces length to `n`, nulling out all dropped elements.
   *
   *  @param n the new size of the buffer, must be less than or equal to the current size
   */
  private def reduceToSize(n: Int): Unit = {
    mutationCount += 1
    Arrays.fill(array, n, size0, null)
    size0 = n
  }

  /** Trims the ArrayBuffer to an appropriate size for the current
   *  number of elements (rounding up to the next natural size),
   *  which may replace the array by a shorter one.
   *  This allows releasing some unused memory.
   */
  def trimToSize(): Unit = {
    resize(length)
  }

  /** Trims the `array` buffer size down to either a power of 2
   *  or Int.MaxValue while keeping first `requiredLength` elements.
   *
   *  @param requiredLength the number of elements to retain in the resized array
   */
  private def resize(requiredLength: Int): Unit =
    array = ArrayBuffer.downsize(array, requiredLength)

  @inline private def checkWithinBounds(lo: Int, hi: Int) = {
    if (lo < 0) throw CommonErrors.indexOutOfBounds(index = lo, max = size0 - 1)
    if (hi > size0) throw CommonErrors.indexOutOfBounds(index = hi - 1, max = size0 - 1)
  }

  /** Returns the element of this buffer at index `n`, in constant time.
   *
   *  @param n the index of the element to return
   *  @return the element at index `n`
   *  @throws IndexOutOfBoundsException if `n < 0` or `n >= length`
   */
  def apply(n: Int): A = {
    checkWithinBounds(n, n + 1)
    array(n).asInstanceOf[A]
  }

  /** Replaces the element at index `index` with `elem`, in constant time.
   *
   *  @param index the index of the element to replace
   *  @param elem the new element
   *  @throws IndexOutOfBoundsException if `index < 0` or `index >= length`
   */
  def update(@deprecatedName("n", "2.13.0") index: Int, elem: A): Unit = {
    checkWithinBounds(index, index + 1)
    mutationCount += 1
    array(index) = elem.asInstanceOf[AnyRef]
  }

  /** Returns the number of elements in this buffer. */
  def length = size0

  // TODO: return `IndexedSeqView` rather than `ArrayBufferView`
  /** Returns a view over the elements of this buffer.
   *
   *  Iterators obtained from the view, directly or through further view
   *  transformations, throw a [[java.util.ConcurrentModificationException]]
   *  when this buffer is mutated after their creation.
   */
  override def view: ArrayBufferView[A] = new ArrayBufferView(this, () => mutationCount)

  /** The companion object `ArrayBuffer`, used by transformation methods to build new array buffers. */
  override def iterableFactory: SeqFactory[ArrayBuffer] = ArrayBuffer

  /** Note: This does not actually resize the internal representation.
   *  See clearAndShrink if you want to also resize internally
   */
  def clear(): Unit = reduceToSize(0)

  /** Clears this buffer and shrinks to @param size (rounding up to the next
   *  natural size)
   *  @param size
   *  @return this `ArrayBuffer`, now empty and resized to a capacity at least as large as `size`
   */
  def clearAndShrink(size: Int = ArrayBuffer.DefaultInitialSize): this.type = {
    clear()
    resize(size)
    this
  }

  /** Appends `elem` to this buffer.
   *
   *  Takes amortized constant time: when the backing array is full, it is
   *  replaced by one at least twice as large (until the VM's maximum array
   *  size is reached).
   *
   *  @param elem the element to append
   *  @return this $coll
   */
  def addOne(elem: A): this.type = {
    mutationCount += 1
    val newSize = size0 + 1
    if(array.length <= newSize - 1) ensureSize(newSize)
    size0 = newSize
    array(newSize - 1) = elem.asInstanceOf[AnyRef]
    this
  }

  // Overridden to use array copying for efficiency where possible.
  /** Appends all elements of `elems` to this buffer.
   *
   *  When `elems` is itself an `ArrayBuffer`, its elements are copied in bulk
   *  with a single array copy. `elems` may be this buffer itself.
   *
   *  @param elems the collection containing the elements to append
   *  @return this $coll
   */
  override def addAll(elems: IterableOnce[A]^): this.type = {
    elems match {
      case elems: ArrayBuffer[?] =>
        val elemsLength = elems.size0
        if (elemsLength > 0) {
          mutationCount += 1
          ensureSize(size0 + elemsLength)
          Array.copy(elems.array, 0, array, length, elemsLength)
          size0 = length + elemsLength
        }
      case _ => super.addAll(elems)
    }
    this
  }

  /** Inserts `elem` at index `index` into this buffer.
   *
   *  The elements at indices `index` and above are shifted one position towards
   *  the end, taking time linear in `length - index`. If `index == length`, the
   *  element is appended.
   *
   *  @param index the index where the element is inserted
   *  @param elem the element to insert
   *  @throws IndexOutOfBoundsException if the index `index` is not in the valid range
   *          `0 <= index <= length`
   */
  def insert(@deprecatedName("n", "2.13.0") index: Int, elem: A): Unit = {
    checkWithinBounds(index, index)
    mutationCount += 1
    ensureSize(size0 + 1)
    Array.copy(array, index, array, index + 1, size0 - index)
    size0 += 1
    this(index) = elem
  }

  /** Prepends `elem` to this buffer.
   *
   *  Takes time linear in the buffer size, as all existing elements are shifted
   *  one position towards the end.
   *
   *  @param elem the element to prepend
   *  @return this $coll
   */
  def prepend(elem: A): this.type = {
    insert(0, elem)
    this
  }

  /** Inserts all elements of `elems` at index `index` into this buffer.
   *
   *  The elements at indices `index` and above are shifted towards the end to
   *  make room. If `elems` is a `collection.Iterable`, its elements are copied
   *  in directly, which is safe even when `elems` is this buffer itself; any
   *  other `IterableOnce` is first copied into a temporary array buffer.
   *
   *  @param index the index where the elements are inserted
   *  @param elems the collection containing the elements to insert
   *  @throws IndexOutOfBoundsException if the index `index` is not in the valid range
   *          `0 <= index <= length`
   */
  def insertAll(@deprecatedName("n", "2.13.0") index: Int, elems: IterableOnce[A]^): Unit = {
    checkWithinBounds(index, index)
    elems match {
      case elems: collection.Iterable[A @unchecked] =>
        val elemsLength = elems.size
        if (elemsLength > 0) {
          mutationCount += 1
          ensureSize(size0 + elemsLength)
          val len = size0
          Array.copy(array, index, array, index + elemsLength, len - index)
          // if `elems eq this`, this copy is safe because
          //   - `elems.array eq this.array`
          //   - we didn't overwrite the values being inserted after moving them in
          //     the previous line
          //   - `copyElemsToArray` will call `System.arraycopy`
          //   - `System.arraycopy` will effectively "read" all the values before
          //     overwriting any of them when two arrays are the same reference
          val actual = IterableOnce.copyElemsToArray(elems, array.asInstanceOf[Array[Any]], index, elemsLength)
          if (actual != elemsLength) throw new IllegalStateException(s"Copied $actual of $elemsLength")
          size0 = len + elemsLength // update size AFTER the copy, in case we're inserting a proxy
        }
      case _ => insertAll(index, ArrayBuffer.from(elems))
    }
  }

  /** Note: This does not actually resize the internal representation.
   *  See trimToSize if you want to also resize internally
   *
   *  @param index the zero-based position of the element to remove
   *  @return the element that was removed at position `index`
   */
  def remove(@deprecatedName("n", "2.13.0") index: Int): A = {
    checkWithinBounds(index, index + 1)
    val res = this(index)
    Array.copy(array, index + 1, array, index, size0 - (index + 1))
    reduceToSize(size0 - 1)
    res
  }

  /** Note: This does not actually resize the internal representation.
   *  See trimToSize if you want to also resize internally
   *
   *  @param index the zero-based position of the first element to remove
   *  @param count the number of elements to remove
   *  @throws IllegalArgumentException if `count` is negative
   *  @throws IndexOutOfBoundsException if the range `[index, index + count)` is out of bounds
   */
  def remove(@deprecatedName("n", "2.13.0") index: Int, count: Int): Unit =
    if (count > 0) {
      checkWithinBounds(index, index + count)
      Array.copy(array, index + count, array, index, size0 - (index + count))
      reduceToSize(size0 - count)
    } else if (count < 0) {
      throw new IllegalArgumentException("removing negative number of elements: " + count)
    }

  /** Returns this buffer itself, not a copy of it. */
  @deprecated("Use 'this' instance instead", "2.13.0")
  @deprecatedOverriding("ArrayBuffer[A] no longer extends Builder[A, ArrayBuffer[A]]", "2.13.0")
  @inline def result(): this.type = this

  /** Returns a new builder that appends its elements to this buffer and applies
   *  `f` to this buffer when its `result()` method is called.
   *
   *  @tparam NewTo the result type of `f`
   *  @param f the function applied to this buffer to obtain the builder's result
   *  @return a builder backed by this buffer, with result `f` applied to the buffer
   */
  @deprecated("Use 'new GrowableBuilder(this).mapResult(f)' instead", "2.13.0")
  @deprecatedOverriding("ArrayBuffer[A] no longer extends Builder[A, ArrayBuffer[A]]", "2.13.0")
  @inline def mapResult[NewTo](f: (ArrayBuffer[A]) => NewTo): Builder[A, NewTo]^{f} = new GrowableBuilder[A, ArrayBuffer[A]](this).mapResult(f)

  /** The prefix of this $coll's `toString` representation, `"ArrayBuffer"`. */
  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected def stringPrefix = "ArrayBuffer"

  /** Copies elements of this buffer to an array, as a single bulk array copy.
   *
   *  Copying starts at index `start` of the destination and stops when the end
   *  of this buffer, the end of `xs`, or the limit `len` is reached, whichever
   *  comes first.
   *
   *  @tparam B the element type of the destination array
   *  @param xs the destination array
   *  @param start the index of `xs` at which to start copying
   *  @param len the maximum number of elements to copy
   *  @return the number of elements copied
   */
  override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Int = {
    val copied = IterableOnce.elemsToCopyToArray(length, xs.length, start, len)
    if(copied > 0) {
      Array.copy(array, 0, xs, start, copied)
    }
    copied
  }

  /** Sorts this $coll in place according to an Ordering.
   *
   *  @see [[scala.collection.mutable.IndexedSeqOps.sortInPlace]]
   *  @tparam B a supertype of the element type `A` for which an `Ordering` is available
   *  @param  ord the ordering to be used to compare elements.
   *  @return modified input $coll sorted according to the ordering `ord`.
   */
  override def sortInPlace[B >: A]()(implicit ord: Ordering[B]): this.type = {
    if (length > 1) {
      mutationCount += 1
      scala.util.Sorting.stableSort(array.asInstanceOf[Array[B]], 0, length)
    }
    this
  }

  @tailrec private def foldl[B](start: Int, end: Int, z: B, op: (B, A) => B): B =
    if (start == end) z
    else foldl(start + 1, end, op(z, array(start).asInstanceOf[A]), op)

  @tailrec private def foldr[B](start: Int, end: Int, z: B, op: (A, B) => B): B =
    if (start == end) z
    else foldr(start, end - 1, op(array(end - 1).asInstanceOf[A], z), op)

  /** Applies the binary operator `op` to the start value `z` and all elements
   *  of this buffer, going left to right.
   *
   *  Overridden to read the backing array directly.
   *
   *  @tparam B the result type of the operator
   *  @param z the start value
   *  @param op the binary operator
   *  @return the result of inserting `op` between consecutive elements, going
   *          left to right with start value `z` on the left, or `z` if this
   *          buffer is empty
   */
  override def foldLeft[B](z: B)(op: (B, A) => B): B = foldl(0, length, z, op)

  /** Applies the binary operator `op` to all elements of this buffer and the
   *  start value `z`, going right to left.
   *
   *  Overridden to read the backing array directly.
   *
   *  @tparam B the result type of the operator
   *  @param z the start value
   *  @param op the binary operator
   *  @return the result of inserting `op` between consecutive elements, going
   *          right to left with start value `z` on the right, or `z` if this
   *          buffer is empty
   */
  override def foldRight[B](z: B)(op: (A, B) => B): B = foldr(0, length, z, op)

  /** Applies the binary operator `op` to all elements of this buffer, going
   *  left to right.
   *
   *  Overridden to read the backing array directly.
   *
   *  @tparam B the result type of the operator, a supertype of the element type
   *  @param op the binary operator
   *  @return the result of inserting `op` between consecutive elements, going left to right
   *  @throws UnsupportedOperationException if this buffer is empty
   */
  override def reduceLeft[B >: A](op: (B, A) => B): B =
    if (length > 0) foldl(1, length, array(0).asInstanceOf[B], op)
    else super.reduceLeft(op)

  /** Applies the binary operator `op` to all elements of this buffer, going
   *  right to left.
   *
   *  Overridden to read the backing array directly.
   *
   *  @tparam B the result type of the operator, a supertype of the element type
   *  @param op the binary operator
   *  @return the result of inserting `op` between consecutive elements, going right to left
   *  @throws UnsupportedOperationException if this buffer is empty
   */
  override def reduceRight[B >: A](op: (A, B) => B): B =
    if (length > 0) foldr(0, length - 1, array(length - 1).asInstanceOf[B], op)
    else super.reduceRight(op)

  /** Groups elements into fixed-size blocks by passing a "sliding window" over
   *  them, returning an iterator of array buffers.
   *
   *  The iterator is fail-fast: it throws a
   *  [[java.util.ConcurrentModificationException]] if this buffer is mutated
   *  during iteration.
   *
   *  @param size the number of elements per group
   *  @param step the distance between the first elements of successive groups
   *  @return an iterator over the groups, each an `ArrayBuffer` of `size`
   *          elements, except possibly a smaller final group
   */
  override def sliding(size: Int, step: Int): Iterator[ArrayBuffer[A]] =
    new MutationTracker.CheckedIterator(super.sliding(size = size, step = step), mutationCount)
}

/** Factory object for the `ArrayBuffer` class.
 *
 *  $factoryInfo
 *
 *  @define coll array buffer
 *  @define Coll `mutable.ArrayBuffer`
 */
@SerialVersionUID(3L)
object ArrayBuffer extends StrictOptimizedSeqFactory[ArrayBuffer] {
  /** The capacity, 16, of the backing array of an `ArrayBuffer` created without
   *  an initial size, and the default capacity `clearAndShrink` shrinks to. It
   *  is also the minimum capacity allocated when a buffer's backing array first
   *  grows.
   */
  final val DefaultInitialSize = 16
  private val emptyArray = new Array[AnyRef](0)

  /** Creates an array buffer containing the elements of `coll`.
   *
   *  If the size of `coll` is known, the backing array is allocated only once,
   *  large enough for all elements.
   *
   *  @tparam B the element type
   *  @param coll the collection providing the elements
   *  @return a new `ArrayBuffer` with the elements of `coll` in order
   */
  def from[B](coll: collection.IterableOnce[B]^): ArrayBuffer[B] = {
    val k = coll.knownSize
    if (k >= 0) {
      // Avoid reallocation of buffer if length is known
      val array = ensureSize(emptyArray, 0, k) // don't duplicate sizing logic, and check VM array size limit
      val actual = IterableOnce.copyElemsToArray(coll, array.asInstanceOf[Array[Any]])
      if (actual != k) throw new IllegalStateException(s"Copied $actual of $k")
      new ArrayBuffer[B](array, k)
    }
    else new ArrayBuffer[B] ++= coll
  }

  /** Returns a new builder that produces an `ArrayBuffer`.
   *
   *  The returned builder forwards `sizeHint` to the underlying buffer, so the
   *  backing array can be pre-allocated.
   *
   *  @tparam A the element type
   *  @return a builder producing an `ArrayBuffer`
   */
  def newBuilder[A]: Builder[A, ArrayBuffer[A]] = new GrowableBuilder[A, ArrayBuffer[A]](empty[A]) {
    override def sizeHint(size: Int): Unit = elems.sizeHint(size)
  }

  /** Creates a new, empty array buffer with the default initial capacity.
   *
   *  @tparam A the element type
   *  @return a new empty `ArrayBuffer`
   */
  def empty[A]: ArrayBuffer[A] = new ArrayBuffer[A]()

  /** The increased size for an array-backed collection.
   *
   *  @param arrayLen  the length of the backing array
   *  @param targetLen the minimum length to resize up to
   *  @return
   *   - `-1` if no resizing is needed, else
   *   - `VM_MaxArraySize` if `arrayLen` is too large to be doubled, else
   *   - `max(targetLen, arrayLen * 2, DefaultInitialSize)`.
   *   - Throws an exception if `targetLen` exceeds `VM_MaxArraySize` or is negative (overflow).
   */
  private[mutable] def resizeUp(arrayLen: Int, targetLen: Int): Int =
    if (targetLen < 0) throw new RuntimeException(s"Overflow while resizing array of array-backed collection. Requested length: $targetLen; current length: $arrayLen; increase: ${targetLen - arrayLen}")
    else if (targetLen <= arrayLen) -1
    else if (targetLen > VM_MaxArraySize) throw new RuntimeException(s"Array of array-backed collection exceeds VM length limit of $VM_MaxArraySize. Requested length: $targetLen; current length: $arrayLen")
    else if (arrayLen > VM_MaxArraySize / 2) VM_MaxArraySize
    else math.max(targetLen, math.max(arrayLen * 2, DefaultInitialSize))

  // if necessary, copy (curSize elements of) the array to a new array of capacity n.
  // Should use Array.copyOf(array, resizeEnsuring(array.length))?
  private def ensureSize(array: Array[AnyRef], curSize: Int, targetSize: Int): Array[AnyRef] = {
    val newLen = resizeUp(array.length, targetSize)
    if (newLen < 0) array
    else {
      val res = new Array[AnyRef](newLen)
      System.arraycopy(array, 0, res, 0, curSize)
      res
    }
  }

  /**
   *  @param arrayLen  the length of the backing array
   *  @param targetLen the length to resize down to, if smaller than `arrayLen`
   *  @return -1 if no resizing is needed, or the size for the new array otherwise
   */
  private def resizeDown(arrayLen: Int, targetLen: Int): Int =
    if (targetLen >= arrayLen) -1 else math.max(targetLen, 0)
  private def downsize(array: Array[AnyRef], targetSize: Int): Array[AnyRef] = {
    val newLen = resizeDown(array.length, targetSize)
    if (newLen < 0) array
    else if (newLen == 0) emptyArray
    else {
      val res = new Array[AnyRef](newLen)
      System.arraycopy(array, 0, res, 0, targetSize)
      res
    }
  }
}

// TODO: use `CheckedIndexedSeqView.Id` once we can change the return type of `ArrayBuffer#view`
/** An `IndexedSeqView` over an [[ArrayBuffer]], obtained from `ArrayBuffer#view`.
 *
 *  Iterators obtained from this view, directly or through the overridden
 *  transformation methods, check the buffer's mutation count and throw a
 *  [[java.util.ConcurrentModificationException]] if the buffer is mutated
 *  after the iterator's creation.
 *
 *  @tparam A the element type of the underlying buffer
 */
final class ArrayBufferView[A] private[mutable](underlying: ArrayBuffer[A], mutationCount: () => Int)
  extends AbstractIndexedSeqView[A] {
  /** Creates a view over the first `length` elements of `array`.
   *
   *  The array is wrapped in a fresh `ArrayBuffer`, and the view performs no
   *  mutation tracking: its mutation count is a constant 0, so its iterators
   *  never detect a concurrent mutation.
   *
   *  @param array the array holding the elements, erased to `AnyRef`
   *  @param length the number of elements to expose
   */
  @deprecated("never intended to be public; call ArrayBuffer#view instead", since = "2.13.7")
  def this(array: Array[AnyRef], length: Int) = {
    // this won't actually track mutation, but it would be a pain to have the implementation
    // check if we have a method to get the current mutation count or not on every method and
    // change what it does based on that. hopefully no one ever calls this.
    this({
      val _array = array
      val _length = length
      val buf = new ArrayBuffer[A](0) {
        this.array = _array
        this.size0 = _length
      }
      buf
    }, () => 0)
  }

  /** Returns a fresh array containing the elements of the underlying buffer,
   *  not the buffer's internal storage.
   */
  @deprecated("never intended to be public", since = "2.13.7")
  def array: Array[AnyRef] = underlying.toArray[Any].asInstanceOf[Array[AnyRef]]

  /** Returns the element of the underlying buffer at index `n`.
   *
   *  @param n the index of the element to return
   *  @throws IndexOutOfBoundsException if `n < 0` or `n >= length`
   */
  @throws[IndexOutOfBoundsException]
  def apply(n: Int): A = underlying(n)
  /** Returns the current number of elements in the underlying buffer. */
  def length: Int = underlying.length
  /** The prefix of this view's `toString` representation, `"ArrayBufferView"`. */
  override protected def className = "ArrayBufferView"

  // we could inherit all these from `CheckedIndexedSeqView`, except this class is public
  /** Returns a fail-fast iterator over the elements of this view. */
  override def iterator: Iterator[A]^{this} = new CheckedIndexedSeqView.CheckedIterator(this, mutationCount())
  /** Returns a fail-fast iterator over the elements of this view, in reverse order. */
  override def reverseIterator: Iterator[A]^{this} = new CheckedIndexedSeqView.CheckedReverseIterator(this, mutationCount())

  /** Returns a mutation-checked view of this view with `elem` appended.
   *
   *  @tparam B the element type of the returned view
   *  @param elem the element to append
   *  @return a view of the elements of this view followed by `elem`
   */
  override def appended[B >: A](elem: B): IndexedSeqView[B]^{this} = new CheckedIndexedSeqView.Appended(this, elem)(mutationCount)
  /** Returns a mutation-checked view of this view with `elem` prepended.
   *
   *  @tparam B the element type of the returned view
   *  @param elem the element to prepend
   *  @return a view of `elem` followed by the elements of this view
   */
  override def prepended[B >: A](elem: B): IndexedSeqView[B]^{this} = new CheckedIndexedSeqView.Prepended(elem, this)(mutationCount)
  /** Returns a mutation-checked view of the first `n` elements of this view.
   *
   *  @param n the number of elements to take
   *  @return a view of the first `n` elements, of all elements if `n` exceeds
   *          the length, or of no elements if `n` is negative
   */
  override def take(n: Int): IndexedSeqView[A]^{this} = new CheckedIndexedSeqView.Take(this, n)(mutationCount)
  /** Returns a mutation-checked view of the last `n` elements of this view.
   *
   *  @param n the number of elements to take
   *  @return a view of the last `n` elements, of all elements if `n` exceeds
   *          the length, or of no elements if `n` is negative
   */
  override def takeRight(n: Int): IndexedSeqView[A]^{this} = new CheckedIndexedSeqView.TakeRight(this, n)(mutationCount)
  /** Returns a mutation-checked view of all but the first `n` elements of this view.
   *
   *  @param n the number of elements to drop
   *  @return a view of all elements except the first `n`
   */
  override def drop(n: Int): IndexedSeqView[A]^{this} = new CheckedIndexedSeqView.Drop(this, n)(mutationCount)
  /** Returns a mutation-checked view of all but the last `n` elements of this view.
   *
   *  @param n the number of elements to drop
   *  @return a view of all elements except the last `n`
   */
  override def dropRight(n: Int): IndexedSeqView[A]^{this} = new CheckedIndexedSeqView.DropRight(this, n)(mutationCount)
  /** Returns a mutation-checked view with `f` lazily applied to each element of this view.
   *
   *  @tparam B the element type of the returned view
   *  @param f the function to apply to each element
   *  @return a view of the results of applying `f` to the elements of this view
   */
  override def map[B](f: A => B): IndexedSeqView[B]^{this, f} = new CheckedIndexedSeqView.Map(this, f)(mutationCount)
  /** Returns a mutation-checked view of the elements of this view in reverse order. */
  override def reverse: IndexedSeqView[A]^{this} = new CheckedIndexedSeqView.Reverse(this)(mutationCount)
  /** Returns a mutation-checked view of the elements at indices `from` until `until` of this view.
   *
   *  @param from the index of the first element in the slice
   *  @param until the index one past the last element in the slice
   *  @return a view of the elements in the given index range
   */
  override def slice(from: Int, until: Int): IndexedSeqView[A]^{this} = new CheckedIndexedSeqView.Slice(this, from, until)(mutationCount)
  /** Returns a mutation-checked view that applies `f` to each element as it is
   *  traversed, for its side effect, and produces the element unchanged.
   *
   *  @tparam U the result type of `f`, which is discarded
   *  @param f the function to apply to each traversed element
   *  @return a view of the same elements as this view
   */
  override def tapEach[U](f: A => U): IndexedSeqView[A]^{this, f} = new CheckedIndexedSeqView.Map(this, { (a: A) => f(a); a})(mutationCount)

  /** Returns a mutation-checked view of the elements of this view followed by
   *  the elements of `suffix`.
   *
   *  @tparam B the element type of the returned view
   *  @param suffix the indexed sequence or view whose elements follow this view's
   *  @return a view of the concatenation
   */
  override def concat[B >: A](suffix: IndexedSeqView.SomeIndexedSeqOps[B]^): IndexedSeqView[B]^{suffix, this} = new CheckedIndexedSeqView.Concat(this, suffix)(mutationCount)
  /** Returns a mutation-checked view of the elements of this view followed by
   *  the elements of `suffix`, like `concat`.
   *
   *  @tparam B the element type of the returned view
   *  @param suffix the indexed sequence or view whose elements follow this view's
   *  @return a view of the concatenation
   */
  override def appendedAll[B >: A](suffix: IndexedSeqView.SomeIndexedSeqOps[B]^): IndexedSeqView[B]^{suffix, this} = new CheckedIndexedSeqView.Concat(this, suffix)(mutationCount)
  /** Returns a mutation-checked view of the elements of `prefix` followed by
   *  the elements of this view.
   *
   *  @tparam B the element type of the returned view
   *  @param prefix the indexed sequence or view whose elements precede this view's
   *  @return a view of the concatenation
   */
  override def prependedAll[B >: A](prefix: IndexedSeqView.SomeIndexedSeqOps[B]^): IndexedSeqView[B]^{prefix, this} = new CheckedIndexedSeqView.Concat(prefix, this)(mutationCount)
}
