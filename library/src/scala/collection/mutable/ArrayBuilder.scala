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
package mutable

import scala.language.`2.13`
import language.experimental.captureChecking
import scala.collection.mutable.ArrayBuffer.resizeUp
import scala.reflect.ClassTag

/** A builder class for arrays.
 *
 *  @tparam T    the type of the elements for the builder.
 */
@SerialVersionUID(3L)
sealed abstract class ArrayBuilder[T]
  extends ReusableBuilder[T, Array[T]]
    with Serializable {
  /** The number of elements this builder can hold without resizing, typically
   *  the length of `elems`; 0 while no backing array is allocated.
   */
  protected var capacity: Int = 0
  /** The backing array, holding the first `size` elements added to this
   *  builder, or `null` while no array is allocated.
   */
  protected def elems: Array[T] | Null // may not be allocated at size = capacity = 0
  /** The number of elements added to this builder so far. */
  protected var size: Int = 0

  /** Current number of elements. */
  def length: Int = size

  /** Current number of elements. */
  override def knownSize: Int = size

  /** Grows the backing array, if necessary, so that it can hold at least
   *  `size` elements.
   *
   *  When growing is needed, the new capacity is the largest of the requested
   *  size, twice the current capacity, and the default initial size (16),
   *  capped at the maximum array size the VM supports.
   *
   *  @param size the required minimum capacity
   *  @throws RuntimeException if `size` is negative (an overflow) or exceeds
   *          the maximum array size the VM supports
   */
  protected final def ensureSize(size: Int): Unit = {
    val newLen = resizeUp(capacity, size)
    if (newLen > 0) resize(newLen)
  }

  /** Grows the backing array to hold at least `size` elements.
   *
   *  Does nothing if the current capacity suffices; otherwise resizes to
   *  exactly `size`, avoiding the over-allocation of the doubling strategy
   *  when the final number of elements is known in advance.
   *
   *  @param size the expected number of elements
   */
  override final def sizeHint(size: Int): Unit = if (capacity < size) resize(size)

  /** Discards all elements added so far, leaving this builder empty.
   *
   *  Any backing array is kept at its current capacity and reused.
   */
  def clear(): Unit = size = 0

  /** Resizes this builder to the new capacity `size`; builders backed by an
   *  array reallocate it to that length, preserving the elements added so far.
   *
   *  @param size the new capacity
   */
  protected def resize(size: Int): Unit

  /** Adds all elements of an array.
   *
   *  @param xs the array of elements to add
   *  @return this builder with the elements of `xs` appended
   */
  def addAll(xs: Array[? <: T]): this.type = addAll(xs, 0, xs.length)

  /** Adds a slice of an array.
   *
   *  @param xs the array from which a slice of elements is added
   *  @param offset the start index within `xs` from which to copy elements (clamped to 0 if negative)
   *  @param length the maximum number of elements to copy from `xs` (clamped to 0 if negative, and to the number of available elements)
   *  @return this builder with the selected slice of `xs` appended
   */
  def addAll(xs: Array[? <: T], offset: Int, length: Int): this.type = {
    val offset1 = offset.max(0)
    val length1 = length.max(0)
    val effectiveLength = length1.min(xs.length - offset1)
    doAddAll(xs, offset1, effectiveLength)
  }

  private def doAddAll(xs: Array[? <: T], offset: Int, length: Int): this.type = {
    if (length > 0) {
      ensureSize(this.size + length)
      Array.copy(xs, offset, elems.nn, this.size, length)
      size += length
    }
    this
  }

  /** Adds all elements of an iterable collection.
   *
   *  If `xs` has a known size, the backing array is grown once up front and
   *  the elements are copied in bulk; otherwise the elements are added one by
   *  one.
   *
   *  @param xs the collection whose elements are added
   *  @return this builder with the elements of `xs` appended
   *  @throws IllegalStateException if `xs` reports a known size but yields a
   *          different number of elements
   */
  override def addAll(xs: IterableOnce[T]^): this.type = {
    val k = xs.knownSize
    if (k > 0) {
      ensureSize(this.size + k)
      val actual = IterableOnce.copyElemsToArray(xs, elems.nn, this.size)
      if (actual != k) throw new IllegalStateException(s"Copied $actual of $k")
      size += k
    } else if (k < 0) super.addAll(xs)
    this
  }
}

/** A companion object for array builders. */
object ArrayBuilder {

  /** Creates a new arraybuilder of type `T`.
   *
   *  @tparam T     type of the elements for the array builder, with a `ClassTag` context bound.
   *  @return       a new empty array builder.
   */
  @inline def make[T: ClassTag]: ArrayBuilder[T] = {
    val tag = implicitly[ClassTag[T]]
    tag.runtimeClass match {
      case java.lang.Byte.TYPE      => new ArrayBuilder.ofByte().asInstanceOf[ArrayBuilder[T]]
      case java.lang.Short.TYPE     => new ArrayBuilder.ofShort().asInstanceOf[ArrayBuilder[T]]
      case java.lang.Character.TYPE => new ArrayBuilder.ofChar().asInstanceOf[ArrayBuilder[T]]
      case java.lang.Integer.TYPE   => new ArrayBuilder.ofInt().asInstanceOf[ArrayBuilder[T]]
      case java.lang.Long.TYPE      => new ArrayBuilder.ofLong().asInstanceOf[ArrayBuilder[T]]
      case java.lang.Float.TYPE     => new ArrayBuilder.ofFloat().asInstanceOf[ArrayBuilder[T]]
      case java.lang.Double.TYPE    => new ArrayBuilder.ofDouble().asInstanceOf[ArrayBuilder[T]]
      case java.lang.Boolean.TYPE   => new ArrayBuilder.ofBoolean().asInstanceOf[ArrayBuilder[T]]
      case java.lang.Void.TYPE      => new ArrayBuilder.ofUnit().asInstanceOf[ArrayBuilder[T]]
      case _                        => new ArrayBuilder.ofRef[T & AnyRef]()(using tag.asInstanceOf[ClassTag[T & AnyRef]]).asInstanceOf[ArrayBuilder[T]]
    }
  }

  /** A class for array builders for arrays of reference types.
   *
   *  This builder can be reused.
   *
   *  @tparam T     type of elements for the array builder, subtype of `AnyRef` with a `ClassTag` context bound.
   */
  @SerialVersionUID(3L)
  final class ofRef[T <: AnyRef | Null](implicit ct: ClassTag[T]) extends ArrayBuilder[T] {

    /** The backing array; `null` until storage is first allocated, and reset
     *  to `null` when `result()` hands the array off without copying.
     */
    protected var elems: Array[T] | Null = null

    private def mkArray(size: Int): Array[T] = {
      if (capacity == size && capacity > 0) elems.nn
      else if (elems eq null) new Array[T](size)
      else java.util.Arrays.copyOf[T](elems, size)
    }

    /** Reallocates the backing array to length `size`, copying the elements
     *  added so far, and updates `capacity`.
     *
     *  @param size the new capacity
     */
    protected def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    /** Adds a single element to this builder.
     *
     *  @param elem the element to add
     *  @return this builder with `elem` appended
     */
    def addOne(elem: T): this.type = {
      ensureSize(size + 1)
      elems.nn(size) = elem
      size += 1
      this
    }

    /** Returns an array containing all elements added to this builder.
     *
     *  If the elements added exactly fill the backing array, that array is
     *  returned directly, without copying, and this builder gives it up;
     *  otherwise the elements are copied into a new array of exactly the
     *  right length. After this call, `clear()` must be called before this
     *  builder is used again.
     */
    def result(): Array[T] = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        val res = elems.nn
        elems = null
        res
      }
      else mkArray(size)
    }

    /** Discards all elements added so far, leaving this builder empty.
     *
     *  The backing array is kept at its current capacity, but its cells are
     *  nulled out so that the discarded elements can be garbage collected.
     */
    override def clear(): Unit = {
      super.clear()
      if(elems ne null) java.util.Arrays.fill(elems.asInstanceOf[Array[AnyRef]], null)
    }

    /** Returns the string `"ArrayBuilder.ofRef"`. */
    override def toString() = "ArrayBuilder.ofRef"
  }

  /** A class for array builders for arrays of `byte`s. It can be reused. */
  @SerialVersionUID(3L)
  final class ofByte extends ArrayBuilder[Byte] {

    /** The backing array; `null` until storage is first allocated, and reset
     *  to `null` when `result()` hands the array off without copying.
     */
    protected var elems: Array[Byte] | Null = null

    private def mkArray(size: Int): Array[Byte] = {
      val newelems = new Array[Byte](size)
      if (this.size > 0) Array.copy(elems.nn, 0, newelems, 0, this.size)
      newelems
    }

    /** Reallocates the backing array to length `size`, copying the elements
     *  added so far, and updates `capacity`.
     *
     *  @param size the new capacity
     */
    protected def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    /** Adds a single element to this builder.
     *
     *  @param elem the element to add
     *  @return this builder with `elem` appended
     */
    def addOne(elem: Byte): this.type = {
      ensureSize(size + 1)
      elems.nn(size) = elem
      size += 1
      this
    }

    /** Returns an array containing all elements added to this builder.
     *
     *  If the elements added exactly fill the backing array, that array is
     *  returned directly, without copying, and this builder gives it up;
     *  otherwise the elements are copied into a new array of exactly the
     *  right length. After this call, `clear()` must be called before this
     *  builder is used again.
     */
    def result(): Array[Byte] = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        val res = elems.nn
        elems = null
        res
      }
      else mkArray(size)
    }

    /** Returns the string `"ArrayBuilder.ofByte"`. */
    override def toString() = "ArrayBuilder.ofByte"
  }

  /** A class for array builders for arrays of `short`s. It can be reused. */
  @SerialVersionUID(3L)
  final class ofShort extends ArrayBuilder[Short] {

    /** The backing array; `null` until storage is first allocated, and reset
     *  to `null` when `result()` hands the array off without copying.
     */
    protected var elems: Array[Short] | Null = null

    private def mkArray(size: Int): Array[Short] = {
      val newelems = new Array[Short](size)
      if (this.size > 0) Array.copy(elems.nn, 0, newelems, 0, this.size)
      newelems
    }

    /** Reallocates the backing array to length `size`, copying the elements
     *  added so far, and updates `capacity`.
     *
     *  @param size the new capacity
     */
    protected def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    /** Adds a single element to this builder.
     *
     *  @param elem the element to add
     *  @return this builder with `elem` appended
     */
    def addOne(elem: Short): this.type = {
      ensureSize(size + 1)
      elems.nn(size) = elem
      size += 1
      this
    }

    /** Returns an array containing all elements added to this builder.
     *
     *  If the elements added exactly fill the backing array, that array is
     *  returned directly, without copying, and this builder gives it up;
     *  otherwise the elements are copied into a new array of exactly the
     *  right length. After this call, `clear()` must be called before this
     *  builder is used again.
     */
    def result(): Array[Short] = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        val res = elems.nn
        elems = null
        res
      }
      else mkArray(size)
    }

    /** Returns the string `"ArrayBuilder.ofShort"`. */
    override def toString() = "ArrayBuilder.ofShort"
  }

  /** A class for array builders for arrays of `char`s. It can be reused. */
  @SerialVersionUID(3L)
  final class ofChar extends ArrayBuilder[Char] {

    /** The backing array; `null` until storage is first allocated, and reset
     *  to `null` when `result()` hands the array off without copying.
     */
    protected var elems: Array[Char] | Null = null

    private def mkArray(size: Int): Array[Char] = {
      val newelems = new Array[Char](size)
      if (this.size > 0) Array.copy(elems.nn, 0, newelems, 0, this.size)
      newelems
    }

    /** Reallocates the backing array to length `size`, copying the elements
     *  added so far, and updates `capacity`.
     *
     *  @param size the new capacity
     */
    protected def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    /** Adds a single element to this builder.
     *
     *  @param elem the element to add
     *  @return this builder with `elem` appended
     */
    def addOne(elem: Char): this.type = {
      ensureSize(size + 1)
      elems.nn(size) = elem
      size += 1
      this
    }

    /** Returns an array containing all elements added to this builder.
     *
     *  If the elements added exactly fill the backing array, that array is
     *  returned directly, without copying, and this builder gives it up;
     *  otherwise the elements are copied into a new array of exactly the
     *  right length. After this call, `clear()` must be called before this
     *  builder is used again.
     */
    def result(): Array[Char] = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        val res = elems.nn
        elems = null
        res
      }
      else mkArray(size)
    }

    /** Returns the string `"ArrayBuilder.ofChar"`. */
    override def toString() = "ArrayBuilder.ofChar"
  }

  /** A class for array builders for arrays of `int`s. It can be reused. */
  @SerialVersionUID(3L)
  final class ofInt extends ArrayBuilder[Int] {

    /** The backing array; `null` until storage is first allocated, and reset
     *  to `null` when `result()` hands the array off without copying.
     */
    protected var elems: Array[Int] | Null = null

    private def mkArray(size: Int): Array[Int] = {
      val newelems = new Array[Int](size)
      if (this.size > 0) Array.copy(elems.nn, 0, newelems, 0, this.size)
      newelems
    }

    /** Reallocates the backing array to length `size`, copying the elements
     *  added so far, and updates `capacity`.
     *
     *  @param size the new capacity
     */
    protected def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    /** Adds a single element to this builder.
     *
     *  @param elem the element to add
     *  @return this builder with `elem` appended
     */
    def addOne(elem: Int): this.type = {
      ensureSize(size + 1)
      elems.nn(size) = elem
      size += 1
      this
    }

    /** Returns an array containing all elements added to this builder.
     *
     *  If the elements added exactly fill the backing array, that array is
     *  returned directly, without copying, and this builder gives it up;
     *  otherwise the elements are copied into a new array of exactly the
     *  right length. After this call, `clear()` must be called before this
     *  builder is used again.
     */
    def result(): Array[Int] = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        val res = elems.nn
        elems = null
        res
      }
      else mkArray(size)
    }

    /** Returns the string `"ArrayBuilder.ofInt"`. */
    override def toString() = "ArrayBuilder.ofInt"
  }

  /** A class for array builders for arrays of `long`s. It can be reused. */
  @SerialVersionUID(3L)
  final class ofLong extends ArrayBuilder[Long] {

    /** The backing array; `null` until storage is first allocated, and reset
     *  to `null` when `result()` hands the array off without copying.
     */
    protected var elems: Array[Long] | Null = null

    private def mkArray(size: Int): Array[Long] = {
      val newelems = new Array[Long](size)
      if (this.size > 0) Array.copy(elems.nn, 0, newelems, 0, this.size)
      newelems
    }

    /** Reallocates the backing array to length `size`, copying the elements
     *  added so far, and updates `capacity`.
     *
     *  @param size the new capacity
     */
    protected def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    /** Adds a single element to this builder.
     *
     *  @param elem the element to add
     *  @return this builder with `elem` appended
     */
    def addOne(elem: Long): this.type = {
      ensureSize(size + 1)
      elems.nn(size) = elem
      size += 1
      this
    }

    /** Returns an array containing all elements added to this builder.
     *
     *  If the elements added exactly fill the backing array, that array is
     *  returned directly, without copying, and this builder gives it up;
     *  otherwise the elements are copied into a new array of exactly the
     *  right length. After this call, `clear()` must be called before this
     *  builder is used again.
     */
    def result(): Array[Long] = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        val res = elems.nn
        elems = null
        res
      }
      else mkArray(size)
    }

    /** Returns the string `"ArrayBuilder.ofLong"`. */
    override def toString() = "ArrayBuilder.ofLong"
  }

  /** A class for array builders for arrays of `float`s. It can be reused. */
  @SerialVersionUID(3L)
  final class ofFloat extends ArrayBuilder[Float] {

    /** The backing array; `null` until storage is first allocated, and reset
     *  to `null` when `result()` hands the array off without copying.
     */
    protected var elems: Array[Float] | Null = null

    private def mkArray(size: Int): Array[Float] = {
      val newelems = new Array[Float](size)
      if (this.size > 0) Array.copy(elems.nn, 0, newelems, 0, this.size)
      newelems
    }

    /** Reallocates the backing array to length `size`, copying the elements
     *  added so far, and updates `capacity`.
     *
     *  @param size the new capacity
     */
    protected def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    /** Adds a single element to this builder.
     *
     *  @param elem the element to add
     *  @return this builder with `elem` appended
     */
    def addOne(elem: Float): this.type = {
      ensureSize(size + 1)
      elems.nn(size) = elem
      size += 1
      this
    }

    /** Returns an array containing all elements added to this builder.
     *
     *  If the elements added exactly fill the backing array, that array is
     *  returned directly, without copying, and this builder gives it up;
     *  otherwise the elements are copied into a new array of exactly the
     *  right length. After this call, `clear()` must be called before this
     *  builder is used again.
     */
    def result(): Array[Float] = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        val res = elems.nn
        elems = null
        res
      }
      else mkArray(size)
    }

    /** Returns the string `"ArrayBuilder.ofFloat"`. */
    override def toString() = "ArrayBuilder.ofFloat"
  }

  /** A class for array builders for arrays of `double`s. It can be reused. */
  @SerialVersionUID(3L)
  final class ofDouble extends ArrayBuilder[Double] {

    /** The backing array; `null` until storage is first allocated, and reset
     *  to `null` when `result()` hands the array off without copying.
     */
    protected var elems: Array[Double] | Null = null

    private def mkArray(size: Int): Array[Double] = {
      val newelems = new Array[Double](size)
      if (this.size > 0) Array.copy(elems.nn, 0, newelems, 0, this.size)
      newelems
    }

    /** Reallocates the backing array to length `size`, copying the elements
     *  added so far, and updates `capacity`.
     *
     *  @param size the new capacity
     */
    protected def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    /** Adds a single element to this builder.
     *
     *  @param elem the element to add
     *  @return this builder with `elem` appended
     */
    def addOne(elem: Double): this.type = {
      ensureSize(size + 1)
      elems.nn(size) = elem
      size += 1
      this
    }

    /** Returns an array containing all elements added to this builder.
     *
     *  If the elements added exactly fill the backing array, that array is
     *  returned directly, without copying, and this builder gives it up;
     *  otherwise the elements are copied into a new array of exactly the
     *  right length. After this call, `clear()` must be called before this
     *  builder is used again.
     */
    def result(): Array[Double] = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        val res = elems.nn
        elems = null
        res
      }
      else mkArray(size)
    }

    /** Returns the string `"ArrayBuilder.ofDouble"`. */
    override def toString() = "ArrayBuilder.ofDouble"
  }

  /** A class for array builders for arrays of `boolean`s. It can be reused. */
  @SerialVersionUID(3L)
  class ofBoolean extends ArrayBuilder[Boolean] {
    this: ofBoolean^{} =>

    /** The backing array; `null` until storage is first allocated, and reset
     *  to `null` when `result()` hands the array off without copying.
     */
    protected var elems: Array[Boolean] | Null = null

    private def mkArray(size: Int): Array[Boolean] = {
      val newelems = new Array[Boolean](size)
      if (this.size > 0) Array.copy(elems.nn, 0, newelems, 0, this.size)
      newelems
    }

    /** Reallocates the backing array to length `size`, copying the elements
     *  added so far, and updates `capacity`.
     *
     *  @param size the new capacity
     */
    protected def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    /** Adds a single element to this builder.
     *
     *  @param elem the element to add
     *  @return this builder with `elem` appended
     */
    def addOne(elem: Boolean): this.type = {
      ensureSize(size + 1)
      elems.nn(size) = elem
      size += 1
      this
    }

    /** Returns an array containing all elements added to this builder.
     *
     *  If the elements added exactly fill the backing array, that array is
     *  returned directly, without copying, and this builder gives it up;
     *  otherwise the elements are copied into a new array of exactly the
     *  right length. After this call, `clear()` must be called before this
     *  builder is used again.
     */
    def result(): Array[Boolean] = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        val res = elems.nn
        elems = null
        res
      }
      else mkArray(size)
    }

    /** Returns the string `"ArrayBuilder.ofBoolean"`. */
    override def toString() = "ArrayBuilder.ofBoolean"
  }

  /** A class for array builders for arrays of `Unit` type. It can be reused. */
  @SerialVersionUID(3L)
  final class ofUnit extends ArrayBuilder[Unit] {

    /** Not supported: this builder stores no elements, only their count.
     *
     *  @throws UnsupportedOperationException always
     */
    protected def elems: Array[Unit] | Null = throw new UnsupportedOperationException()

    /** Adds a single unit value by incrementing the element count.
     *
     *  @param elem never used, as all unit values are identical
     *  @return this builder with its size increased by 1
     */
    def addOne(elem: Unit): this.type = {
      val newSize = size + 1
      ensureSize(newSize)
      size = newSize
      this
    }

    /** Adds all elements of a collection by increasing the element count by
     *  its size.
     *
     *  @param xs the collection whose elements are counted; it is iterated
     *            fully to compute its size, but its elements are not stored
     *  @return this builder with its size increased by the size of `xs`
     */
    override def addAll(xs: IterableOnce[Unit]^): this.type = {
      val newSize = size + xs.iterator.size
      ensureSize(newSize)
      size = newSize
      this
    }

    /** Adds `length` unit values by increasing the element count, without
     *  inspecting the array.
     *
     *  @param xs the array whose elements are counted; never used
     *  @param offset the start index of the slice; never used
     *  @param length the number of elements to add, applied as is
     *  @return this builder with its size increased by `length`
     *  @note NEEDS-HUMAN: unlike the base overload, this override does not
     *        clamp `offset` and `length`: a negative `length` shrinks the
     *        builder (or makes `ensureSize` throw), and a `length` larger
     *        than the slice inflates the count.
     */
    override def addAll(xs: Array[? <: Unit], offset: Int, length: Int): this.type = {
      val newSize = size + length
      ensureSize(newSize)
      size = newSize
      this
    }

    /** Returns a new array of `size` unit values.
     *
     *  A fresh array is allocated and filled on each call; this builder's
     *  count is left unchanged.
     */
    def result() = {
      val ans = new Array[Unit](size)
      var i = 0
      while (i < size) { ans(i) = (); i += 1 }
      ans
    }

    /** Records the new capacity; no storage is allocated, as this builder
     *  stores no elements.
     *
     *  @param size the new capacity
     */
    protected def resize(size: Int): Unit = capacity = size

    /** Returns the string `"ArrayBuilder.ofUnit"`. */
    override def toString() = "ArrayBuilder.ofUnit"
  }
}
