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
import java.util.Arrays
import scala.collection.Stepper.EfficientSplit
import scala.collection.convert.impl._
import scala.reflect.ClassTag
import scala.util.hashing.MurmurHash3

/** A collection representing `Array[T]`. Unlike `ArrayBuffer` it is always backed by the same
 *  underlying `Array`, therefore it is not growable or shrinkable.
 *
 *  @tparam T    type of the elements in this wrapped array.
 *
 *  @define Coll `ArraySeq`
 *  @define coll wrapped array
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
@SerialVersionUID(3L)
sealed abstract class ArraySeq[T]
  extends AbstractSeq[T]
    with IndexedSeq[T]
    with IndexedSeqOps[T, ArraySeq, ArraySeq[T]]
    with StrictOptimizedSeqOps[T, ArraySeq, ArraySeq[T]]
    with Serializable {

  /** Returns the companion factory in its untagged form, which requires no `ClassTag`
   *  and therefore builds collections backed by an `Array[AnyRef]`, storing primitive
   *  values boxed.
   */
  override def iterableFactory: scala.collection.SeqFactory[ArraySeq] = ArraySeq.untagged

  /** Builds a new `ArraySeq` containing the elements of `coll`, using this wrapped array's
   *  `elemTag` so that the result has the same underlying array representation.
   *
   *  @param coll the collection whose elements are copied into the result
   *  @return a new `ArraySeq` with the elements of `coll`
   */
  override protected def fromSpecific(coll: scala.collection.IterableOnce[T]^): ArraySeq[T] = {
    val b = ArrayBuilder.make(using elemTag).asInstanceOf[ArrayBuilder[T]]
    b.sizeHint(coll, delta = 0)
    b ++= coll
    ArraySeq.make(b.result())
  }
  /** Returns a builder for an `ArraySeq`, using this wrapped array's `elemTag` so that the
   *  result has the same underlying array representation.
   */
  override protected def newSpecificBuilder: Builder[T, ArraySeq[T]] = ArraySeq.newBuilder(using elemTag).asInstanceOf[Builder[T, ArraySeq[T]]]
  /** Returns the empty `ArraySeq`, the single instance shared for all element types. */
  override def empty: ArraySeq[T] = ArraySeq.empty(using elemTag.asInstanceOf[ClassTag[T]])

  /** The tag of the element type. This does not have to be equal to the element type of this ArraySeq. A primitive
   *  ArraySeq can be backed by an array of boxed values and a reference ArraySeq can be backed by an array of a supertype
   *  or subtype of the element type. 
   */
  def elemTag: ClassTag[?]

  /** Updates element at given index.
   *
   *  @param index the zero-based position of the element to update
   *  @param elem the new value to store at the given index
   */
  def update(@deprecatedName("idx", "2.13.0") index: Int, elem: T): Unit

  /** The underlying array. Its element type does not have to be equal to the element type of this ArraySeq. A primitive
   *  ArraySeq can be backed by an array of boxed values and a reference ArraySeq can be backed by an array of a supertype
   *  or subtype of the element type. 
   */
  def array: Array[?]

  /** Returns a stepper for the elements of this wrapped array.
   *
   *  Steppers enable creating a Java stream to operate on the elements. For a primitive
   *  element type, the stepper steps over unboxed values unless a reference-shaped stepper
   *  is explicitly requested.
   *
   *  @tparam S the type of the stepper, determined by `shape`
   *  @param shape implicit evidence selecting the stepper type for the element type `T`
   *  @return a stepper over the elements, supporting efficient splitting for parallel
   *          processing
   */
  override def stepper[S <: Stepper[?]](implicit shape: StepperShape[T, S]): S & EfficientSplit

  /** Returns `"ArraySeq"`, the name of this collection type used in `toString` output. */
  override protected def className = "ArraySeq"

  /** Clones this object, including the underlying Array. */
  override def clone(): ArraySeq[T] = ArraySeq.make(array.clone()).asInstanceOf[ArraySeq[T]]

  /** Copies elements of this wrapped array to another array, beginning at index `start`
   *  of `xs`.
   *
   *  The number of elements copied is the minimum of `len`, the length of this wrapped
   *  array, and the remaining capacity of `xs` from `start`; if that minimum is not
   *  positive, nothing is copied. Copying is performed by a single `Array.copy` of the
   *  underlying array.
   *
   *  @tparam B the element type of the destination array, a supertype of `T`
   *  @param xs the destination array
   *  @param start the index of `xs` at which to write the first element
   *  @param len the maximum number of elements to copy
   *  @return the number of elements actually copied
   */
  override def copyToArray[B >: T](xs: Array[B], start: Int, len: Int): Int = {
    val copied = IterableOnce.elemsToCopyToArray(length, xs.length, start, len)
    if(copied > 0) {
      Array.copy(array, 0, xs, start, copied)
    }
    copied
  }

  /** Compares this wrapped array with another object for equality.
   *
   *  Two `ArraySeq`s whose underlying arrays differ in length are never equal. Otherwise,
   *  general sequence equality applies, so a wrapped array can be equal to any other
   *  sequence containing the same elements in the same order.
   *
   *  @param other the object to compare with
   *  @return `true` if `other` is a sequence with equal elements in the same order
   */
  override def equals(other: Any): Boolean = other match {
    case that: ArraySeq[?] if this.array.length != that.array.length =>
      false
    case _ =>
      super.equals(other)
  }

  /** Returns a new `ArraySeq` with the elements of this wrapped array sorted according to
   *  the given ordering.
   *
   *  This wrapped array is not modified; the result wraps a sorted copy of the underlying
   *  array.
   *
   *  @tparam B the type on which `ord` is defined, a supertype of `T`
   *  @param ord the ordering used to compare elements
   */
  override def sorted[B >: T](implicit ord: Ordering[B]): ArraySeq[T] =
    ArraySeq.make(array.sorted(using ord.asInstanceOf[Ordering[Any]])).asInstanceOf[ArraySeq[T]]

  /** Sorts this wrapped array in place according to the given ordering, using a stable
   *  sort.
   *
   *  A wrapped array with fewer than two elements is left untouched.
   *
   *  @tparam B the type on which `ord` is defined, a supertype of `T`
   *  @param ord the ordering used to compare elements
   *  @return this wrapped array, sorted
   */
  override def sortInPlace[B >: T]()(implicit ord: Ordering[B]): this.type = {
    if (length > 1) scala.util.Sorting.stableSort(array.asInstanceOf[Array[B]])
    this
  }
}

/** A companion object used to create instances of `ArraySeq`. */
@SerialVersionUID(3L)
object ArraySeq extends StrictOptimizedClassTagSeqFactory[ArraySeq] { self =>
  /** A factory for `ArraySeq`s that requires no `ClassTag`.
   *
   *  Collections built through this factory are backed by an `Array[AnyRef]` and store
   *  primitive values boxed.
   */
  val untagged: SeqFactory[ArraySeq] = new ClassTagSeqFactory.AnySeqDelegate(self)

  // This is reused for all calls to empty.
  private val EmptyArraySeq  = new ofRef[AnyRef](new Array[AnyRef](0))
  /** Returns the empty `ArraySeq`.
   *
   *  The same instance, backed by an empty `Array[AnyRef]`, is shared for all element
   *  types; the `ClassTag` is never consulted.
   *
   *  @tparam T the element type
   */
  def empty[T : ClassTag]: ArraySeq[T] = EmptyArraySeq.asInstanceOf[ArraySeq[T]]

  /** Builds a new `ArraySeq` containing the elements of the given collection.
   *
   *  The elements are copied into a new array whose element type is determined by the
   *  `ClassTag`, and the result wraps that array.
   *
   *  @tparam A the element type
   *  @param it the collection whose elements are copied
   *  @return a new `ArraySeq` with the elements of `it`
   */
  def from[A : ClassTag](it: scala.collection.IterableOnce[A]^): ArraySeq[A] = make(Array.from[A](it))

  /** Returns a new builder for an `ArraySeq`.
   *
   *  The builder collects elements into an array whose element type is determined by the
   *  `ClassTag`, then wraps the result without copying.
   *
   *  @tparam A the element type
   */
  def newBuilder[A : ClassTag]: Builder[A, ArraySeq[A]] = ArrayBuilder.make[A].mapResult(make)

  /** Wraps an existing `Array` into a `ArraySeq` of the proper primitive specialization type
   *  without copying.
   *
   *  Note that an array containing boxed primitives can be converted to a `ArraySeq` without
   *  copying. For example, `val a: Array[Any] = Array(1)` is an array of `Object` at runtime,
   *  containing `Integer`s. An `ArraySeq[Int]` can be obtained with a cast:
   *  `ArraySeq.make(a).asInstanceOf[ArraySeq[Int]]`. The values are still
   *  boxed, the resulting instance is an [[ArraySeq.ofRef]]. Writing
   *  `ArraySeq.make(a.asInstanceOf[Array[Int]])` does not work, it throws a `ClassCastException`
   *  at runtime.
   *
   *  @tparam T the element type of the array
   *  @param x the array to wrap
   *  @return an `ArraySeq` wrapping the given array using the appropriate primitive specialization, or `null` if `x` is `null`
   */
  def make[T](x: Array[T]): ArraySeq[T] = ((x: @unchecked) match {
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

  /** An `ArraySeq` backed by an array of reference values.
   *
   *  The given array is used directly, not copied: mutations of the array are visible
   *  through this wrapped array and vice versa.
   *
   *  @tparam T the element type, a reference type
   *  @param array the underlying array
   */
  @SerialVersionUID(3L)
  final class ofRef[T <: AnyRef | Null](val array: Array[T]) extends ArraySeq[T] {
    /** Returns a class tag for the runtime component type of the underlying array,
     *  which may be a subtype or supertype of `T`.
     */
    def elemTag: ClassTag[T] = ClassTag[T](array.getClass.getComponentType)
    /** Returns the number of elements in this wrapped array. */
    def length: Int = array.length
    /** Returns the element at the given index of the underlying array.
     *
     *  @param index the zero-based index of the element
     *  @throws ArrayIndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    def apply(index: Int): T = array(index)
    /** Replaces the element at the given index of the underlying array.
     *
     *  @param index the zero-based index of the element to replace
     *  @param elem the new value
     *  @throws ArrayIndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    def update(index: Int, elem: T): Unit = { array(index) = elem }
    /** Returns a hash code computed from the array elements, consistent with the hashing
     *  of other sequences.
     */
    override def hashCode() = MurmurHash3.arraySeqHash(array)
    /** Compares this wrapped array with another object for equality.
     *
     *  Two `ofRef` instances are equal if their underlying arrays contain equal
     *  elements (compared with `equals`) in the same order. Comparison with
     *  anything else falls back to general sequence equality.
     *
     *  @param that the object to compare with
     */
    override def equals(that: Any) = that match {
      case that: ofRef[?] =>
        Array.equals(
          this.array.asInstanceOf[Array[AnyRef]],
          that.array.asInstanceOf[Array[AnyRef]])
      case _ => super.equals(that)
    }
    /** Returns an iterator over the elements of the underlying array. */
    override def iterator: Iterator[T] = new ArrayOps.ArrayIterator[T](array)
    /** Returns a stepper for the elements of this wrapped array.
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
        new ObjectArrayStepper(array, 0, array.length)
      else shape.parUnbox(new ObjectArrayStepper(array, 0, array.length).asInstanceOf[AnyStepper[T] & EfficientSplit])
      ).asInstanceOf[S & EfficientSplit]
  }

  /** An `ArraySeq` backed by an `Array[Byte]`, storing its elements unboxed.
   *
   *  The given array is used directly, not copied: mutations of the array are visible
   *  through this wrapped array and vice versa.
   *
   *  @param array the underlying array
   */
  @SerialVersionUID(3L)
  final class ofByte(val array: Array[Byte]) extends ArraySeq[Byte] {
    // Type erases to `ManifestFactory.ByteManifest`, but can't annotate that because it's not accessible
    /** Returns `ClassTag.Byte`, the tag of the unboxed element type. */
    def elemTag: ClassTag.Byte.type = ClassTag.Byte
    /** Returns the number of elements in this wrapped array. */
    def length: Int = array.length
    /** Returns the element at the given index of the underlying array.
     *
     *  @param index the zero-based index of the element
     *  @throws ArrayIndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    def apply(index: Int): Byte = array(index)
    /** Replaces the element at the given index of the underlying array.
     *
     *  @param index the zero-based index of the element to replace
     *  @param elem the new value
     *  @throws ArrayIndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    def update(index: Int, elem: Byte): Unit = { array(index) = elem }
    /** Returns a hash code computed from the array elements, consistent with the hashing
     *  of other sequences.
     */
    override def hashCode() = MurmurHash3.arraySeqHash(array)
    /** Compares this wrapped array with another object for equality.
     *
     *  Two `ofByte` instances are equal if their underlying arrays contain the
     *  same elements in the same order. Comparison with anything else falls back
     *  to general sequence equality.
     *
     *  @param that the object to compare with
     */
    override def equals(that: Any) = that match {
      case that: ofByte => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
    /** Returns an iterator over the elements of the underlying array. */
    override def iterator: Iterator[Byte] = new ArrayOps.ArrayIterator[Byte](array)
    /** Returns a stepper for the elements of this wrapped array.
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
        AnyStepper.ofParIntStepper(new WidenedByteArrayStepper(array, 0, array.length))
      else new WidenedByteArrayStepper(array, 0, array.length)
      ).asInstanceOf[S & EfficientSplit]
  }

  /** An `ArraySeq` backed by an `Array[Short]`, storing its elements unboxed.
   *
   *  The given array is used directly, not copied: mutations of the array are visible
   *  through this wrapped array and vice versa.
   *
   *  @param array the underlying array
   */
  @SerialVersionUID(3L)
  final class ofShort(val array: Array[Short]) extends ArraySeq[Short] {
    // Type erases to `ManifestFactory.ShortManifest`, but can't annotate that because it's not accessible
    /** Returns `ClassTag.Short`, the tag of the unboxed element type. */
    def elemTag: ClassTag.Short.type = ClassTag.Short
    /** Returns the number of elements in this wrapped array. */
    def length: Int = array.length
    /** Returns the element at the given index of the underlying array.
     *
     *  @param index the zero-based index of the element
     *  @throws ArrayIndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    def apply(index: Int): Short = array(index)
    /** Replaces the element at the given index of the underlying array.
     *
     *  @param index the zero-based index of the element to replace
     *  @param elem the new value
     *  @throws ArrayIndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    def update(index: Int, elem: Short): Unit = { array(index) = elem }
    /** Returns a hash code computed from the array elements, consistent with the hashing
     *  of other sequences.
     */
    override def hashCode() = MurmurHash3.arraySeqHash(array)
    /** Compares this wrapped array with another object for equality.
     *
     *  Two `ofShort` instances are equal if their underlying arrays contain the
     *  same elements in the same order. Comparison with anything else falls back
     *  to general sequence equality.
     *
     *  @param that the object to compare with
     */
    override def equals(that: Any) = that match {
      case that: ofShort => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
    /** Returns an iterator over the elements of the underlying array. */
    override def iterator: Iterator[Short] = new ArrayOps.ArrayIterator[Short](array)
    /** Returns a stepper for the elements of this wrapped array.
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
        AnyStepper.ofParIntStepper(new WidenedShortArrayStepper(array, 0, array.length))
      else new WidenedShortArrayStepper(array, 0, array.length)
      ).asInstanceOf[S & EfficientSplit]
  }

  /** An `ArraySeq` backed by an `Array[Char]`, storing its elements unboxed.
   *
   *  The given array is used directly, not copied: mutations of the array are visible
   *  through this wrapped array and vice versa.
   *
   *  @param array the underlying array
   */
  @SerialVersionUID(3L)
  final class ofChar(val array: Array[Char]) extends ArraySeq[Char] {
    // Type erases to `ManifestFactory.CharManifest`, but can't annotate that because it's not accessible
    /** Returns `ClassTag.Char`, the tag of the unboxed element type. */
    def elemTag: ClassTag.Char.type = ClassTag.Char
    /** Returns the number of elements in this wrapped array. */
    def length: Int = array.length
    /** Returns the element at the given index of the underlying array.
     *
     *  @param index the zero-based index of the element
     *  @throws ArrayIndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    def apply(index: Int): Char = array(index)
    /** Replaces the element at the given index of the underlying array.
     *
     *  @param index the zero-based index of the element to replace
     *  @param elem the new value
     *  @throws ArrayIndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    def update(index: Int, elem: Char): Unit = { array(index) = elem }
    /** Returns a hash code computed from the array elements, consistent with the hashing
     *  of other sequences.
     */
    override def hashCode() = MurmurHash3.arraySeqHash(array)
    /** Compares this wrapped array with another object for equality.
     *
     *  Two `ofChar` instances are equal if their underlying arrays contain the
     *  same elements in the same order. Comparison with anything else falls back
     *  to general sequence equality.
     *
     *  @param that the object to compare with
     */
    override def equals(that: Any) = that match {
      case that: ofChar => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
    /** Returns an iterator over the elements of the underlying array. */
    override def iterator: Iterator[Char] = new ArrayOps.ArrayIterator[Char](array)
    /** Returns a stepper for the elements of this wrapped array.
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
        AnyStepper.ofParIntStepper(new WidenedCharArrayStepper(array, 0, array.length))
      else new WidenedCharArrayStepper(array, 0, array.length)
      ).asInstanceOf[S & EfficientSplit]

    /** Appends the characters of this wrapped array to a string builder,
     *  preceded by the string `start`, separated by the string `sep`, and
     *  followed by the string `end`.
     *
     *  Optimized for the underlying `Array[Char]`: with an empty separator the
     *  whole array is appended in a single call, and otherwise the required
     *  capacity is reserved up front.
     *
     *  @param sb the string builder to append to
     *  @param start the starting string
     *  @param sep the separator string
     *  @param end the ending string
     *  @return the string builder `sb` to which elements were appended
     */
    override def addString(sb: StringBuilder, start: String, sep: String, end: String): sb.type = {
      val jsb = sb.underlying
      if (start.length != 0) jsb.append(start)
      val len = array.length
      if (len != 0) {
        if (sep.isEmpty) jsb.append(array)
        else {
          jsb.ensureCapacity(jsb.length + len + end.length + (len - 1) * sep.length)
          jsb.append(array(0))
          var i = 1
          while (i < len) {
            jsb.append(sep)
            jsb.append(array(i))
            i += 1
          }
        }
      }
      if (end.length != 0) jsb.append(end)
      sb
    }
  }

  /** An `ArraySeq` backed by an `Array[Int]`, storing its elements unboxed.
   *
   *  The given array is used directly, not copied: mutations of the array are visible
   *  through this wrapped array and vice versa.
   *
   *  @param array the underlying array
   */
  @SerialVersionUID(3L)
  final class ofInt(val array: Array[Int]) extends ArraySeq[Int] {
    // Type erases to `ManifestFactory.IntManifest`, but can't annotate that because it's not accessible
    /** Returns `ClassTag.Int`, the tag of the unboxed element type. */
    def elemTag: ClassTag.Int.type = ClassTag.Int
    /** Returns the number of elements in this wrapped array. */
    def length: Int = array.length
    /** Returns the element at the given index of the underlying array.
     *
     *  @param index the zero-based index of the element
     *  @throws ArrayIndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    def apply(index: Int): Int = array(index)
    /** Replaces the element at the given index of the underlying array.
     *
     *  @param index the zero-based index of the element to replace
     *  @param elem the new value
     *  @throws ArrayIndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    def update(index: Int, elem: Int): Unit = { array(index) = elem }
    /** Returns a hash code computed from the array elements, consistent with the hashing
     *  of other sequences.
     */
    override def hashCode() = MurmurHash3.arraySeqHash(array)
    /** Compares this wrapped array with another object for equality.
     *
     *  Two `ofInt` instances are equal if their underlying arrays contain the
     *  same elements in the same order. Comparison with anything else falls back
     *  to general sequence equality.
     *
     *  @param that the object to compare with
     */
    override def equals(that: Any) = that match {
      case that: ofInt => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
    /** Returns an iterator over the elements of the underlying array. */
    override def iterator: Iterator[Int] = new ArrayOps.ArrayIterator[Int](array)
    /** Returns a stepper for the elements of this wrapped array.
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
        AnyStepper.ofParIntStepper(new IntArrayStepper(array, 0, array.length))
      else new IntArrayStepper(array, 0, array.length)
      ).asInstanceOf[S & EfficientSplit]
  }

  /** An `ArraySeq` backed by an `Array[Long]`, storing its elements unboxed.
   *
   *  The given array is used directly, not copied: mutations of the array are visible
   *  through this wrapped array and vice versa.
   *
   *  @param array the underlying array
   */
  @SerialVersionUID(3L)
  final class ofLong(val array: Array[Long]) extends ArraySeq[Long] {
    // Type erases to `ManifestFactory.LongManifest`, but can't annotate that because it's not accessible
    /** Returns `ClassTag.Long`, the tag of the unboxed element type. */
    def elemTag: ClassTag.Long.type = ClassTag.Long
    /** Returns the number of elements in this wrapped array. */
    def length: Int = array.length
    /** Returns the element at the given index of the underlying array.
     *
     *  @param index the zero-based index of the element
     *  @throws ArrayIndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    def apply(index: Int): Long = array(index)
    /** Replaces the element at the given index of the underlying array.
     *
     *  @param index the zero-based index of the element to replace
     *  @param elem the new value
     *  @throws ArrayIndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    def update(index: Int, elem: Long): Unit = { array(index) = elem }
    /** Returns a hash code computed from the array elements, consistent with the hashing
     *  of other sequences.
     */
    override def hashCode() = MurmurHash3.arraySeqHash(array)
    /** Compares this wrapped array with another object for equality.
     *
     *  Two `ofLong` instances are equal if their underlying arrays contain the
     *  same elements in the same order. Comparison with anything else falls back
     *  to general sequence equality.
     *
     *  @param that the object to compare with
     */
    override def equals(that: Any) = that match {
      case that: ofLong => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
    /** Returns an iterator over the elements of the underlying array. */
    override def iterator: Iterator[Long] = new ArrayOps.ArrayIterator[Long](array)
    /** Returns a stepper for the elements of this wrapped array.
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
        AnyStepper.ofParLongStepper(new LongArrayStepper(array, 0, array.length))
      else new LongArrayStepper(array, 0, array.length)
      ).asInstanceOf[S & EfficientSplit]
  }

  /** An `ArraySeq` backed by an `Array[Float]`, storing its elements unboxed.
   *
   *  The given array is used directly, not copied: mutations of the array are visible
   *  through this wrapped array and vice versa.
   *
   *  @param array the underlying array
   */
  @SerialVersionUID(3L)
  final class ofFloat(val array: Array[Float]) extends ArraySeq[Float] {
    // Type erases to `ManifestFactory.FloatManifest`, but can't annotate that because it's not accessible
    /** Returns `ClassTag.Float`, the tag of the unboxed element type. */
    def elemTag: ClassTag.Float.type = ClassTag.Float
    /** Returns the number of elements in this wrapped array. */
    def length: Int = array.length
    /** Returns the element at the given index of the underlying array.
     *
     *  @param index the zero-based index of the element
     *  @throws ArrayIndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    def apply(index: Int): Float = array(index)
    /** Replaces the element at the given index of the underlying array.
     *
     *  @param index the zero-based index of the element to replace
     *  @param elem the new value
     *  @throws ArrayIndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    def update(index: Int, elem: Float): Unit = { array(index) = elem }
    /** Returns a hash code computed from the array elements, consistent with the hashing
     *  of other sequences.
     */
    override def hashCode() = MurmurHash3.arraySeqHash(array)
    /** Compares this wrapped array with another object for equality.
     *
     *  Two `ofFloat` instances are equal if they share the same underlying array,
     *  or if their underlying arrays contain the same elements in the same order,
     *  compared with `==`. A `NaN` element is not `==` to anything, so two
     *  distinct arrays containing `NaN` are never equal. Comparison with anything
     *  else falls back to general sequence equality.
     *
     *  @param that the object to compare with
     */
    override def equals(that: Any) = that match {
      case that: ofFloat =>
        val thatArray = that.array
        (array eq thatArray) || array.length == thatArray.length && {
          var i = 0
          while (i < array.length && array(i) == thatArray(i)) i += 1
          i >= array.length
        }
      case _ => super.equals(that)
    }
    /** Returns an iterator over the elements of the underlying array. */
    override def iterator: Iterator[Float] = new ArrayOps.ArrayIterator[Float](array)
    /** Returns a stepper for the elements of this wrapped array.
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
        AnyStepper.ofParDoubleStepper(new WidenedFloatArrayStepper(array, 0, array.length))
      else new WidenedFloatArrayStepper(array, 0, array.length)
      ).asInstanceOf[S & EfficientSplit]
  }

  /** An `ArraySeq` backed by an `Array[Double]`, storing its elements unboxed.
   *
   *  The given array is used directly, not copied: mutations of the array are visible
   *  through this wrapped array and vice versa.
   *
   *  @param array the underlying array
   */
  @SerialVersionUID(3L)
  final class ofDouble(val array: Array[Double]) extends ArraySeq[Double] {
    // Type erases to `ManifestFactory.DoubleManifest`, but can't annotate that because it's not accessible
    /** Returns `ClassTag.Double`, the tag of the unboxed element type. */
    def elemTag: ClassTag.Double.type = ClassTag.Double
    /** Returns the number of elements in this wrapped array. */
    def length: Int = array.length
    /** Returns the element at the given index of the underlying array.
     *
     *  @param index the zero-based index of the element
     *  @throws ArrayIndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    def apply(index: Int): Double = array(index)
    /** Replaces the element at the given index of the underlying array.
     *
     *  @param index the zero-based index of the element to replace
     *  @param elem the new value
     *  @throws ArrayIndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    def update(index: Int, elem: Double): Unit = { array(index) = elem }
    /** Returns a hash code computed from the array elements, consistent with the hashing
     *  of other sequences.
     */
    override def hashCode() = MurmurHash3.arraySeqHash(array)
    /** Compares this wrapped array with another object for equality.
     *
     *  Two `ofDouble` instances are equal if they share the same underlying array,
     *  or if their underlying arrays contain the same elements in the same order,
     *  compared with `==`. A `NaN` element is not `==` to anything, so two
     *  distinct arrays containing `NaN` are never equal. Comparison with anything
     *  else falls back to general sequence equality.
     *
     *  @param that the object to compare with
     */
    override def equals(that: Any) = that match {
      case that: ofDouble =>
        val thatArray = that.array
        (array eq thatArray) || array.length == thatArray.length && {
          var i = 0
          while (i < array.length && array(i) == thatArray(i)) i += 1
          i >= array.length
        }
      case _ => super.equals(that)
    }
    /** Returns an iterator over the elements of the underlying array. */
    override def iterator: Iterator[Double] = new ArrayOps.ArrayIterator[Double](array)
    /** Returns a stepper for the elements of this wrapped array.
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
        AnyStepper.ofParDoubleStepper(new DoubleArrayStepper(array, 0, array.length))
      else new DoubleArrayStepper(array, 0, array.length)
      ).asInstanceOf[S & EfficientSplit]
  }

  /** An `ArraySeq` backed by an `Array[Boolean]`, storing its elements unboxed.
   *
   *  The given array is used directly, not copied: mutations of the array are visible
   *  through this wrapped array and vice versa.
   *
   *  @param array the underlying array
   */
  @SerialVersionUID(3L)
  final class ofBoolean(val array: Array[Boolean]) extends ArraySeq[Boolean] {
    // Type erases to `ManifestFactory.BooleanManifest`, but can't annotate that because it's not accessible
    /** Returns `ClassTag.Boolean`, the tag of the unboxed element type. */
    def elemTag: ClassTag.Boolean.type = ClassTag.Boolean
    /** Returns the number of elements in this wrapped array. */
    def length: Int = array.length
    /** Returns the element at the given index of the underlying array.
     *
     *  @param index the zero-based index of the element
     *  @throws ArrayIndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    def apply(index: Int): Boolean = array(index)
    /** Replaces the element at the given index of the underlying array.
     *
     *  @param index the zero-based index of the element to replace
     *  @param elem the new value
     *  @throws ArrayIndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    def update(index: Int, elem: Boolean): Unit = { array(index) = elem }
    /** Returns a hash code computed from the array elements, consistent with the hashing
     *  of other sequences.
     */
    override def hashCode() = MurmurHash3.arraySeqHash(array)
    /** Compares this wrapped array with another object for equality.
     *
     *  Two `ofBoolean` instances are equal if their underlying arrays contain the
     *  same elements in the same order. Comparison with anything else falls back
     *  to general sequence equality.
     *
     *  @param that the object to compare with
     */
    override def equals(that: Any) = that match {
      case that: ofBoolean => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
    /** Returns an iterator over the elements of the underlying array. */
    override def iterator: Iterator[Boolean] = new ArrayOps.ArrayIterator[Boolean](array)
    /** Returns a stepper for the elements of this wrapped array.
     *
     *  There is no primitive stepper shape for `Boolean`, so the stepper always
     *  steps over boxed values.
     *
     *  @tparam S the type of the stepper, determined by `shape`
     *  @param shape implicit evidence selecting the stepper type for the element type `Boolean`
     *  @return a stepper over the elements, supporting efficient splitting for
     *          parallel processing
     */
    override def stepper[S <: Stepper[?]](implicit shape: StepperShape[Boolean, S]): S & EfficientSplit =
      new BoxedBooleanArrayStepper(array, 0, array.length).asInstanceOf[S & EfficientSplit]
  }

  /** An `ArraySeq` backed by an `Array[Unit]` (at runtime, an array of boxed unit values).
   *
   *  The given array is used directly, not copied: mutations of the array are visible
   *  through this wrapped array and vice versa.
   *
   *  @param array the underlying array
   */
  @SerialVersionUID(3L)
  final class ofUnit(val array: Array[Unit]) extends ArraySeq[Unit] {
    // Type erases to `ManifestFactory.UnitManifest`, but can't annotate that because it's not accessible
    /** Returns `ClassTag.Unit`, the tag of the element type. */
    def elemTag: ClassTag.Unit.type = ClassTag.Unit
    /** Returns the number of elements in this wrapped array. */
    def length: Int = array.length
    /** Returns the unit value at the given index of the underlying array.
     *
     *  @param index the zero-based index of the element
     *  @throws ArrayIndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    def apply(index: Int): Unit = array(index)
    /** Replaces the element at the given index of the underlying array.
     *
     *  @param index the zero-based index of the element to replace
     *  @param elem the new value
     *  @throws ArrayIndexOutOfBoundsException if `index` is negative or not less than
     *          `length`
     */
    def update(index: Int, elem: Unit): Unit = { array(index) = elem }
    /** Returns a hash code computed from the array elements, consistent with the hashing
     *  of other sequences.
     */
    override def hashCode() = MurmurHash3.arraySeqHash(array)
    /** Compares this wrapped array with another object for equality.
     *
     *  Two `ofUnit` instances are equal if their underlying arrays have the same
     *  length, since all unit values are equal. Comparison with anything else
     *  falls back to general sequence equality.
     *
     *  @param that the object to compare with
     */
    override def equals(that: Any) = that match {
      case that: ofUnit => array.length == that.array.length
      case _ => super.equals(that)
    }
    /** Returns an iterator over the elements of the underlying array. */
    override def iterator: Iterator[Unit] = new ArrayOps.ArrayIterator[Unit](array)
    /** Returns a stepper for the elements of this wrapped array.
     *
     *  The stepper always steps over the boxed unit values.
     *
     *  @tparam S the type of the stepper, determined by `shape`
     *  @param shape implicit evidence selecting the stepper type for the element type `Unit`
     *  @return a stepper over the elements, supporting efficient splitting for
     *          parallel processing
     */
    override def stepper[S <: Stepper[?]](implicit shape: StepperShape[Unit, S]): S & EfficientSplit =
      new ObjectArrayStepper[AnyRef](array.asInstanceOf[Array[AnyRef]], 0, array.length).asInstanceOf[S & EfficientSplit]
  }
}
