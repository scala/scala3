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

package scala.collection
package mutable

import scala.language.`2.13`

import scala.reflect.ClassTag
import scala.runtime.BoxedUnit

import scala.scalajs.js
import scala.scalajs.LinkingInfo

/** A builder class for arrays.
 *
 *  @since 2.8
 *
 *  @tparam T    the type of the elements for the builder.
 */
@SerialVersionUID(3L)
sealed abstract class ArrayBuilder[T]
  extends ReusableBuilder[T, Array[T]]
    with Serializable {
  protected[this] var capacity: Int = 0
  protected[this] def elems: Array[T] | Null // may not be allocated at size = capacity = 0
  protected var size: Int = 0

  def length: Int = size

  override def knownSize: Int = size

  protected[this] final def ensureSize(size: Int): Unit = {
    if (capacity < size || capacity == 0) {
      var newsize = if (capacity == 0) 16 else capacity * 2
      while (newsize < size) newsize *= 2
      resize(newsize)
    }
  }

  override final def sizeHint(size: Int): Unit =
    if (capacity < size) resize(size)

  def clear(): Unit = size = 0

  protected[this] def resize(size: Int): Unit

  /** Adds all elements of an array.
   *
   *  @param xs the array from which to add elements
   */
  def addAll(xs: Array[_ <: T]): this.type = addAll(xs, 0, xs.length)

  /** Adds a slice of an array.
   *
   *  @param xs the array from which to add a slice of elements
   *  @param offset the starting index in `xs` from which to copy elements
   *  @param length the number of elements to copy from `xs`
   */
  def addAll(xs: Array[_ <: T], offset: Int, length: Int): this.type = {
    ensureSize(this.size + length)
    Array.copy(xs, offset, elems.nn, this.size, length)
    size += length
    this
  }

  override def addAll(xs: IterableOnce[T]): this.type = {
    val k = xs.knownSize
    if(k > 0) {
      ensureSize(this.size + k)
      xs match {
        case xs: Iterable[T] => xs.copyToArray(elems.nn, this.size)
        case _ => xs.iterator.copyToArray(elems.nn, this.size)
      }
      size += k
    } else if(k < 0) super.addAll(xs)
    this
  }
}

/** A companion object for array builders.
 *
 *  @since 2.8
 */
object ArrayBuilder {

  /** Creates a new arraybuilder of type `T`.
   *
   *  @tparam T     type of the elements for the array builder, with a `ClassTag` context bound.
   *  @return       a new empty array builder.
   */
  @inline
  def make[T: ClassTag]: ArrayBuilder[T] =
    if (LinkingInfo.isWebAssembly) makeForWasm
    else makeForJS

  /** Implementation of `make` for JS.
   *
   *  @tparam T the element type of the array builder, with a `ClassTag` context bound
   */
  @inline
  private def makeForJS[T: ClassTag]: ArrayBuilder[T] =
    new ArrayBuilder.generic[T](implicitly[ClassTag[T]].runtimeClass)

  /** Implementation of `make` for Wasm.
   *
   *  This is the original upstream implementation.
   *
   *  @tparam T the element type of the array builder, with a `ClassTag` context bound
   *  @return a new array builder specialized for the runtime type of `T`
   */
  @inline
  private def makeForWasm[T: ClassTag]: ArrayBuilder[T] = {
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
      case _                        => new ArrayBuilder.ofRef[T with AnyRef]()(tag.asInstanceOf[ClassTag[T with AnyRef]]).asInstanceOf[ArrayBuilder[T]]
    }
  }

  /** A generic ArrayBuilder optimized for Scala.js.
   *
   *  @tparam T              type of elements for the array builder.
   *  @param  elementClass   runtime class of the elements in the array.
   */
  @inline
  private final class generic[T](elementClass: Class[_]) extends ArrayBuilder[T] {

    private val isCharArrayBuilder = classOf[Char] == elementClass
    protected[this] def elems: Array[T] = throw new Error("unreachable")
    private var jsElems: js.Array[Any] = js.Array()

    override def length: Int = jsElems.length

    override def knownSize: Int = jsElems.length

    def addOne(elem: T): this.type = {
      val unboxedElem =
        if (isCharArrayBuilder) elem.asInstanceOf[Char].toInt
        else if (elem == null) zeroOf(elementClass)
        else elem
      jsElems.push(unboxedElem)
      this
    }

    /** Adds a slice of an array.
     *
     *  @param xs the array from which to add a slice of elements
     *  @param offset the starting index in `xs` from which to copy elements
     *  @param length the number of elements to copy from `xs`
     */
    override def addAll(xs: Array[_ <: T], offset: Int, length: Int): this.type = {
      val end = offset + length
      var i = offset
      while (i < end) {
        this += xs(i)
        i += 1
      }
      this
    }

    override def addAll(xs: IterableOnce[T]): this.type = {
      val it = xs.iterator
      while (it.hasNext) {
        this += it.next()
      }
      this
    }

    override def clear(): Unit =
      jsElems = js.Array()

    protected[this] def resize(size: Int): Unit = ()

    def result(): Array[T] = {
      val elemRuntimeClass =
        if (classOf[Unit] == elementClass) classOf[BoxedUnit]
        else if (classOf[Null] == elementClass || classOf[Nothing] == elementClass) classOf[Object]
        else elementClass
      genericArrayBuilderResult(elemRuntimeClass, jsElems)
    }

    override def toString(): String = "ArrayBuilder.generic"
  }

  // Intrinsic
  private def zeroOf(runtimeClass: Class[_]): Any = runtimeClass match {
    case java.lang.Byte.TYPE      => 0.toByte
    case java.lang.Short.TYPE     => 0.toShort
    case java.lang.Character.TYPE => 0 // yes, as an Int
    case java.lang.Integer.TYPE   => 0
    case java.lang.Long.TYPE      => 0L
    case java.lang.Float.TYPE     => 0.0f
    case java.lang.Double.TYPE    => 0.0
    case java.lang.Boolean.TYPE   => false
    case java.lang.Void.TYPE      => ()
    case _                        => null
  }

  // Intrinsic
  private def genericArrayBuilderResult[T](runtimeClass: Class[_],
      a: js.Array[Any]): Array[T] = {
    val len = a.length

    if (classOf[Char] == runtimeClass) {
      val result = new Array[Char](len)
      var i = 0
      while (i != len) {
        result(i) = a(i).asInstanceOf[Int].toChar
        i += 1
      }
      result.asInstanceOf[Array[T]]
    } else {
      val result: Array[T] = java.lang.reflect.Array.newInstance(
          runtimeClass, len).asInstanceOf[Array[T]]
      var i = 0
      while (i != len) {
        result(i) = a(i).asInstanceOf[T]
        i += 1
      }
      result
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

    protected var elems: Array[T] | Null = null

    private def mkArray(size: Int): Array[T] = {
      if (capacity == size && capacity > 0) elems.nn
      else if (elems eq null) new Array[T](size)
      else java.util.Arrays.copyOf[T](elems, size)
    }

    protected[this] def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    def addOne(elem: T): this.type = {
      ensureSize(size + 1)
      elems.nn(size) = elem
      size += 1
      this
    }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        val res = elems.nn
        elems = null
        res
      }
      else mkArray(size)
    }

    override def clear(): Unit = {
      super.clear()
      if(elems ne null) java.util.Arrays.fill(elems.asInstanceOf[Array[AnyRef]], null)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofRef[_] => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofRef"
  }

  /** A class for array builders for arrays of `byte`s. It can be reused. */
  @SerialVersionUID(3L)
  final class ofByte extends ArrayBuilder[Byte] {

    protected var elems: Array[Byte] | Null = null

    private def mkArray(size: Int): Array[Byte] = {
      val newelems = new Array[Byte](size)
      if (this.size > 0) Array.copy(elems.nn, 0, newelems, 0, this.size)
      newelems
    }

    protected[this] def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    def addOne(elem: Byte): this.type = {
      ensureSize(size + 1)
      elems.nn(size) = elem
      size += 1
      this
    }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        val res = elems.nn
        elems = null
        res
      }
      else mkArray(size)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofByte => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofByte"
  }

  /** A class for array builders for arrays of `short`s. It can be reused. */
  @SerialVersionUID(3L)
  final class ofShort extends ArrayBuilder[Short] {

    protected var elems: Array[Short] | Null = null

    private def mkArray(size: Int): Array[Short] = {
      val newelems = new Array[Short](size)
      if (this.size > 0) Array.copy(elems.nn, 0, newelems, 0, this.size)
      newelems
    }

    protected[this] def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    def addOne(elem: Short): this.type = {
      ensureSize(size + 1)
      elems.nn(size) = elem
      size += 1
      this
    }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        val res = elems.nn
        elems = null
        res
      }
      else mkArray(size)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofShort => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofShort"
  }

  /** A class for array builders for arrays of `char`s. It can be reused. */
  @SerialVersionUID(3L)
  final class ofChar extends ArrayBuilder[Char] {

    protected var elems: Array[Char] | Null = null

    private def mkArray(size: Int): Array[Char] = {
      val newelems = new Array[Char](size)
      if (this.size > 0) Array.copy(elems.nn, 0, newelems, 0, this.size)
      newelems
    }

    protected[this] def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    def addOne(elem: Char): this.type = {
      ensureSize(size + 1)
      elems.nn(size) = elem
      size += 1
      this
    }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        val res = elems.nn
        elems = null
        res
      }
      else mkArray(size)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofChar => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofChar"
  }

  /** A class for array builders for arrays of `int`s. It can be reused. */
  @SerialVersionUID(3L)
  final class ofInt extends ArrayBuilder[Int] {

    protected var elems: Array[Int] | Null = null

    private def mkArray(size: Int): Array[Int] = {
      val newelems = new Array[Int](size)
      if (this.size > 0) Array.copy(elems.nn, 0, newelems, 0, this.size)
      newelems
    }

    protected[this] def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    def addOne(elem: Int): this.type = {
      ensureSize(size + 1)
      elems.nn(size) = elem
      size += 1
      this
    }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        val res = elems.nn
        elems = null
        res
      }
      else mkArray(size)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofInt => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofInt"
  }

  /** A class for array builders for arrays of `long`s. It can be reused. */
  @SerialVersionUID(3L)
  final class ofLong extends ArrayBuilder[Long] {

    protected var elems: Array[Long] | Null = null

    private def mkArray(size: Int): Array[Long] = {
      val newelems = new Array[Long](size)
      if (this.size > 0) Array.copy(elems.nn, 0, newelems, 0, this.size)
      newelems
    }

    protected[this] def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    def addOne(elem: Long): this.type = {
      ensureSize(size + 1)
      elems.nn(size) = elem
      size += 1
      this
    }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        val res = elems.nn
        elems = null
        res
      }
      else mkArray(size)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofLong => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofLong"
  }

  /** A class for array builders for arrays of `float`s. It can be reused. */
  @SerialVersionUID(3L)
  final class ofFloat extends ArrayBuilder[Float] {

    protected var elems: Array[Float] | Null = null

    private def mkArray(size: Int): Array[Float] = {
      val newelems = new Array[Float](size)
      if (this.size > 0) Array.copy(elems.nn, 0, newelems, 0, this.size)
      newelems
    }

    protected[this] def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    def addOne(elem: Float): this.type = {
      ensureSize(size + 1)
      elems.nn(size) = elem
      size += 1
      this
    }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        val res = elems.nn
        elems = null
        res
      }
      else mkArray(size)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofFloat => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofFloat"
  }

  /** A class for array builders for arrays of `double`s. It can be reused. */
  @SerialVersionUID(3L)
  final class ofDouble extends ArrayBuilder[Double] {

    protected var elems: Array[Double] | Null = null

    private def mkArray(size: Int): Array[Double] = {
      val newelems = new Array[Double](size)
      if (this.size > 0) Array.copy(elems.nn, 0, newelems, 0, this.size)
      newelems
    }

    protected[this] def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    def addOne(elem: Double): this.type = {
      ensureSize(size + 1)
      elems.nn(size) = elem
      size += 1
      this
    }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        val res = elems.nn
        elems = null
        res
      }
      else mkArray(size)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofDouble => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofDouble"
  }

  /** A class for array builders for arrays of `boolean`s. It can be reused. */
  @SerialVersionUID(3L)
  class ofBoolean extends ArrayBuilder[Boolean] {

    protected var elems: Array[Boolean] | Null = null

    private def mkArray(size: Int): Array[Boolean] = {
      val newelems = new Array[Boolean](size)
      if (this.size > 0) Array.copy(elems.nn, 0, newelems, 0, this.size)
      newelems
    }

    protected[this] def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    def addOne(elem: Boolean): this.type = {
      ensureSize(size + 1)
      elems.nn(size) = elem
      size += 1
      this
    }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        val res = elems.nn
        elems = null
        res
      }
      else mkArray(size)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofBoolean => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofBoolean"
  }

  /** A class for array builders for arrays of `Unit` type. It can be reused. */
  @SerialVersionUID(3L)
  final class ofUnit extends ArrayBuilder[Unit] {

    protected def elems: Array[Unit] | Null = throw new UnsupportedOperationException()

    def addOne(elem: Unit): this.type = {
      size += 1
      this
    }

    override def addAll(xs: IterableOnce[Unit]): this.type = {
      size += xs.iterator.size
      this
    }

    override def addAll(xs: Array[_ <: Unit], offset: Int, length: Int): this.type = {
      size += length
      this
    }

    def result() = {
      val ans = new Array[Unit](size)
      var i = 0
      while (i < size) { ans(i) = (); i += 1 }
      ans
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofUnit => (size == x.size)
      case _ => false
    }

    protected[this] def resize(size: Int): Unit = ()

    override def toString = "ArrayBuilder.ofUnit"
  }
}
