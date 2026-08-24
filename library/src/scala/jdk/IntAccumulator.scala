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

package scala.jdk

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.Spliterator
import java.util.function.{Consumer, IntConsumer}
import java.{lang => jl}

import scala.annotation._
import scala.collection.Stepper.EfficientSplit
import scala.collection.{AnyStepper, Factory, IntStepper, SeqFactory, Stepper, StepperShape, mutable}
import scala.language.implicitConversions

import scala.language.`2.13`

/** A specialized Accumulator that holds `Int`s without boxing, see [[Accumulator]]. */
final class IntAccumulator
  extends Accumulator[Int, AnyAccumulator, IntAccumulator]
    with mutable.SeqOps[Int, AnyAccumulator, IntAccumulator]
    with Serializable {
  private[jdk] var current: Array[Int] = IntAccumulator.emptyIntArray
  private[jdk] var history: Array[Array[Int]] = IntAccumulator.emptyIntArrayArray

  private[jdk] def cumulative(i: Int) = { val x = history(i); x(x.length-2).toLong << 32 | (x(x.length-1)&0xFFFFFFFFL) }

  /** Returns `"IntAccumulator"`, the prefix used by `toString`. */
  override protected def className: String = "IntAccumulator"

  /** Returns a [[scala.collection.Stepper]] over the elements of this `IntAccumulator` that
   *  supports efficient splitting, so that it can be traversed in parallel.
   *
   *  @tparam S the specific stepper type, determined by `shape`
   *  @param shape the implicit shape selecting the stepper specialized for `Int`; only
   *         `IntShape` and `ReferenceShape` are supported
   *  @return a stepper of shape `S`; for `IntShape` it steps over the elements without boxing,
   *          for `ReferenceShape` it boxes them as `java.lang.Integer`
   */
  def efficientStepper[S <: Stepper[?]](implicit shape: StepperShape[Int, S]): S & EfficientSplit = {
    val st = new IntAccumulatorStepper(this)
    val r =
      if (shape.shape == StepperShape.IntShape) st
      else {
        assert(shape.shape == StepperShape.ReferenceShape, s"unexpected StepperShape: $shape")
        AnyStepper.ofParIntStepper(st)
      }
    r.asInstanceOf[S & EfficientSplit]
  }

  private def expand(): Unit = {
    if (index > 0) {
      val cuml = (if (hIndex > 0) cumulative(hIndex-1) else 0) + index
      current(current.length-2) = (cuml >>> 32).toInt
      current(current.length-1) = (cuml & 0xFFFFFFFFL).toInt
      if (hIndex >= history.length) hExpand()
      history(hIndex) = current
      hIndex += 1
    }
    current = new Array[Int](nextBlockSize+1)
    index = 0
  }

  private def hExpand(): Unit = {
    if (hIndex == 0) history = new Array[Array[Int]](4)
    else history = java.util.Arrays.copyOf(history, history.length << 1)
  }

  /** Appends an element to this `IntAccumulator`.
   *
   *  @param a the `Int` value to append
   *  @return this `IntAccumulator` with the element appended
   */
  def addOne(a: Int): this.type = {
    totalSize += 1
    if (index+2 >= current.length) expand()
    current(index) = a
    index += 1
    this
  }

  /** Result collection consisting of all elements appended so far. */
  override def result(): IntAccumulator = this

  /** Removes all elements from `that` and appends them to this `IntAccumulator`.
   *
   *  @param that the `IntAccumulator` to drain elements from; it will be empty after this operation
   */
  def drain(that: IntAccumulator): Unit = {
    var h = 0
    var prev = 0L
    var more = true
    while (more && h < that.hIndex) {
      val cuml = that.cumulative(h)
      val n = (cuml - prev).toInt
      if (current.length - index - 2 >= n) {
        System.arraycopy(that.history(h), 0, current, index, n)
        prev = cuml
        index += n
        h += 1
      }
      else more = false
    }
    if (h >= that.hIndex && current.length - index - 2 >= that.index) {
      if (that.index > 0) System.arraycopy(that.current, 0, current, index, that.index)
      index += that.index
    }
    else {
      val slots = (if (index > 0) 1 else 0) + that.hIndex - h
      if (hIndex + slots > history.length) {
        val n = math.max(4, 1 << (32 - jl.Integer.numberOfLeadingZeros(1 + hIndex + slots)))
        history = java.util.Arrays.copyOf(history, n)
      }
      var pv = if (hIndex > 0) cumulative(hIndex-1) else 0L
      if (index > 0) {
        val x =
          if (index < (current.length >>> 3) && current.length - 1 > 32) {
            val ans = java.util.Arrays.copyOf(current, index + 2)
            ans(ans.length - 2) = current(current.length - 2)
            ans(ans.length - 1) = current(current.length - 1)
            ans
          }
          else current
        pv = pv + index
        x(x.length - 2) = (pv >>> 32).toInt
        x(x.length - 1) = (pv & 0xFFFFFFFFL).toInt
        history(hIndex) = x
        hIndex += 1
      }
      while (h < that.hIndex) {
        val cuml = that.cumulative(h)
        pv = pv + cuml - prev
        prev = cuml
        val x = that.history(h)
        x(x.length - 2) = (pv >>> 32).toInt
        x(x.length - 1) = (pv & 0xFFFFFFFFL).toInt
        history(hIndex) = x
        h += 1
        hIndex += 1
      }
      index = that.index
      current = that.current
    }
    totalSize += that.totalSize
    that.clear()
  }

  /** Removes all accumulated elements from this `IntAccumulator`, releasing the arrays that held them. */
  override def clear(): Unit = {
    super.clear()
    current = IntAccumulator.emptyIntArray
    history = IntAccumulator.emptyIntArrayArray
  }

  /** Retrieves the `ix`th element.
   *
   *  @param ix the zero-based index of the element to retrieve, as a `Long`
   *  @return the `Int` element at index `ix`
   */
  def apply(ix: Long): Int = {
    if (totalSize - ix <= index || hIndex == 0) current((ix - (totalSize - index)).toInt)
    else {
      val w = seekSlot(ix)
      history((w >>> 32).toInt)((w & 0xFFFFFFFFL).toInt)
    }
  }

  /** Retrieves the `ix`th element, using an `Int` index.
   *
   *  @param i the zero-based index of the element to retrieve
   *  @return the `Int` element at index `i`
   */
  def apply(i: Int): Int = apply(i.toLong)

  /** Replaces the element at index `idx` with `elem`.
   *
   *  `idx` is not validated, and an out-of-range index has more than one possible outcome. It can
   *  land in unused capacity of the current array, in which case the write silently succeeds
   *  without changing any element this accumulator reports. Because the offset into the current
   *  array is computed as a `Long` and then narrowed to an `Int`, an index far enough out of range
   *  can also wrap onto an occupied slot and silently overwrite an element this accumulator does
   *  report. The same narrowing happens on the other branch: `seekSlot` narrows the index it is
   *  given to an `Int` when locating a slot in `history`, so an out-of-range index can wrap onto
   *  an occupied history slot as well. Otherwise the write throws.
   *
   *  @param idx the zero-based index of the element to replace
   *  @param elem the `Int` value to store at index `idx`
   *  @throws ArrayIndexOutOfBoundsException if `idx` is out of range and the computed offset falls
   *          outside the array being written, rather than into unused current-array capacity or
   *          onto a slot reached by `Int` wraparound
   */
  def update(idx: Long, elem: Int): Unit = {
    if (totalSize - idx <= index || hIndex == 0) current((idx - (totalSize - index)).toInt) = elem
    else {
      val w = seekSlot(idx)
      history((w >>> 32).toInt)((w & 0xFFFFFFFFL).toInt) = elem
    }
  }

  /** Replaces the element at index `idx` with `elem`, using an `Int` index.
   *
   *  `idx` is not validated, and an out-of-range index has more than one possible outcome. It can
   *  land in unused capacity of the current array, in which case the write silently succeeds
   *  without changing any element this accumulator reports. Otherwise the write throws. An `Int`
   *  index is widened to a `Long` without loss, so it can never wrap onto an occupied slot the way
   *  a sufficiently large `Long` index can.
   *
   *  @param idx the zero-based index of the element to replace
   *  @param elem the `Int` value to store at index `idx`
   *  @throws ArrayIndexOutOfBoundsException if `idx` is out of range and the computed offset falls
   *          outside the array being written, rather than into unused current-array capacity
   */
  def update(idx: Int, elem: Int): Unit = update(idx.toLong, elem)

  /** Returns an `Iterator` over the contents of this `IntAccumulator`. The `Iterator` is not specialized. */
  def iterator: Iterator[Int] = stepper.iterator

  /** Applies `f` to every element of this `IntAccumulator`, in order.
   *
   *  Elements are read through an [[scala.collection.IntStepper]], so a function specialized for
   *  `Int` receives them without boxing.
   *
   *  @tparam U the result type of `f`, which is discarded
   *  @param f the function applied to each element
   */
  override def foreach[U](f: Int => U): Unit = {
    val s = stepper
    while (s.hasStep) f(s.nextStep())
  }

  /** Returns a new `IntAccumulator` containing the results of applying `f` to each element of
   *  this one, in order.
   *
   *  Unlike the inherited `map`, which builds an [[AnyAccumulator]], this overload keeps the
   *  elements unboxed.
   *
   *  @param f the function applied to each element
   */
  def map(f: Int => Int): IntAccumulator = {
    val b = newSpecificBuilder
    val s = stepper
    while (s.hasStep)
      b.addOne(f(s.nextStep()))
    b.result()
  }

  /** Returns a new `IntAccumulator` containing the elements produced by applying `f` to each
   *  element of this one, concatenated in order.
   *
   *  Unlike the inherited `flatMap`, which builds an [[AnyAccumulator]], this overload keeps the
   *  elements unboxed.
   *
   *  @param f the function mapping each element to a collection of `Int`s
   */
  def flatMap(f: Int => IterableOnce[Int]): IntAccumulator = {
    val b = newSpecificBuilder
    val s = stepper
    while (s.hasStep)
      b.addAll(f(s.nextStep()))
    b.result()
  }

  /** Returns a new `IntAccumulator` containing the results of applying `pf` to the elements of
   *  this one for which it is defined, in order.
   *
   *  Unlike the inherited `collect`, which builds an [[AnyAccumulator]], this overload keeps the
   *  elements unboxed.
   *
   *  @param pf the partial function applied to the elements on which it is defined
   */
  def collect(pf: PartialFunction[Int, Int]): IntAccumulator = {
    val b = newSpecificBuilder
    val s = stepper
    while (s.hasStep) {
      val n = s.nextStep()
      pf.runWith(b.addOne)(n)
    }
    b.result()
  }

  private def filterAccImpl(pred: Int => Boolean, not: Boolean): IntAccumulator = {
    val b = newSpecificBuilder
    val s = stepper
    while (s.hasStep) {
      val n = s.nextStep()
      if (pred(n) != not) b.addOne(n)
    }
    b.result()
  }

  /** Returns a new `IntAccumulator` containing the elements of this one that satisfy `pred`, in
   *  order.
   *
   *  @param pred the predicate each element is tested against
   */
  override def filter(pred: Int => Boolean): IntAccumulator = filterAccImpl(pred, not = false)

  /** Returns a new `IntAccumulator` containing the elements of this one that do not satisfy
   *  `pred`, in order.
   *
   *  @param pred the predicate each element is tested against
   */
  override def filterNot(pred: Int => Boolean): IntAccumulator = filterAccImpl(pred, not = true)

  /** Returns `true` if `p` holds for every element of this `IntAccumulator`, and `true` if this
   *  `IntAccumulator` is empty. Testing stops at the first element for which `p` is `false`.
   *
   *  @param p the predicate each element is tested against
   */
  override def forall(p: Int => Boolean): Boolean = {
    val s = stepper
    while (s.hasStep)
      if (!p(s.nextStep())) return false
    true
  }

  /** Returns `true` if `p` holds for at least one element of this `IntAccumulator`, `false`
   *  otherwise. Testing stops at the first element for which `p` is `true`.
   *
   *  @param p the predicate each element is tested against
   */
  override def exists(p: Int => Boolean): Boolean = {
    val s = stepper
    while (s.hasStep)
      if (p(s.nextStep())) return true
    false
  }

  /** Returns the number of elements of this `IntAccumulator` that satisfy `p`.
   *
   *  @param p the predicate each element is tested against
   *  @return the number of matching elements, as an `Int`, which overflows silently if more than
   *          `Int.MaxValue` elements match; use [[countLong]] for large accumulators
   */
  override def count(p: Int => Boolean): Int = {
    var r = 0
    val s = stepper
    while (s.hasStep)
      if (p(s.nextStep())) r += 1
    r
  }

  /** Returns the number of elements of this `IntAccumulator` that satisfy `p`, as a `Long`, so
   *  that accumulators holding more than `Int.MaxValue` elements are counted correctly.
   *
   *  @param p the predicate each element is tested against
   */
  def countLong(p: Int => Boolean): Long = {
    var r = 0L
    val s = stepper
    while (s.hasStep)
      if (p(s.nextStep())) r += 1
    r
  }

  /** Copies the elements in this `IntAccumulator` into an `Array[Int]`. */
  @nowarn // cat=lint-overload see toArray[B: ClassTag]
  def toArray: Array[Int] = {
    if (totalSize > Int.MaxValue) throw new IllegalArgumentException("Too many elements accumulated for an array: "+totalSize.toString)
    val a = new Array[Int](totalSize.toInt)
    var j = 0
    var h = 0
    var pv = 0L
    while (h < hIndex) {
      val x = history(h)
      val cuml = cumulative(h)
      val n = (cuml - pv).toInt
      pv = cuml
      System.arraycopy(x, 0, a, j, n)
      j += n
      h += 1
    }
    System.arraycopy(current, 0, a, j, index)
    j += index
    a
  }

  /** Copies the elements in this `IntAccumulator` to a `List`. */
  override def toList: List[Int] = {
    var ans: List[Int] = Nil
    var i = index - 1
    while (i >= 0) {
      ans = current(i) :: ans
      i -= 1
    }
    var h = hIndex - 1
    while (h >= 0) {
      val a = history(h)
      i = (cumulative(h) - (if (h == 0) 0L else cumulative(h-1))).toInt - 1
      while (i >= 0) {
        ans = a(i) :: ans
        i -= 1
      }
      h -= 1
    }
    ans
  }

  /** Copies the elements in this `IntAccumulator` to a specified collection.
   *  Note that the target collection is not specialized.
   *  Usage example: `acc.to(Vector)`
   *
   *  @tparam C1 the type of the target collection
   *  @param factory the factory for building the target collection from `Int` elements
   *  @return a collection of type `C1` containing all elements of this `IntAccumulator`
   */
  override def to[C1](factory: Factory[Int, C1]): C1 = {
    if (totalSize > Int.MaxValue) throw new IllegalArgumentException("Too many elements accumulated for a Scala collection: "+totalSize.toString)
    factory.fromSpecific(iterator)
  }

  /** Returns an `IntAccumulator` holding the elements of `coll`; used by operations that
   *  preserve this collection's type to build their result.
   *
   *  @param coll the collection whose elements are accumulated
   *  @return `coll` itself if it already is an `IntAccumulator`, otherwise a new `IntAccumulator`
   *          with all of its elements appended in order
   */
  override protected def fromSpecific(coll: IterableOnce[Int]): IntAccumulator = IntAccumulator.fromSpecific(coll)
  /** Returns a new, empty `IntAccumulator`, which acts as both the builder and its result. */
  override protected def newSpecificBuilder: IntAccumulator = IntAccumulator.newBuilder
  /** Returns the [[AnyAccumulator]] companion object, the factory used to build the results of
   *  inherited operations, such as `map`, that can produce elements of any type.
   */
  override def iterableFactory: SeqFactory[AnyAccumulator] = AnyAccumulator

  /** Returns a new, empty `IntAccumulator`. */
  override def empty: IntAccumulator = IntAccumulator.empty

  private def writeReplace(): AnyRef = new IntAccumulator.SerializationProxy(this)
}

object IntAccumulator extends collection.SpecificIterableFactory[Int, IntAccumulator] {
  private val emptyIntArray = new Array[Int](0)
  private val emptyIntArrayArray = new Array[Array[Int]](0)

  /** Returns the [[IntAccumulator]] companion object as a factory for `java.lang.Integer`
   *  elements, so that collections of boxed `Integer`s can be built into an `IntAccumulator`.
   *
   *  @param ia the `IntAccumulator` companion object being converted (never used)
   */
  implicit def toJavaIntegerAccumulator(ia: IntAccumulator.type): collection.SpecificIterableFactory[jl.Integer, IntAccumulator] = IntAccumulator.asInstanceOf[collection.SpecificIterableFactory[jl.Integer, IntAccumulator]]

  import java.util.{function => jf}

  /** A `Supplier` of `IntAccumulator`s, suitable for use with `java.util.stream.IntStream`'s `collect` method.  Suitable for `Stream[Int]` also. */
  def supplier: jf.Supplier[IntAccumulator]  = () => new IntAccumulator

  /** A `BiConsumer` that adds an element to an `IntAccumulator`, suitable for use with `java.util.stream.IntStream`'s `collect` method. */
  def adder: jf.ObjIntConsumer[IntAccumulator] = (ac: IntAccumulator, a: Int) => ac addOne a

  /** A `BiConsumer` that adds a boxed `Int` to an `IntAccumulator`, suitable for use with `java.util.stream.Stream`'s `collect` method. */
  def boxedAdder: jf.BiConsumer[IntAccumulator, Int] = (ac: IntAccumulator, a: Int) => ac addOne a

  /** A `BiConsumer` that merges `IntAccumulator`s, suitable for use with `java.util.stream.IntStream`'s `collect` method.  Suitable for `Stream[Int]` also. */
  def merger: jf.BiConsumer[IntAccumulator, IntAccumulator] = (a1: IntAccumulator, a2: IntAccumulator) => a1 drain a2

  private def fromArray(a: Array[Int]): IntAccumulator = {
    val r = new IntAccumulator
    var i = 0
    while (i < a.length) { r addOne a(i); i += 1 }
    r
  }

  /** Returns an `IntAccumulator` holding the elements of `it`.
   *
   *  @param it the collection whose elements are accumulated; it may be a one-shot source, such
   *         as an `Iterator`, in which case it is consumed
   *  @return `it` itself if it already is an `IntAccumulator`, otherwise a new `IntAccumulator`
   *          with all of its elements appended in order
   */
  override def fromSpecific(it: IterableOnce[Int]): IntAccumulator = it match {
    case acc: IntAccumulator => acc
    case as: collection.immutable.ArraySeq.ofInt => fromArray(as.unsafeArray)
    case as: collection.mutable.ArraySeq.ofInt => fromArray(as.array) // this case ensures Array(1).to(Accumulator) doesn't box
    case _ => (new IntAccumulator).addAll(it)
  }

  /** Returns a new, empty `IntAccumulator`. */
  override def empty: IntAccumulator = new IntAccumulator

  /** Returns a builder for `IntAccumulator`s, which is itself a new, empty `IntAccumulator`. */
  override def newBuilder: IntAccumulator = new IntAccumulator

  /** A serialization proxy that writes an `IntAccumulator` as its size followed by its elements,
   *  and reads it back into a freshly built accumulator.
   *
   *  @tparam A an unused type parameter; the proxy always serializes `Int` elements
   *  @param acc the accumulator whose elements are written; it is `@transient`, so it is only
   *         available while serializing, not after deserialization
   */
  class SerializationProxy[A](@transient private val acc: IntAccumulator) extends Serializable {
    @transient private var result: IntAccumulator = compiletime.uninitialized

    private def writeObject(out: ObjectOutputStream): Unit = {
      out.defaultWriteObject()
      val size = acc.sizeLong
      out.writeLong(size)
      val st = acc.stepper
      while (st.hasStep)
        out.writeInt(st.nextStep())
    }

    private def readObject(in: ObjectInputStream): Unit = {
      in.defaultReadObject()
      val res = new IntAccumulator()
      var elems = in.readLong()
      while (elems > 0) {
        res += in.readInt()
        elems -= 1L
      }
      result = res
    }

    private def readResolve(): AnyRef = result
  }
}

private[jdk] class IntAccumulatorStepper(private val acc: IntAccumulator) extends IntStepper with EfficientSplit {
  import java.util.Spliterator._

  private var h: Int = 0
  private var i: Int = 0
  private var a: Array[Int] = if (acc.hIndex > 0) acc.history(0) else acc.current
  private var n: Long = if (acc.hIndex > 0) acc.cumulative(0) else acc.index
  private var N: Long = acc.totalSize

  private def duplicateSelf(limit: Long): IntAccumulatorStepper = {
    val ans = new IntAccumulatorStepper(acc)
    ans.h = h
    ans.i = i
    ans.a = a
    ans.n = n
    ans.N = limit
    ans
  }

  private def loadMore(): Unit = {
    h += 1
    if (h < acc.hIndex) { a = acc.history(h); n = acc.cumulative(h) - acc.cumulative(h-1) }
    else { a = acc.current; n = acc.index }
    i = 0
  }

  /** Returns the characteristics of this stepper: `ORDERED`, `SIZED`, `SUBSIZED` and `NONNULL`. */
  def characteristics: Int = ORDERED | SIZED | SUBSIZED | NONNULL

  /** Returns the exact number of elements remaining in this stepper. */
  def estimateSize: Long = N

  /** Returns `true` if at least one element remains in this stepper. */
  def hasStep: Boolean = N > 0

  /** Returns the next element and advances this stepper.
   *
   *  @throws NoSuchElementException if no elements remain
   */
  def nextStep(): Int =
    if (N <= 0) throw new NoSuchElementException("next on empty Stepper")
    else {
      if (i >= n) loadMore()
      val ans = a(i)
      i += 1
      N -= 1
      ans
    }

  /** Splits the remaining elements in half, returning a stepper over the first half and leaving
   *  this stepper positioned on the second half, or `null` if fewer than two elements remain.
   */
  def trySplit(): IntStepper | Null =
    if (N <= 1) null
    else {
      val half = N >> 1
      val M = (if (h <= 0) 0L else acc.cumulative(h-1)) + i
      val R = M + half
      val ans = duplicateSelf(half)
      if (h < acc.hIndex) {
        val w = acc.seekSlot(R)
        h = (w >>> 32).toInt
        if (h < acc.hIndex) {
          a = acc.history(h)
          n = acc.cumulative(h) - (if (h > 0) acc.cumulative(h-1) else 0)
        }
        else {
          a = acc.current
          n = acc.index
        }
        i = (w & 0xFFFFFFFFL).toInt
      }
      else i += half.toInt
      N -= half
      ans
    }

  /** Returns a `java.util.Spliterator.OfInt` over the remaining elements of this stepper, which
   *  advances this stepper as it is consumed.
   *
   *  @tparam B a supertype of `Int` (never used, the result is always an `OfInt`)
   *  @return a `Spliterator.OfInt` whose `tryAdvance` and `forEachRemaining` read the
   *          accumulator's blocks directly, rather than going through `nextStep()`
   */
  override def spliterator[B >: Int]: Spliterator.OfInt = new IntStepper.IntStepperSpliterator(this) {
    // Overridden for efficiency
    override def tryAdvance(c: IntConsumer): Boolean =
      if (N <= 0) false
      else {
        if (i >= n) loadMore()
        c.accept(a(i))
        i += 1
        N -= 1
        true
      }

    // Overridden for efficiency
    override def tryAdvance(c: Consumer[? >: jl.Integer]): Boolean = (c: AnyRef) match {
      case ic: IntConsumer => tryAdvance(ic)
      case _ =>
        if (N <= 0) false
        else {
          if (i >= n) loadMore()
          c.accept(a(i))
          i += 1
          N -= 1
          true
        }
    }

    // Overridden for efficiency
    override def forEachRemaining(c: IntConsumer): Unit =
      while (N > 0) {
        if (i >= n) loadMore()
        val i0 = i
        if ((n-i) > N) n = i + N.toInt
        while (i < n) {
          c.accept(a(i))
          i += 1
        }
        N -= (n - i0)
      }

    // Overridden for efficiency
    override def forEachRemaining(c: Consumer[? >: jl.Integer]): Unit = (c: AnyRef) match {
      case ic: IntConsumer => forEachRemaining(ic)
      case _ =>
        while (N > 0) {
          if (i >= n) loadMore()
          val i0 = i
          if ((n-i) > N) n = i + N.toInt
          while (i < n) {
            c.accept(a(i))
            i += 1
          }
          N -= (n - i0)
        }
    }
  }
}
