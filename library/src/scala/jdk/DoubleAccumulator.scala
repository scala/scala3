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

import scala.language.`2.13`
import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.Spliterator
import java.util.function.{Consumer, DoubleConsumer}
import java.{lang => jl}

import scala.annotation._
import scala.collection.Stepper.EfficientSplit
import scala.collection.{AnyStepper, DoubleStepper, Factory, SeqFactory, Stepper, StepperShape, mutable}
import scala.language.implicitConversions

/** A specialized Accumulator that holds `Double`s without boxing, see [[Accumulator]]. */
final class DoubleAccumulator
  extends Accumulator[Double, AnyAccumulator, DoubleAccumulator]
    with mutable.SeqOps[Double, AnyAccumulator, DoubleAccumulator]
    with Serializable {
  private[jdk] var current: Array[Double] = DoubleAccumulator.emptyDoubleArray
  private[jdk] var history: Array[Array[Double]] = DoubleAccumulator.emptyDoubleArrayArray

  private[jdk] def cumulative(i: Int) = { val x = history(i); x(x.length-1).toLong }

  /** Returns `"DoubleAccumulator"`, the prefix used by `toString`. */
  override protected def className: String = "DoubleAccumulator"

  /** Returns a [[scala.collection.Stepper]] over the elements of this `DoubleAccumulator` that
   *  supports efficient splitting, so that it can be traversed in parallel.
   *
   *  @tparam S the specific stepper type, determined by `shape`
   *  @param shape the implicit shape selecting the stepper specialized for the element type; it
   *         has to select either the `Double` shape or the reference shape
   *  @return a stepper of shape `S`; an unboxed `DoubleStepper` for the `Double` shape, and a
   *          boxing `AnyStepper` wrapped around it for the reference shape
   */
  def efficientStepper[S <: Stepper[?]](implicit shape: StepperShape[Double, S]): S & EfficientSplit = {
    val st = new DoubleAccumulatorStepper(this)
    val r =
      if (shape.shape == StepperShape.DoubleShape) st
      else {
        assert(shape.shape == StepperShape.ReferenceShape, s"unexpected StepperShape: $shape")
        AnyStepper.ofParDoubleStepper(st)
      }
    r.asInstanceOf[S & EfficientSplit]
  }

  private def expand(): Unit = {
    if (index > 0) {
      current(current.length-1) = (if (hIndex > 0) { val x = history(hIndex-1); x(x.length-1) } else 0) + index
      if (hIndex >= history.length) hExpand()
      history(hIndex) = current
      hIndex += 1
    }
    current = new Array[Double](nextBlockSize+1)
    index = 0
  }

  private def hExpand(): Unit = {
    if (hIndex == 0) history = new Array[Array[Double]](4)
    else history = java.util.Arrays.copyOf(history, history.length << 1)
  }

  /** Appends an element to this `DoubleAccumulator`.
   *
   *  @param a the `Double` value to append
   *  @return this `DoubleAccumulator` (to allow chaining of operations)
   */
  def addOne(a: Double): this.type = {
    totalSize += 1
    if (index+1 >= current.length) expand()
    current(index) = a
    index += 1
    this
  }

  /** Result collection consisting of all elements appended so far. */
  override def result(): DoubleAccumulator = this

  /** Removes all elements from `that` and appends them to this `DoubleAccumulator`.
   *
   *  @param that the `DoubleAccumulator` to drain elements from; it will be empty after this operation
   */
  def drain(that: DoubleAccumulator): Unit = {
    var h = 0
    var prev = 0L
    var more = true
    while (more && h < that.hIndex) {
      val cuml = that.cumulative(h)
      val n = (cuml - prev).toInt
      if (current.length - index - 1 >= n) {
        System.arraycopy(that.history(h), 0, current, index, n)
        prev = cuml
        index += n
        h += 1
      }
      else more = false
    }
    if (h >= that.hIndex && current.length - index - 1>= that.index) {
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
            val ans = java.util.Arrays.copyOf(current, index + 1)
            ans(ans.length - 1) = current(current.length - 1)
            ans
          }
          else current
        pv = pv + index
        x(x.length - 1) = pv.toDouble // see comment on Accumulator.cumulative
        history(hIndex) = x
        hIndex += 1
      }
      while (h < that.hIndex) {
        val cuml = that.cumulative(h)
        pv = pv + cuml - prev
        prev = cuml
        val x = that.history(h)
        x(x.length - 1) = pv.toDouble // see comment on Accumulator.cumulative
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

  /** Removes all accumulated elements from this `DoubleAccumulator`, releasing the arrays that held them. */
  override def clear(): Unit = {
    super.clear()
    current = DoubleAccumulator.emptyDoubleArray
    history = DoubleAccumulator.emptyDoubleArrayArray
  }

  /** Retrieves the `ix`th element.
   *
   *  @param ix the zero-based index of the element to retrieve
   *  @return the `Double` value stored at position `ix`
   */
  def apply(ix: Long): Double = {
    if (totalSize - ix <= index || hIndex == 0) current((ix - (totalSize - index)).toInt)
    else {
      val w = seekSlot(ix)
      history((w >>> 32).toInt)((w & 0xFFFFFFFFL).toInt)
    }
  }

  /** Retrieves the `ix`th element, using an `Int` index.
   *
   *  @param i the zero-based index of the element to retrieve (converted to `Long` internally)
   *  @return the `Double` value stored at position `i`
   */
  def apply(i: Int): Double = apply(i.toLong)

  /** Replaces the element at index `idx` with `elem`.
   *
   *  `idx` is not validated, and an out-of-range index has more than one possible outcome. It can
   *  land in unused capacity of the array it is written to, in which case the write silently
   *  succeeds without changing any element this accumulator reports. The offset into that array is
   *  computed as a `Long` and then narrowed to an `Int`, both when it is computed directly for the
   *  current array and when `seekSlot` computes it for a history array, so an index far enough out
   *  of range can also wrap onto an occupied slot and silently overwrite an element this
   *  accumulator does report, or onto a history block's trailing bookkeeping slot, corrupting the
   *  accumulator's internal indexing.
   *
   *  @param idx the zero-based index of the element to replace
   *  @param elem the `Double` value to store at index `idx`
   *  @throws ArrayIndexOutOfBoundsException if the offset computed from `idx` falls outside the
   *          bounds of the array being written
   */
  def update(idx: Long, elem: Double): Unit = {
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
   *  index is widened to a `Long` without loss, so it can never wrap onto an occupied slot the
   *  way a sufficiently large `Long` index can.
   *
   *  @param idx the zero-based index of the element to replace
   *  @param elem the `Double` value to store at index `idx`
   *  @throws ArrayIndexOutOfBoundsException if `idx` is out of range and the computed offset falls
   *          outside the array being written, rather than into unused current-array capacity
   */
  def update(idx: Int, elem: Double): Unit = update(idx.toLong, elem)

  /** Returns an `Iterator` over the contents of this `DoubleAccumulator`. The `Iterator` is not specialized. */
  def iterator: Iterator[Double] = stepper.iterator

  /** Applies `f` to every element of this `DoubleAccumulator`, in order.
   *
   *  @tparam U the result type of `f`, which is discarded
   *  @param f the function applied to each element
   */
  override def foreach[U](f: Double => U): Unit = {
    val s = stepper
    while (s.hasStep) f(s.nextStep())
  }

  /** Returns a new `DoubleAccumulator` holding the results of applying `f` to every element of
   *  this one, in order.
   *
   *  @param f the function applied to each element
   */
  def map(f: Double => Double): DoubleAccumulator = {
    val b = newSpecificBuilder
    val s = stepper
    while (s.hasStep)
      b.addOne(f(s.nextStep()))
    b.result()
  }

  /** Returns a new `DoubleAccumulator` holding the concatenated results of applying `f` to every
   *  element of this one, in order.
   *
   *  @param f the function mapping each element to the elements to append in its place
   */
  def flatMap(f: Double => IterableOnce[Double]): DoubleAccumulator = {
    val b = newSpecificBuilder
    val s = stepper
    while (s.hasStep)
      b.addAll(f(s.nextStep()))
    b.result()
  }

  /** Returns a new `DoubleAccumulator` holding the results of applying `pf` to the elements of
   *  this one for which it is defined, in order.
   *
   *  @param pf the partial function applied to each element; elements outside its domain are skipped
   */
  def collect(pf: PartialFunction[Double, Double]): DoubleAccumulator = {
    val b = newSpecificBuilder
    val s = stepper
    while (s.hasStep) {
      val n = s.nextStep()
      pf.runWith(b.addOne)(n)
    }
    b.result()
  }

  private def filterAccImpl(pred: Double => Boolean, not: Boolean): DoubleAccumulator = {
    val b = newSpecificBuilder
    val s = stepper
    while (s.hasStep) {
      val n = s.nextStep()
      if (pred(n) != not) b.addOne(n)
    }
    b.result()
  }

  /** Returns a new `DoubleAccumulator` holding the elements of this one that satisfy `pred`, in order.
   *
   *  @param pred the predicate each element is tested against
   */
  override def filter(pred: Double => Boolean): DoubleAccumulator = filterAccImpl(pred, not = false)

  /** Returns a new `DoubleAccumulator` holding the elements of this one that do not satisfy `pred`,
   *  in order.
   *
   *  @param pred the predicate each element is tested against
   */
  override def filterNot(pred: Double => Boolean): DoubleAccumulator = filterAccImpl(pred, not = true)

  /** Tests whether `p` holds for every element of this `DoubleAccumulator`.
   *
   *  @param p the predicate each element is tested against
   *  @return `true` if every element satisfies `p`, or if this accumulator is empty; `false` as
   *          soon as an element fails the test, leaving the remaining elements untested
   */
  override def forall(p: Double => Boolean): Boolean = {
    val s = stepper
    while (s.hasStep)
      if (!p(s.nextStep())) return false
    true
  }

  /** Tests whether `p` holds for at least one element of this `DoubleAccumulator`.
   *
   *  @param p the predicate each element is tested against
   *  @return `true` as soon as an element satisfies `p`, leaving the remaining elements untested;
   *          `false` if no element does, and in particular if this accumulator is empty
   */
  override def exists(p: Double => Boolean): Boolean = {
    val s = stepper
    while (s.hasStep)
      if (p(s.nextStep())) return true
    false
  }

  /** Counts the elements of this `DoubleAccumulator` that satisfy a predicate.
   *
   *  @param p the predicate each element is tested against
   *  @return the number of matching elements, as an `Int`, which overflows if more than
   *          `Int.MaxValue` elements match; use [[countLong]] for such accumulators
   */
  override def count(p: Double => Boolean): Int = {
    var r = 0
    val s = stepper
    while (s.hasStep)
      if (p(s.nextStep())) r += 1
    r
  }

  /** Counts the elements of this `DoubleAccumulator` that satisfy a predicate.
   *
   *  @param p the predicate each element is tested against
   *  @return the number of matching elements, as a `Long`, so that accumulators holding more
   *          than `Int.MaxValue` elements are counted correctly
   */
  def countLong(p: Double => Boolean): Long = {
    var r = 0L
    val s = stepper
    while (s.hasStep)
      if (p(s.nextStep())) r += 1
    r
  }

  /** Copies the elements in this `DoubleAccumulator` into an `Array[Double]`. */
  @nowarn // cat=lint-overload see toArray[B: ClassTag]
  def toArray: Array[Double] = {
    if (totalSize > Int.MaxValue) throw new IllegalArgumentException("Too many elements accumulated for an array: "+totalSize.toString)
    val a = new Array[Double](totalSize.toInt)
    var j = 0
    var h = 0
    var pv = 0L
    while (h < hIndex) {
      val x = history(h)
      val cuml = x(x.length-1).toLong
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

  /** Copies the elements in this `DoubleAccumulator` to a `List`. */
  override def toList: List[Double] = {
    var ans: List[Double] = Nil
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

  /** Copies the elements in this `DoubleAccumulator` to a specified collection.
   *  Note that the target collection is not specialized.
   *  Usage example: `acc.to(Vector)`
   *
   *  @tparam C1 the result type of the target collection (e.g., `Vector[Double]`)
   *  @param factory the factory for creating the target collection from elements
   *  @return a new collection of type `C1` containing all elements of this `DoubleAccumulator`
   */
  override def to[C1](factory: Factory[Double, C1]): C1 = {
    if (totalSize > Int.MaxValue) throw new IllegalArgumentException("Too many elements accumulated for a Scala collection: "+totalSize.toString)
    factory.fromSpecific(iterator)
  }

  /** Returns a `DoubleAccumulator` holding the elements of `coll`, used to rebuild this
   *  collection type from the result of a generic operation.
   *
   *  @param coll the elements of the resulting accumulator; a one-shot source, such as an
   *         `Iterator`, is consumed
   *  @return `coll` itself if it already is a `DoubleAccumulator`, otherwise a new
   *          `DoubleAccumulator` with all of its elements appended in order
   */
  override protected def fromSpecific(coll: IterableOnce[Double]): DoubleAccumulator = DoubleAccumulator.fromSpecific(coll)
  /** Returns a new, empty `DoubleAccumulator`, which acts as its own builder and result. */
  override protected def newSpecificBuilder: DoubleAccumulator = DoubleAccumulator.newBuilder
  /** Returns the [[AnyAccumulator]] companion object, the factory used to build the results of
   *  operations that are not specialized for `Double`, such as mapping to another element type.
   */
  override def iterableFactory: SeqFactory[AnyAccumulator] = AnyAccumulator

  /** Returns a new, empty `DoubleAccumulator`. */
  override def empty: DoubleAccumulator = DoubleAccumulator.empty

  private def writeReplace(): AnyRef = new DoubleAccumulator.SerializationProxy(this)
}

object DoubleAccumulator extends collection.SpecificIterableFactory[Double, DoubleAccumulator] {
  private val emptyDoubleArray = new Array[Double](0)
  private val emptyDoubleArrayArray = new Array[Array[Double]](0)

  /** Adapts the [[DoubleAccumulator]] companion object to a factory for boxed `java.lang.Double`
   *  elements, so that it can be used where a factory of a Java-typed collection is expected.
   *
   *  @param ia the `DoubleAccumulator` companion object being converted (never used)
   *  @return the `DoubleAccumulator` companion object itself, cast to a
   *          `SpecificIterableFactory` of `java.lang.Double`; no new factory is created
   */
  implicit def toJavaDoubleAccumulator(ia: DoubleAccumulator.type): collection.SpecificIterableFactory[jl.Double, DoubleAccumulator] = DoubleAccumulator.asInstanceOf[collection.SpecificIterableFactory[jl.Double, DoubleAccumulator]]

  import java.util.{function => jf}

  /** A `Supplier` of `DoubleAccumulator`s, suitable for use with `java.util.stream.DoubleStream`'s `collect` method.  Suitable for `Stream[Double]` also. */
  def supplier: jf.Supplier[DoubleAccumulator]  = () => new DoubleAccumulator

  /** A `BiConsumer` that adds an element to an `DoubleAccumulator`, suitable for use with `java.util.stream.DoubleStream`'s `collect` method. */
  def adder: jf.ObjDoubleConsumer[DoubleAccumulator] = (ac: DoubleAccumulator, a: Double) => ac addOne a

  /** A `BiConsumer` that adds a boxed `Double` to an `DoubleAccumulator`, suitable for use with `java.util.stream.Stream`'s `collect` method. */
  def boxedAdder: jf.BiConsumer[DoubleAccumulator, Double] = (ac: DoubleAccumulator, a: Double) => ac addOne a

  /** A `BiConsumer` that merges `DoubleAccumulator`s, suitable for use with `java.util.stream.DoubleStream`'s `collect` method.  Suitable for `Stream[Double]` also. */
  def merger: jf.BiConsumer[DoubleAccumulator, DoubleAccumulator] = (a1: DoubleAccumulator, a2: DoubleAccumulator) => a1 drain a2

  private def fromArray(a: Array[Double]): DoubleAccumulator = {
    val r = new DoubleAccumulator
    var i = 0
    while (i < a.length) { r addOne a(i); i += 1 }
    r
  }

  /** Returns a `DoubleAccumulator` holding the elements of `it`.
   *
   *  @param it the elements to accumulate; a one-shot source, such as an `Iterator`, is consumed
   *  @return `it` itself if it already is a `DoubleAccumulator`, otherwise a new
   *          `DoubleAccumulator` with all of its elements appended in order; an `ArraySeq` of
   *          `Double` is copied without boxing
   */
  override def fromSpecific(it: IterableOnce[Double]): DoubleAccumulator = it match {
    case acc: DoubleAccumulator => acc
    case as: collection.immutable.ArraySeq.ofDouble => fromArray(as.unsafeArray)
    case as: collection.mutable.ArraySeq.ofDouble => fromArray(as.array) // this case ensures Array(1).to(Accumulator) doesn't box
    case _ => (new DoubleAccumulator).addAll(it)
  }

  /** Returns a new, empty `DoubleAccumulator`. */
  override def empty: DoubleAccumulator = new DoubleAccumulator

  /** Returns a new, empty `DoubleAccumulator`, which acts as its own builder and result. */
  override def newBuilder: DoubleAccumulator = new DoubleAccumulator

  /** A serialization proxy that writes a `DoubleAccumulator` as its size followed by its
   *  elements, and reads it back into a freshly built accumulator.
   *
   *  @tparam A a type parameter that is never used; the element type is always `Double`
   *  @param acc the accumulator whose elements are written; it is `@transient`, so it is only
   *         available while serializing, not after deserialization
   */
  class SerializationProxy[A](@transient private val acc: DoubleAccumulator) extends Serializable {
    @transient private var result: DoubleAccumulator = compiletime.uninitialized

    private def writeObject(out: ObjectOutputStream): Unit = {
      out.defaultWriteObject()
      val size = acc.sizeLong
      out.writeLong(size)
      val st = acc.stepper
      while (st.hasStep)
        out.writeDouble(st.nextStep())
    }

    private def readObject(in: ObjectInputStream): Unit = {
      in.defaultReadObject()
      val res = new DoubleAccumulator()
      var elems = in.readLong()
      while (elems > 0) {
        res += in.readDouble()
        elems -= 1L
      }
      result = res
    }

    private def readResolve(): AnyRef = result
  }
}

private[jdk] class DoubleAccumulatorStepper(private val acc: DoubleAccumulator) extends DoubleStepper with EfficientSplit {
  import java.util.Spliterator._

  private var h: Int = 0
  private var i: Int = 0
  private var a: Array[Double] = if (acc.hIndex > 0) acc.history(0) else acc.current
  private var n: Long = if (acc.hIndex > 0) acc.cumulative(0) else acc.index
  private var N: Long = acc.totalSize

  private def duplicateSelf(limit: Long): DoubleAccumulatorStepper = {
    val ans = new DoubleAccumulatorStepper(acc)
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

  def characteristics: Int = ORDERED | SIZED | SUBSIZED | NONNULL

  def estimateSize: Long = N

  def hasStep: Boolean = N > 0

  def nextStep(): Double =
    if (n <= 0) throw new NoSuchElementException("next on empty Stepper")
    else {
      if (i >= n) loadMore()
      val ans = a(i)
      i += 1
      N -= 1
      ans
    }

  def trySplit(): DoubleStepper | Null =
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

  override def spliterator[B >: Double]: Spliterator.OfDouble = new DoubleStepper.DoubleStepperSpliterator(this) {
    // Overridden for efficiency
    override def tryAdvance(c: DoubleConsumer): Boolean =
      if (N <= 0) false
      else {
        if (i >= n) loadMore()
        c.accept(a(i))
        i += 1
        N -= 1
        true
      }

    // Overridden for efficiency
    override def tryAdvance(c: Consumer[? >: jl.Double]): Boolean = (c: AnyRef) match {
      case ic: DoubleConsumer => tryAdvance(ic)
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
    override def forEachRemaining(c: DoubleConsumer): Unit =
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
    override def forEachRemaining(c: Consumer[? >: jl.Double]): Unit = (c: AnyRef) match {
      case ic: DoubleConsumer => forEachRemaining(ic)
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
