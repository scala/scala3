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

import scala.language.`2.13`
import language.experimental.captureChecking

import java.util.function.{Consumer, DoubleConsumer, IntConsumer, LongConsumer}
import java.util.{PrimitiveIterator, Spliterator, Iterator => JIterator}
import java.{lang => jl}

import scala.collection.Stepper.EfficientSplit

/** Steppers exist to enable creating Java streams over Scala collections, see
 *  [[scala.jdk.StreamConverters]]. Besides that use case, they allow iterating over collections
 *  holding unboxed primitives (e.g., `Array[Int]`) without boxing the elements.
 *
 *  Steppers have an iterator-like interface with methods `hasStep` and `nextStep()`. The difference
 *  to iterators - and the reason `Stepper` is not a subtype of `Iterator` - is that there are
 *  hand-specialized variants of `Stepper` for `Int`, `Long` and `Double` ([[IntStepper]], etc.).
 *  These enable iterating over collections holding unboxed primitives (e.g., Arrays,
 *  [[scala.jdk.Accumulator]]s) without boxing the elements.
 *
 *  The selection of primitive types (`Int`, `Long` and `Double`) matches the hand-specialized
 *  variants of Java Streams ([[java.util.stream.Stream]], [[java.util.stream.IntStream]], etc.)
 *  and the corresponding Java Spliterators ([[java.util.Spliterator]], [[java.util.Spliterator.OfInt]], etc.).
 *
 *  Steppers can be converted to Scala Iterators, Java Iterators and Java Spliterators. Primitive
 *  Steppers are converted to the corresponding primitive Java Iterators and Spliterators.
 *
 *  @tparam A the element type of the Stepper
 */
trait Stepper[@specialized(Double, Int, Long) +A] {
  /** Checks if there's an element available. */
  def hasStep: Boolean

  /** Returns the next element and advance the stepper. */
  def nextStep(): A

  /** Splits this stepper, if applicable. The elements of the current Stepper are split up between
   *  the resulting Stepper and the current stepper.
   *
   *  May return `null`, in which case the current Stepper yields the same elements as before.
   *
   *  See method `trySplit` in [[java.util.Spliterator]].
   *
   *  @return a new `Stepper` containing a portion of the elements, or `null` if this stepper cannot be split
   */
  def trySplit(): Stepper[A]^{this} | Null

  /** Returns an estimate of the number of elements of this Stepper, or [[Long.MaxValue]]. See
   *  method `estimateSize` in [[java.util.Spliterator]].
   */
  def estimateSize: Long

  /** Returns a set of characteristics of this Stepper and its elements. See method
   *  `characteristics` in [[java.util.Spliterator]].
   */
  def characteristics: Int

  /** Returns a [[java.util.Spliterator]] corresponding to this Stepper.
   *
   *  Note that the return type is `Spliterator[_]` instead of `Spliterator[A]` to allow returning
   *  a [[java.util.Spliterator.OfInt]] (which is a `Spliterator[Integer]`) in the subclass [[IntStepper]]
   *  (which is a `Stepper[Int]`).
   *
   *  @tparam B a supertype of the element type `A`
   *  @return a `Spliterator` over the remaining elements of this stepper
   */
  def spliterator[B >: A]: Spliterator[?]^{this}

  /** Returns a Java [[java.util.Iterator]] corresponding to this Stepper.
   *
   *  Note that the return type is `Iterator[_]` instead of `Iterator[A]` to allow returning
   *  a [[java.util.PrimitiveIterator.OfInt]] (which is a `Iterator[Integer]`) in the subclass
   *  [[IntStepper]] (which is a `Stepper[Int]`).
   *
   *  @tparam B a supertype of the element type `A`
   *  @return a Java `Iterator` over the remaining elements of this stepper
   */
  def javaIterator[B >: A]: JIterator[?]^{this}

  /** Returns an [[Iterator]] corresponding to this Stepper. Note that Iterators corresponding to
   *  primitive Steppers box the elements.
   */
  def iterator: Iterator[A]^{this} = new AbstractIterator[A] {
    def hasNext: Boolean = hasStep
    def next(): A = nextStep()
  }
}

object Stepper {
  /** A marker trait that indicates that a `Stepper` can call `trySplit` with at worst O(log N) time
   *  and space complexity, and that the division is likely to be reasonably even. Steppers marked
   *  with `EfficientSplit` can be converted to parallel streams with the `asJavaParStream` method
   *  defined in [[scala.jdk.StreamConverters]].
   */
  trait EfficientSplit

  private[collection] final def throwNSEE(): Nothing = throw new NoSuchElementException("Empty Stepper")

  /* These adapter classes can wrap an AnyStepper of a numeric type into a possibly widened primitive Stepper type.
   * This provides a basis for more efficient stream processing on unboxed values provided that the original source
   * of the data is boxed. In other cases native implementations of the primitive stepper types should be provided
   * (see for example IntArrayStepper and WidenedByteArrayStepper). */

  private[collection] class UnboxingDoubleStepper(st: AnyStepper[Double]^) extends DoubleStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Double = st.nextStep()
    def estimateSize: Long = st.estimateSize
    def characteristics: Int = st.characteristics
    def trySplit(): DoubleStepper^{this} | Null = {
      val s = st.trySplit()
      if (s == null) null else new UnboxingDoubleStepper(s)
    }
  }

  private[collection] class UnboxingIntStepper(st: AnyStepper[Int]^) extends IntStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Int = st.nextStep()
    def estimateSize: Long = st.estimateSize
    def characteristics: Int = st.characteristics
    def trySplit(): IntStepper^{this} | Null = {
      val s = st.trySplit()
      if (s == null) null else new UnboxingIntStepper(s)
    }
  }

  private[collection] class UnboxingLongStepper(st: AnyStepper[Long]^) extends LongStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Long = st.nextStep()
    def estimateSize: Long = st.estimateSize
    def characteristics: Int = st.characteristics
    def trySplit(): LongStepper^{this} | Null = {
      val s = st.trySplit()
      if (s == null) null else new UnboxingLongStepper(s)
    }
  }

  private[collection] class UnboxingByteStepper(st: AnyStepper[Byte]^) extends IntStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Int = st.nextStep()
    def estimateSize: Long = st.estimateSize
    def characteristics: Int = st.characteristics
    def trySplit(): IntStepper^{this} | Null = {
      val s = st.trySplit()
      if (s == null) null else new UnboxingByteStepper(s)
    }
  }

  private[collection] class UnboxingCharStepper(st: AnyStepper[Char]^) extends IntStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Int = st.nextStep()
    def estimateSize: Long = st.estimateSize
    def characteristics: Int = st.characteristics
    def trySplit(): IntStepper^{this} | Null = {
      val s = st.trySplit()
      if (s == null) null else new UnboxingCharStepper(s)
    }
  }

  private[collection] class UnboxingShortStepper(st: AnyStepper[Short]^) extends IntStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Int = st.nextStep()
    def estimateSize: Long = st.estimateSize
    def characteristics: Int = st.characteristics
    def trySplit(): IntStepper^{this} | Null = {
      val s = st.trySplit()
      if (s == null) null else new UnboxingShortStepper(s)
    }
  }

  private[collection] class UnboxingFloatStepper(st: AnyStepper[Float]^) extends DoubleStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Double = st.nextStep()
    def estimateSize: Long = st.estimateSize
    def characteristics: Int = st.characteristics
    def trySplit(): DoubleStepper^{this} | Null = {
      val s = st.trySplit()
      if (s == null) null else new UnboxingFloatStepper(s)
    }
  }
}

/** A `Stepper` for arbitrary element types. See [[Stepper]].
 *
 *  @tparam A the element type of the stepper
 */
trait AnyStepper[+A] extends Stepper[A] {
  /** Splits this stepper, if applicable. See [[Stepper.trySplit]].
   *
   *  @return a new `AnyStepper` containing a portion of the elements, or `null` if this stepper cannot be split
   */
  def trySplit(): AnyStepper[A]^{this} | Null

  /** Returns a [[java.util.Spliterator]] over the remaining elements of this stepper.
   *
   *  @tparam B a supertype of the element type `A`
   *  @return a `Spliterator` whose advancing, splitting, size estimate and characteristics
   *          all delegate to this stepper
   */
  def spliterator[B >: A]: Spliterator[B]^{this} = new AnyStepper.AnyStepperSpliterator(this)

  /** Returns a Java [[java.util.Iterator]] over the remaining elements of this stepper.
   *
   *  @tparam B a supertype of the element type `A`
   *  @return a Java `Iterator` whose `hasNext` and `next()` delegate to `hasStep` and `nextStep()`
   */
  def javaIterator[B >: A]: JIterator[B]^{this} = new JIterator[B] {
    def hasNext: Boolean = hasStep
    def next(): B = nextStep()
  }
}

object AnyStepper {
  /** A [[java.util.Spliterator]] backed by an [[AnyStepper]]; every operation delegates
   *  to the underlying stepper.
   *
   *  @tparam A the element type
   *  @param s the stepper providing the elements
   */
  class AnyStepperSpliterator[A](s: AnyStepper[A]^) extends Spliterator[A] {
    /** If the underlying stepper has another element, passes it to the given action
     *  and advances past it.
     *
     *  @param c the action to perform on the next element
     *  @return `true` if an element existed and `c` was applied to it, `false` otherwise
     */
    def tryAdvance(c: Consumer[? >: A]): Boolean =
      if (s.hasStep) { c.accept(s.nextStep()); true } else false
    /** Splits the underlying stepper and returns a `Spliterator` over the split-off
     *  elements, or `null` if the underlying stepper cannot be split.
     */
    def trySplit(): Spliterator[A]^{this} | Null = {
      val sp = s.trySplit()
      if (sp == null) null else sp.spliterator
    }
    /** Returns the size estimate of the underlying stepper. */
    def estimateSize(): Long = s.estimateSize
    /** Returns the characteristics of the underlying stepper. */
    def characteristics(): Int = s.characteristics
    // Override for efficiency: implement with hasStep / nextStep instead of tryAdvance
    /** Applies the given action to each remaining element of the underlying stepper,
     *  implemented directly with `hasStep` and `nextStep()` for efficiency.
     *
     *  @param c the action to perform on each remaining element
     */
    override def forEachRemaining(c: Consumer[? >: A]): Unit =
      while (s.hasStep) { c.accept(s.nextStep()) }
  }

  /** Returns an `AnyStepper[Double]` that boxes the elements of the given `DoubleStepper`.
   *
   *  @param st the primitive stepper to wrap
   *  @return a stepper yielding the same elements as `st`, boxed
   */
  def ofSeqDoubleStepper(st: DoubleStepper): AnyStepper[Double] = new BoxedDoubleStepper(st)
  /** Returns an `AnyStepper[Double]` that boxes the elements of the given `DoubleStepper`,
   *  retaining the [[Stepper.EfficientSplit]] marker.
   *
   *  @param st the primitive stepper to wrap, supporting efficient splitting
   *  @return a stepper yielding the same elements as `st`, boxed, with `EfficientSplit`
   */
  def ofParDoubleStepper(st: DoubleStepper & EfficientSplit): AnyStepper[Double] & EfficientSplit = new BoxedDoubleStepper(st) with EfficientSplit

  /** Returns an `AnyStepper[Int]` that boxes the elements of the given `IntStepper`.
   *
   *  @param st the primitive stepper to wrap
   *  @return a stepper yielding the same elements as `st`, boxed
   */
  def ofSeqIntStepper(st: IntStepper): AnyStepper[Int] = new BoxedIntStepper(st)
  /** Returns an `AnyStepper[Int]` that boxes the elements of the given `IntStepper`,
   *  retaining the [[Stepper.EfficientSplit]] marker.
   *
   *  @param st the primitive stepper to wrap, supporting efficient splitting
   *  @return a stepper yielding the same elements as `st`, boxed, with `EfficientSplit`
   */
  def ofParIntStepper(st: IntStepper & EfficientSplit): AnyStepper[Int] & EfficientSplit = new BoxedIntStepper(st) with EfficientSplit

  /** Returns an `AnyStepper[Long]` that boxes the elements of the given `LongStepper`.
   *
   *  @param st the primitive stepper to wrap
   *  @return a stepper yielding the same elements as `st`, boxed
   */
  def ofSeqLongStepper(st: LongStepper): AnyStepper[Long] = new BoxedLongStepper(st)
  /** Returns an `AnyStepper[Long]` that boxes the elements of the given `LongStepper`,
   *  retaining the [[Stepper.EfficientSplit]] marker.
   *
   *  @param st the primitive stepper to wrap, supporting efficient splitting
   *  @return a stepper yielding the same elements as `st`, boxed, with `EfficientSplit`
   */
  def ofParLongStepper(st: LongStepper & EfficientSplit): AnyStepper[Long] & EfficientSplit = new BoxedLongStepper(st) with EfficientSplit

  private[collection] class BoxedDoubleStepper(st: DoubleStepper) extends AnyStepper[Double] {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Double = st.nextStep()
    def estimateSize: Long = st.estimateSize
    def characteristics: Int = st.characteristics
    def trySplit(): AnyStepper[Double] | Null = {
      val s = st.trySplit()
      if (s == null) null else new BoxedDoubleStepper(s)
    }
  }

  private[collection] class BoxedIntStepper(st: IntStepper) extends AnyStepper[Int] {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Int = st.nextStep()
    def estimateSize: Long = st.estimateSize
    def characteristics: Int = st.characteristics
    def trySplit(): AnyStepper[Int] | Null = {
      val s = st.trySplit()
      if (s == null) null else new BoxedIntStepper(s)
    }
  }

  private[collection] class BoxedLongStepper(st: LongStepper) extends AnyStepper[Long] {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Long = st.nextStep()
    def estimateSize: Long = st.estimateSize
    def characteristics: Int = st.characteristics
    def trySplit(): AnyStepper[Long] | Null = {
      val s = st.trySplit()
      if (s == null) null else new BoxedLongStepper(s)
    }
  }
}

/** A `Stepper` for `Int`s. See [[Stepper]]. */
trait IntStepper extends Stepper[Int] {
  /** Splits this stepper, if applicable. See [[Stepper.trySplit]].
   *
   *  @return a new `IntStepper` containing a portion of the elements, or `null` if this stepper cannot be split
   */
  def trySplit(): IntStepper^{this} | Null

  /** Returns a primitive [[java.util.Spliterator.OfInt]] over the remaining elements of
   *  this stepper, which yields the elements without boxing.
   *
   *  @tparam B a supertype of `Int`; never used, the result is always a `Spliterator.OfInt`
   *  @return a `Spliterator.OfInt` whose operations delegate to this stepper
   */
  def spliterator[B >: Int]: Spliterator.OfInt^{this} = new IntStepper.IntStepperSpliterator(this)

  /** Returns a primitive [[java.util.PrimitiveIterator.OfInt]] over the remaining elements
   *  of this stepper, whose `nextInt()` yields the elements without boxing.
   *
   *  @tparam B a supertype of `Int`; never used, the result is always a `PrimitiveIterator.OfInt`
   *  @return a `PrimitiveIterator.OfInt` whose `hasNext` and `nextInt()` delegate to `hasStep` and `nextStep()`
   */
  def javaIterator[B >: Int]: PrimitiveIterator.OfInt^{this} = new PrimitiveIterator.OfInt {
    def hasNext: Boolean = hasStep
    def nextInt(): Int = nextStep()
  }
}
object IntStepper {
  /** A primitive [[java.util.Spliterator.OfInt]] backed by an [[IntStepper]]; every
   *  operation delegates to the underlying stepper.
   *
   *  @param s the stepper providing the elements
   */
  class IntStepperSpliterator(s: IntStepper^) extends Spliterator.OfInt {
    /** If the underlying stepper has another element, passes it to the given action
     *  without boxing and advances past it.
     *
     *  @param c the action to perform on the next element
     *  @return `true` if an element existed and `c` was applied to it, `false` otherwise
     */
    def tryAdvance(c: IntConsumer): Boolean =
      if (s.hasStep) { c.accept(s.nextStep()); true } else false
    // Override for efficiency: don't wrap the function and call the `tryAdvance` overload
    /** If the underlying stepper has another element, passes it to the given action
     *  and advances past it. If `c` is an `IntConsumer`, the element is passed without
     *  boxing; otherwise it is boxed to `java.lang.Integer`.
     *
     *  @param c the action to perform on the next element
     *  @return `true` if an element existed and `c` was applied to it, `false` otherwise
     */
    override def tryAdvance(c: Consumer[? >: jl.Integer]): Boolean = (c: AnyRef) match {
      case ic: IntConsumer => tryAdvance(ic)
      case _ => if (s.hasStep) { c.accept(jl.Integer.valueOf(s.nextStep())); true } else false
    }
    // override required for dotty#6152
    /** Splits the underlying stepper and returns a `Spliterator.OfInt` over the split-off
     *  elements, or `null` if the underlying stepper cannot be split.
     */
    override def trySplit(): Spliterator.OfInt^{this} | Null = {
      val sp = s.trySplit()
      if (sp == null) null else sp.spliterator
    }
    /** Returns the size estimate of the underlying stepper. */
    def estimateSize(): Long = s.estimateSize
    /** Returns the characteristics of the underlying stepper. */
    def characteristics(): Int = s.characteristics
    // Override for efficiency: implement with hasStep / nextStep instead of tryAdvance
    /** Applies the given action to each remaining element of the underlying stepper
     *  without boxing, implemented directly with `hasStep` and `nextStep()` for efficiency.
     *
     *  @param c the action to perform on each remaining element
     */
    override def forEachRemaining(c: IntConsumer): Unit =
      while (s.hasStep) { c.accept(s.nextStep()) }
    // Override for efficiency: implement with hasStep / nextStep instead of tryAdvance
    /** Applies the given action to each remaining element of the underlying stepper.
     *  If `c` is an `IntConsumer`, the elements are passed without boxing; otherwise
     *  each element is boxed to `java.lang.Integer`.
     *
     *  @param c the action to perform on each remaining element
     */
    override def forEachRemaining(c: Consumer[? >: jl.Integer]): Unit = (c: AnyRef) match {
      case ic: IntConsumer => forEachRemaining(ic)
      case _ => while (s.hasStep) { c.accept(jl.Integer.valueOf(s.nextStep())) }
    }
  }
}

/** A `Stepper` for `Double`s. See [[Stepper]]. */
trait DoubleStepper extends Stepper[Double] {
  /** Splits this stepper, if applicable. See [[Stepper.trySplit]].
   *
   *  @return a new `DoubleStepper` containing a portion of the elements, or `null` if this stepper cannot be split
   */
  def trySplit(): DoubleStepper^{this} | Null

  /** Returns a primitive [[java.util.Spliterator.OfDouble]] over the remaining elements of
   *  this stepper, which yields the elements without boxing.
   *
   *  @tparam B a supertype of `Double`; never used, the result is always a `Spliterator.OfDouble`
   *  @return a `Spliterator.OfDouble` whose operations delegate to this stepper
   */
  def spliterator[B >: Double]: Spliterator.OfDouble^{this} = new DoubleStepper.DoubleStepperSpliterator(this)

  /** Returns a primitive [[java.util.PrimitiveIterator.OfDouble]] over the remaining elements
   *  of this stepper, whose `nextDouble()` yields the elements without boxing.
   *
   *  @tparam B a supertype of `Double`; never used, the result is always a `PrimitiveIterator.OfDouble`
   *  @return a `PrimitiveIterator.OfDouble` whose `hasNext` and `nextDouble()` delegate to `hasStep` and `nextStep()`
   */
  def javaIterator[B >: Double]: PrimitiveIterator.OfDouble^{this} = new PrimitiveIterator.OfDouble {
    def hasNext: Boolean = hasStep
    def nextDouble(): Double = nextStep()
  }
}

object DoubleStepper {
  /** A primitive [[java.util.Spliterator.OfDouble]] backed by a [[DoubleStepper]]; every
   *  operation delegates to the underlying stepper.
   *
   *  @param s the stepper providing the elements
   */
  class DoubleStepperSpliterator(s: DoubleStepper^) extends Spliterator.OfDouble {
    /** If the underlying stepper has another element, passes it to the given action
     *  without boxing and advances past it.
     *
     *  @param c the action to perform on the next element
     *  @return `true` if an element existed and `c` was applied to it, `false` otherwise
     */
    def tryAdvance(c: DoubleConsumer): Boolean =
      if (s.hasStep) { c.accept(s.nextStep()); true } else false
    // Override for efficiency: don't wrap the function and call the `tryAdvance` overload
    /** If the underlying stepper has another element, passes it to the given action
     *  and advances past it. If `c` is a `DoubleConsumer`, the element is passed without
     *  boxing; otherwise it is boxed to `java.lang.Double`.
     *
     *  @param c the action to perform on the next element
     *  @return `true` if an element existed and `c` was applied to it, `false` otherwise
     */
    override def tryAdvance(c: Consumer[? >: jl.Double]): Boolean = (c: AnyRef) match {
      case ic: DoubleConsumer => tryAdvance(ic)
      case _ => if (s.hasStep) { c.accept(java.lang.Double.valueOf(s.nextStep())); true } else false
    }
    // override required for dotty#6152
    /** Splits the underlying stepper and returns a `Spliterator.OfDouble` over the split-off
     *  elements, or `null` if the underlying stepper cannot be split.
     */
    override def trySplit(): Spliterator.OfDouble^{this} | Null = {
      val sp = s.trySplit()
      if (sp == null) null else sp.spliterator
    }
    /** Returns the size estimate of the underlying stepper. */
    def estimateSize(): Long = s.estimateSize
    /** Returns the characteristics of the underlying stepper. */
    def characteristics(): Int = s.characteristics
    // Override for efficiency: implement with hasStep / nextStep instead of tryAdvance
    /** Applies the given action to each remaining element of the underlying stepper
     *  without boxing, implemented directly with `hasStep` and `nextStep()` for efficiency.
     *
     *  @param c the action to perform on each remaining element
     */
    override def forEachRemaining(c: DoubleConsumer): Unit =
      while (s.hasStep) { c.accept(s.nextStep()) }
    // Override for efficiency: implement with hasStep / nextStep instead of tryAdvance
    /** Applies the given action to each remaining element of the underlying stepper.
     *  If `c` is a `DoubleConsumer`, the elements are passed without boxing; otherwise
     *  each element is boxed to `java.lang.Double`.
     *
     *  @param c the action to perform on each remaining element
     */
    override def forEachRemaining(c: Consumer[? >: jl.Double]): Unit = (c: AnyRef) match {
      case ic: DoubleConsumer => forEachRemaining(ic)
      case _ => while (s.hasStep) { c.accept(jl.Double.valueOf(s.nextStep())) }
    }
  }
}

/** A `Stepper` for `Long`s. See [[Stepper]]. */
trait LongStepper extends Stepper[Long] {
  /** Splits this stepper, if applicable. See [[Stepper.trySplit]].
   *
   *  @return a new `LongStepper` containing a portion of the elements, or `null` if this stepper cannot be split
   */
  def trySplit(): LongStepper^{this} | Null

  /** Returns a primitive [[java.util.Spliterator.OfLong]] over the remaining elements of
   *  this stepper, which yields the elements without boxing.
   *
   *  @tparam B a supertype of `Long`; never used, the result is always a `Spliterator.OfLong`
   *  @return a `Spliterator.OfLong` whose operations delegate to this stepper
   */
  def spliterator[B >: Long]: Spliterator.OfLong^{this} = new LongStepper.LongStepperSpliterator(this)

  /** Returns a primitive [[java.util.PrimitiveIterator.OfLong]] over the remaining elements
   *  of this stepper, whose `nextLong()` yields the elements without boxing.
   *
   *  @tparam B a supertype of `Long`; never used, the result is always a `PrimitiveIterator.OfLong`
   *  @return a `PrimitiveIterator.OfLong` whose `hasNext` and `nextLong()` delegate to `hasStep` and `nextStep()`
   */
  def javaIterator[B >: Long]: PrimitiveIterator.OfLong^{this} = new PrimitiveIterator.OfLong {
    def hasNext: Boolean = hasStep
    def nextLong(): Long = nextStep()
  }
}

object LongStepper {
  /** A primitive [[java.util.Spliterator.OfLong]] backed by a [[LongStepper]]; every
   *  operation delegates to the underlying stepper.
   *
   *  @param s the stepper providing the elements
   */
  class LongStepperSpliterator(s: LongStepper^) extends Spliterator.OfLong {
    /** If the underlying stepper has another element, passes it to the given action
     *  without boxing and advances past it.
     *
     *  @param c the action to perform on the next element
     *  @return `true` if an element existed and `c` was applied to it, `false` otherwise
     */
    def tryAdvance(c: LongConsumer): Boolean =
      if (s.hasStep) { c.accept(s.nextStep()); true } else false
    // Override for efficiency: don't wrap the function and call the `tryAdvance` overload
    /** If the underlying stepper has another element, passes it to the given action
     *  and advances past it. If `c` is a `LongConsumer`, the element is passed without
     *  boxing; otherwise it is boxed to `java.lang.Long`.
     *
     *  @param c the action to perform on the next element
     *  @return `true` if an element existed and `c` was applied to it, `false` otherwise
     */
    override def tryAdvance(c: Consumer[? >: jl.Long]): Boolean = (c: AnyRef) match {
      case ic: LongConsumer => tryAdvance(ic)
      case _ => if (s.hasStep) { c.accept(java.lang.Long.valueOf(s.nextStep())); true } else false
    }
    // override required for dotty#6152
    /** Splits the underlying stepper and returns a `Spliterator.OfLong` over the split-off
     *  elements, or `null` if the underlying stepper cannot be split.
     */
    override def trySplit(): Spliterator.OfLong^{this} | Null = {
      val sp = s.trySplit()
      if (sp == null) null else sp.spliterator
    }
    /** Returns the size estimate of the underlying stepper. */
    def estimateSize(): Long = s.estimateSize
    /** Returns the characteristics of the underlying stepper. */
    def characteristics(): Int = s.characteristics
    // Override for efficiency: implement with hasStep / nextStep instead of tryAdvance
    /** Applies the given action to each remaining element of the underlying stepper
     *  without boxing, implemented directly with `hasStep` and `nextStep()` for efficiency.
     *
     *  @param c the action to perform on each remaining element
     */
    override def forEachRemaining(c: LongConsumer): Unit =
      while (s.hasStep) { c.accept(s.nextStep()) }
    // Override for efficiency: implement with hasStep / nextStep instead of tryAdvance
    /** Applies the given action to each remaining element of the underlying stepper.
     *  If `c` is a `LongConsumer`, the elements are passed without boxing; otherwise
     *  each element is boxed to `java.lang.Long`.
     *
     *  @param c the action to perform on each remaining element
     */
    override def forEachRemaining(c: Consumer[? >: jl.Long]): Unit = (c: AnyRef) match {
      case ic: LongConsumer => forEachRemaining(ic)
      case _ => while (s.hasStep) { c.accept(jl.Long.valueOf(s.nextStep())) }
    }
  }
}
