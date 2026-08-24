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

package scala.collection.convert

import scala.language.`2.13`
import language.experimental.captureChecking

import java.util.Spliterator
import java.util.stream._
import java.{lang => jl}

import scala.annotation.implicitNotFound
import scala.collection.Stepper.EfficientSplit
import scala.collection._
import scala.collection.convert.StreamExtensions.{AccumulatorFactoryInfo, StreamShape, StreamUnboxer}
import scala.jdk.CollectionConverters._
import scala.jdk._

/** Defines extension methods to create Java Streams for Scala collections, available through
 *  [[scala.jdk.javaapi.StreamConverters]].
 */
trait StreamExtensions {
  this: StreamExtensions =>
  // collections

  /** Provides the `asJavaSeqStream` extension method that creates a sequential Java Stream over
   *  a Scala collection, via the collection's [[Stepper]].
   *
   *  @tparam A the element type of the collection
   *  @param cc the collection to create a Stream for
   */
  implicit class IterableHasSeqStream[A](cc: IterableOnce[A]) {
    /** Creates a sequential [[java.util.stream.Stream Java Stream]] for this collection. If the
     *  collection contains primitive values, a corresponding specialized Stream is returned (e.g.,
     *  [[java.util.stream.IntStream `IntStream`]]).
     *
     *  @tparam S the type of Java Stream to create, determined by the element type `A` via the implicit `StreamShape`
     *  @tparam St the type of `Stepper` used to traverse the collection's elements
     *  @param s implicit evidence connecting element type `A` to the appropriate Java Stream and Stepper types
     *  @param st implicit evidence selecting the appropriate `Stepper` shape for element type `A`
     *  @return a sequential Java Stream of type `S` containing the elements of this collection
     */
    def asJavaSeqStream[S <: BaseStream[?, ?], St <: Stepper[?]](implicit s: StreamShape[A, S, St], st: StepperShape[A, St]): S =
      s.fromStepper(cc.stepper, par = false)
  }

  // Not `CC[X] <: IterableOnce[X]`, but `C` with an extra constraint, to support non-parametric classes like IntAccumulator
  /** Provides the `asJavaParStream` extension method that creates a parallel Java Stream over a
   *  Scala collection, via the collection's [[Stepper]].
   *
   *  The receiver is typed as `C` with a separate `ev` constraint, rather than as
   *  `CC[X] <: IterableOnce[X]`, so that non-parametric collections such as
   *  [[scala.jdk.IntAccumulator]] are also supported.
   *
   *  @tparam A the element type of the collection
   *  @tparam C the type of the collection
   *  @param c the collection to create a Stream for
   *  @param ev evidence that the collection type `C` has elements of type `A`
   */
  implicit class IterableNonGenericHasParStream[A, C <: IterableOnce[?]](c: C)(implicit ev: C <:< IterableOnce[A]) {
    private type IterableOnceWithEfficientStepper = IterableOnce[A] {
      /** Returns a `Stepper` for this collection whose type records that it supports efficient
       *  splitting ([[Stepper.EfficientSplit]]), as required for parallel streams.
       *
       *  @tparam S the type of the returned `Stepper`, determined by the element type `A`
       *  @param shape implicit evidence selecting the appropriate `Stepper` type for element type `A`
       *  @return a `Stepper` over this collection's elements that supports efficient splitting
       */
      def stepper[S <: Stepper[?]](implicit shape : StepperShape[A, S]) : S & EfficientSplit
    }

    /** Creates a parallel [[java.util.stream.Stream Java Stream]] for this collection. If the
     *  collection contains primitive values, a corresponding specialized Stream is returned (e.g.,
     *  [[java.util.stream.IntStream `IntStream`]]).
     *
     *  @tparam S the type of Java Stream to create, determined by the element type `A`
     *  @tparam St the type of `Stepper` used to traverse the collection's elements, required to support efficient splitting
     */
    def asJavaParStream[S <: BaseStream[?, ?], St <: Stepper[?]](implicit
        s: StreamShape[A, S, St],
        st: StepperShape[A, St],
        @implicitNotFound("`parStream` can only be called on collections where `stepper` returns a `Stepper with EfficientSplit`")
        isEfficient: C <:< IterableOnceWithEfficientStepper): S =
      s.fromStepper(ev(c).stepper, par = true)
  }

  // maps

  /** Provides the `asJavaSeqKeyStream`, `asJavaSeqValueStream` and `asJavaSeqStream` extension
   *  methods that create sequential Java Streams over the keys, the values, or the
   *  `(key, value)` pairs of a Scala map, via the map's [[Stepper]]s.
   *
   *  @tparam K the key type of the map
   *  @tparam V the value type of the map
   *  @tparam CC the type of the map
   *  @param cc the map to create Streams for
   */
  implicit class MapHasSeqKeyValueStream[K, V, CC[X, Y] <: collection.MapOps[X, Y, collection.Map, ?]](cc: CC[K, V]) {
    /** Creates a sequential [[java.util.stream.Stream Java Stream]] for the keys of this map. If
     *  the keys are primitive values, a corresponding specialized Stream is returned (e.g.,
     *  [[java.util.stream.IntStream `IntStream`]]).
     *
     *  @tparam S the type of Java Stream to create, determined by the key type `K`
     *  @tparam St the type of `Stepper` used to traverse the map's keys
     *  @param s implicit evidence connecting key type `K` to the appropriate Java Stream and Stepper types
     *  @param st implicit evidence selecting the appropriate `Stepper` shape for key type `K`
     *  @return a sequential Java Stream of type `S` containing the keys of this map
     */
    def asJavaSeqKeyStream[S <: BaseStream[?, ?], St <: Stepper[?]](implicit s: StreamShape[K, S, St], st: StepperShape[K, St]): S =
      s.fromStepper(cc.keyStepper, par = false)

    /** Creates a sequential [[java.util.stream.Stream Java Stream]] for the values of this map. If
     *  the values are primitives, a corresponding specialized Stream is returned (e.g.,
     *  [[java.util.stream.IntStream `IntStream`]]).
     *
     *  @tparam S the type of Java Stream to create, determined by the value type `V`
     *  @tparam St the type of `Stepper` used to traverse the map's values
     *  @param s implicit evidence connecting value type `V` to the appropriate Java Stream and Stepper types
     *  @param st implicit evidence selecting the appropriate `Stepper` shape for value type `V`
     *  @return a sequential Java Stream of type `S` containing the values of this map
     */
    def asJavaSeqValueStream[S <: BaseStream[?, ?], St <: Stepper[?]](implicit s: StreamShape[V, S, St], st: StepperShape[V, St]): S =
      s.fromStepper(cc.valueStepper, par = false)

    // The asJavaSeqStream extension method for IterableOnce doesn't apply because its `CC` takes a single type parameter, whereas the one here takes two
    /** Creates a sequential [[java.util.stream.Stream Java Stream]] for the `(key, value)` pairs of
     *  this map.
     *
     *  @tparam S the type of Java Stream to create, determined by the pair type `(K, V)`
     *  @tparam St the type of `Stepper` used to traverse the map's key-value pairs
     *  @param s implicit evidence connecting pair type `(K, V)` to the appropriate Java Stream and Stepper types
     *  @param st implicit evidence selecting the appropriate `Stepper` shape for pair type `(K, V)`
     *  @return a sequential Java Stream of type `S` containing the `(key, value)` pairs of this map
     */
    def asJavaSeqStream[S <: BaseStream[?, ?], St <: Stepper[?]](implicit s: StreamShape[(K, V), S, St], st: StepperShape[(K, V), St]): S =
      s.fromStepper(cc.stepper, par = false)
  }


  /** Provides the `asJavaParKeyStream`, `asJavaParValueStream` and `asJavaParStream` extension
   *  methods that create parallel Java Streams over the keys, the values, or the `(key, value)`
   *  pairs of a Scala map. Each method requires evidence that the corresponding [[Stepper]] of
   *  the map supports efficient splitting ([[Stepper.EfficientSplit]]).
   *
   *  @tparam K the key type of the map
   *  @tparam V the value type of the map
   *  @tparam CC the type of the map
   *  @param cc the map to create Streams for
   */
  implicit class MapHasParKeyValueStream[K, V, CC[X, Y] <: collection.MapOps[X, Y, collection.Map, ?]](cc: CC[K, V]) {
    private type MapOpsWithEfficientKeyStepper = collection.MapOps[K, V, collection.Map, ?] { def keyStepper[S <: Stepper[?]](implicit shape : StepperShape[K, S]) : S & EfficientSplit }
    private type MapOpsWithEfficientValueStepper = collection.MapOps[K, V, collection.Map, ?] { def valueStepper[S <: Stepper[?]](implicit shape : StepperShape[V, S]) : S & EfficientSplit }
    private type MapOpsWithEfficientStepper = collection.MapOps[K, V, collection.Map, ?] { def stepper[S <: Stepper[?]](implicit shape : StepperShape[(K, V), S]) : S & EfficientSplit }

    /** Creates a parallel [[java.util.stream.Stream Java Stream]] for the keys of this map. If
     *  the keys are primitive values, a corresponding specialized Stream is returned (e.g.,
     *  [[java.util.stream.IntStream `IntStream`]]).
     *
     *  @tparam S the type of Java Stream to create, determined by the key type `K`
     *  @tparam St the type of `Stepper` used to traverse the map's keys, required to support efficient splitting
     */
    def asJavaParKeyStream[S <: BaseStream[?, ?], St <: Stepper[?]](implicit
        s: StreamShape[K, S, St],
        st: StepperShape[K, St],
        @implicitNotFound("parKeyStream can only be called on maps where `keyStepper` returns a `Stepper with EfficientSplit`")
        isEfficient: CC[K, V] <:< MapOpsWithEfficientKeyStepper): S =
      s.fromStepper(cc.keyStepper, par = true)

    /** Creates a parallel [[java.util.stream.Stream Java Stream]] for the values of this map. If
     *  the values are primitives, a corresponding specialized Stream is returned (e.g.,
     *  [[java.util.stream.IntStream `IntStream`]]).
     *
     *  @tparam S the type of Java Stream to create, determined by the value type `V`
     *  @tparam St the type of `Stepper` used to traverse the map's values, required to support efficient splitting
     */
    def asJavaParValueStream[S <: BaseStream[?, ?], St <: Stepper[?]](implicit
        s: StreamShape[V, S, St],
        st: StepperShape[V, St],
        @implicitNotFound("parValueStream can only be called on maps where `valueStepper` returns a `Stepper with EfficientSplit`")
        isEfficient: CC[K, V] <:< MapOpsWithEfficientValueStepper): S =
      s.fromStepper(cc.valueStepper, par = true)

    // The asJavaParStream extension method for IterableOnce doesn't apply because its `CC` takes a single type parameter, whereas the one here takes two
    /** Creates a parallel [[java.util.stream.Stream Java Stream]] for the `(key, value)` pairs of
     *  this map.
     *
     *  @tparam S the type of Java Stream to create, determined by the pair type `(K, V)`
     *  @tparam St the type of `Stepper` used to traverse the map's key-value pairs, required to support efficient splitting
     */
    def asJavaParStream[S <: BaseStream[?, ?], St <: Stepper[?]](implicit
        s: StreamShape[(K, V), S, St],
        st: StepperShape[(K, V), St],
        @implicitNotFound("parStream can only be called on maps where `stepper` returns a `Stepper with EfficientSplit`")
        isEfficient: CC[K, V] <:< MapOpsWithEfficientStepper): S =
      s.fromStepper(cc.stepper, par = true)
  }

  // steppers

  /** Provides the `asJavaSeqStream` extension method that creates a sequential Java Stream over
   *  the elements of a [[Stepper]].
   *
   *  @tparam A the element type of the stepper
   *  @param stepper the stepper to create a Stream for
   */
  implicit class StepperHasSeqStream[A](stepper: Stepper[A]) {
    /** Creates a sequential [[java.util.stream.Stream Java Stream]] for this stepper. If the
     *  stepper yields primitive values, a corresponding specialized Stream is returned (e.g.,
     *  [[java.util.stream.IntStream `IntStream`]]).
     *
     *  @tparam S the type of Java Stream to create, determined by the element type `A`
     *  @tparam St the type of `Stepper` used to traverse the elements
     *  @param s implicit evidence connecting element type `A` to the appropriate Java Stream and Stepper types
     *  @param st implicit evidence selecting the appropriate `Stepper` shape for element type `A`; also used to unbox an `AnyStepper` to a specialized stepper when needed
     *  @return a sequential Java Stream of type `S` containing the elements yielded by this stepper
     */
    def asJavaSeqStream[S <: BaseStream[?, ?], St <: Stepper[?]](implicit s: StreamShape[A, S, St], st: StepperShape[A, St]): S = {
      val sStepper = stepper match {
        case as: AnyStepper[A @unchecked] => st.seqUnbox(as)
        case _ => stepper.asInstanceOf[St]
      }
      s.fromStepper(sStepper, par = false)
    }
  }

  /** Provides the `asJavaParStream` extension method that creates a parallel Java Stream over
   *  the elements of a [[Stepper]] that supports efficient splitting
   *  ([[Stepper.EfficientSplit]]).
   *
   *  @tparam A the element type of the stepper
   *  @param stepper the stepper to create a Stream for
   */
  implicit class StepperHasParStream[A](stepper: Stepper[A] & EfficientSplit) {
    /** Creates a parallel [[java.util.stream.Stream Java Stream]] for this stepper. If the
     *  stepper yields primitive values, a corresponding specialized Stream is returned (e.g.,
     *  [[java.util.stream.IntStream `IntStream`]]).
     *
     *  @tparam S the type of Java Stream to create, determined by the element type `A`
     *  @tparam St the type of `Stepper` used to traverse the elements
     *  @param s implicit evidence connecting element type `A` to the appropriate Java Stream and Stepper types
     *  @param st implicit evidence selecting the appropriate `Stepper` shape for element type `A`; also used to unbox an `AnyStepper with EfficientSplit` to a specialized stepper when needed
     *  @return a parallel Java Stream of type `S` containing the elements yielded by this stepper
     */
    def asJavaParStream[S <: BaseStream[?, ?], St <: Stepper[?]](implicit s: StreamShape[A, S, St], st: StepperShape[A, St]): S = {
      val sStepper = (stepper: @unchecked) match {
        case as: (AnyStepper[A] & EfficientSplit) => st.parUnbox(as)
        case _ => stepper.asInstanceOf[St]
      }
      s.fromStepper(sStepper, par = true)
    }
  }

  // arrays
  // uses the JDK array spliterators (`DoubleArraySpliterator`). users can also call
  // `array.stepper.seqStream`, which then uses the Scala steppers (`DoubleArrayStepper`). the
  // steppers are also available on byte/short/char/float arrays (`WidenedByteArrayStepper`),
  // JDK spliterators only for double/int/long/reference.

  /** Provides `asJavaSeqStream` and `asJavaParStream` extension methods that create sequential
   *  or parallel [[java.util.stream.DoubleStream DoubleStream]]s over a `Double` array.
   *
   *  @param a the array to create Streams for
   */
  implicit class DoubleArrayHasSeqParStream(a: Array[Double]) {
    /** Creates a sequential [[java.util.stream.DoubleStream Java DoubleStream]] for this array. */
    def asJavaSeqStream: DoubleStream = java.util.Arrays.stream(a)
    /** Creates a parallel [[java.util.stream.DoubleStream Java DoubleStream]] for this array. */
    def asJavaParStream: DoubleStream = asJavaSeqStream.parallel
  }

  /** Provides `asJavaSeqStream` and `asJavaParStream` extension methods that create sequential
   *  or parallel [[java.util.stream.IntStream IntStream]]s over an `Int` array.
   *
   *  @param a the array to create Streams for
   */
  implicit class IntArrayHasSeqParStream(a: Array[Int]) {
    /** Creates a sequential [[java.util.stream.IntStream Java IntStream]] for this array. */
    def asJavaSeqStream: IntStream = java.util.Arrays.stream(a)
    /** Creates a parallel [[java.util.stream.IntStream Java IntStream]] for this array. */
    def asJavaParStream: IntStream = asJavaSeqStream.parallel
  }

  /** Provides `asJavaSeqStream` and `asJavaParStream` extension methods that create sequential
   *  or parallel [[java.util.stream.LongStream LongStream]]s over a `Long` array.
   *
   *  @param a the array to create Streams for
   */
  implicit class LongArrayHasSeqParStream(a: Array[Long]) {
    /** Creates a sequential [[java.util.stream.LongStream Java LongStream]] for this array. */
    def asJavaSeqStream: LongStream = java.util.Arrays.stream(a)
    /** Creates a parallel [[java.util.stream.LongStream Java LongStream]] for this array. */
    def asJavaParStream: LongStream = asJavaSeqStream.parallel
  }

  /** Provides `asJavaSeqStream` and `asJavaParStream` extension methods that create sequential
   *  or parallel [[java.util.stream.Stream Java Stream]]s over an array of references.
   *
   *  @tparam A the element type of the array
   *  @param a the array to create Streams for
   */
  implicit class AnyArrayHasSeqParStream[A <: AnyRef](a: Array[A]) {
    /** Creates a sequential [[java.util.stream.Stream Java Stream]] for this array. */
    def asJavaSeqStream: Stream[A] = java.util.Arrays.stream(a)
    /** Creates a parallel [[java.util.stream.Stream Java Stream]] for this array. */
    def asJavaParStream: Stream[A] = asJavaSeqStream.parallel
  }

  /** Provides `asJavaSeqStream` and `asJavaParStream` extension methods that create sequential
   *  or parallel [[java.util.stream.IntStream IntStream]]s over a `Byte` array; each element is
   *  widened to an `Int`.
   *
   *  @param a the array to create Streams for
   */
  implicit class ByteArrayHasSeqParStream(a: Array[Byte]) {
    /** Creates a sequential [[java.util.stream.IntStream Java IntStream]] for this array. */
    def asJavaSeqStream: IntStream = a.stepper.asJavaSeqStream
    /** Creates a parallel [[java.util.stream.IntStream Java IntStream]] for this array. */
    def asJavaParStream: IntStream = a.stepper.asJavaParStream
  }

  /** Provides `asJavaSeqStream` and `asJavaParStream` extension methods that create sequential
   *  or parallel [[java.util.stream.IntStream IntStream]]s over a `Short` array; each element is
   *  widened to an `Int`.
   *
   *  @param a the array to create Streams for
   */
  implicit class ShortArrayHasSeqParStream(a: Array[Short]) {
    /** Creates a sequential [[java.util.stream.IntStream Java IntStream]] for this array. */
    def asJavaSeqStream: IntStream = a.stepper.asJavaSeqStream
    /** Creates a parallel [[java.util.stream.IntStream Java IntStream]] for this array. */
    def asJavaParStream: IntStream = a.stepper.asJavaParStream
  }

  /** Provides `asJavaSeqStream` and `asJavaParStream` extension methods that create sequential
   *  or parallel [[java.util.stream.IntStream IntStream]]s over a `Char` array; each element is
   *  widened to an `Int`.
   *
   *  @param a the array to create Streams for
   */
  implicit class CharArrayHasSeqParStream(a: Array[Char]) {
    /** Creates a sequential [[java.util.stream.IntStream Java IntStream]] for this array. */
    def asJavaSeqStream: IntStream = a.stepper.asJavaSeqStream
    /** Creates a parallel [[java.util.stream.IntStream Java IntStream]] for this array. */
    def asJavaParStream: IntStream = a.stepper.asJavaParStream
  }

  /** Provides `asJavaSeqStream` and `asJavaParStream` extension methods that create sequential
   *  or parallel [[java.util.stream.DoubleStream DoubleStream]]s over a `Float` array; each
   *  element is widened to a `Double`.
   *
   *  @param a the array to create Streams for
   */
  implicit class FloatArrayHasSeqParStream(a: Array[Float]) {
    /** Creates a sequential [[java.util.stream.DoubleStream Java DoubleStream]] for this array. */
    def asJavaSeqStream: DoubleStream = a.stepper.asJavaSeqStream
    /** Creates a parallel [[java.util.stream.DoubleStream Java DoubleStream]] for this array. */
    def asJavaParStream: DoubleStream = a.stepper.asJavaParStream
  }



  // strings

  /** Provides extension methods that create sequential or parallel
   *  [[java.util.stream.IntStream IntStream]]s over the characters or the code points of a
   *  `String`.
   *
   *  @param s the string to create Streams for
   */
  implicit class StringHasSeqParStream(s: String) {
    /** A sequential stream on the characters of a string, same as [[asJavaSeqCharStream]]. See also
     *  [[asJavaSeqCodePointStream]].
     */
    def asJavaSeqStream: IntStream = StreamSupport.intStream(s.stepper.spliterator, /* par = */ false)
    /** A parallel stream on the characters of a string, same as [[asJavaParCharStream]]. See also
     *  [[asJavaParCodePointStream]].
     */
    def asJavaParStream: IntStream = StreamSupport.intStream(s.stepper.spliterator, /* par = */ true)

    /** A sequential stream on the characters of a string. See also  [[asJavaSeqCodePointStream]]. */
    def asJavaSeqCharStream: IntStream = StreamSupport.intStream(s.charStepper.spliterator, /* par = */ false)
    /** A parallel stream on the characters of a string. See also [[asJavaParCodePointStream]]. */
    def asJavaParCharStream: IntStream = StreamSupport.intStream(s.charStepper.spliterator, /* par = */ true)

    /** A sequential stream on the code points of a string. See also [[asJavaSeqCharStream]]. */
    def asJavaSeqCodePointStream: IntStream = StreamSupport.intStream(s.codePointStepper.spliterator, /* par = */ false)
    /** A parallel stream on the code points of a string. See also [[asJavaParCharStream]]. */
    def asJavaParCodePointStream: IntStream = StreamSupport.intStream(s.codePointStepper.spliterator, /* par = */ true)
  }

  // toScala for streams

  /** Provides the `toScala` extension method that collects the elements of a Java Stream into a
   *  Scala collection, and the `asJavaPrimitiveStream` extension method that unboxes a Stream of
   *  boxed primitives to the corresponding primitive Stream.
   *
   *  @tparam A the element type of the stream
   *  @param stream the Java Stream to convert
   */
  implicit class StreamHasToScala[A](stream: Stream[A]) {
    /** Copies the elements of this stream into a Scala collection.
     *
     *  Converting a parallel streams to an [[scala.jdk.Accumulator]] using `stream.toScala(Accumulator)`
     *  builds the result in parallel.
     *
     *  A `toScala(Accumulator)` call automatically converts streams of boxed integers, longs or
     *  doubles are converted to the primitive accumulators ([[scala.jdk.IntAccumulator]], etc.).
     *
     *  When converting a parallel stream to a different Scala collection, the stream is first
     *  converted into an [[scala.jdk.Accumulator]], which supports parallel building. The accumulator is
     *  then converted to the target collection. Note that the stream is processed eagerly while
     *  building the accumulator, even if the target collection is lazy.
     *
     *  Sequential streams are directly converted to the target collection. If the target collection
     *  is lazy, the conversion is lazy as well.
     *
     *  @tparam C1 the type of the target Scala collection
     *  @param factory the factory used to build the target collection
     *  @param info implicit evidence connecting the element type to a specialized `Accumulator`, or a generic fallback (resolved automatically)
     *  @return the elements of this stream collected into a Scala collection of type `C1`
     */
    def toScala[C1](factory: collection.Factory[A, C1])(implicit info: AccumulatorFactoryInfo[A, C1]): C1 = {

      def anyAcc = stream.collect(AnyAccumulator.supplier[A], AnyAccumulator.adder[A], AnyAccumulator.merger[A])
      if (info.companion == AnyAccumulator) anyAcc.asInstanceOf[C1]
      else if (info.companion == IntAccumulator) stream.asInstanceOf[Stream[Int]].collect(IntAccumulator.supplier, IntAccumulator.boxedAdder, IntAccumulator.merger).asInstanceOf[C1]
      else if (info.companion == LongAccumulator) stream.asInstanceOf[Stream[Long]].collect(LongAccumulator.supplier, LongAccumulator.boxedAdder, LongAccumulator.merger).asInstanceOf[C1]
      else if (info.companion == DoubleAccumulator) stream.asInstanceOf[Stream[Double]].collect(DoubleAccumulator.supplier, DoubleAccumulator.boxedAdder, DoubleAccumulator.merger).asInstanceOf[C1]
      else if (stream.isParallel) anyAcc.to(factory)
      else factory.fromSpecific(stream.iterator.asScala)
    }

    /** Converts a generic Java Stream wrapping a primitive type to a corresponding primitive
     *  Stream.
     *
     *  @tparam S the resulting primitive stream type (e.g., `IntStream`, `LongStream`, `DoubleStream`)
     *  @param unboxer implicit conversion from boxed `Stream[A]` to primitive stream `S`
     *  @return a primitive Java Stream of type `S` containing the unboxed elements of this stream
     */
    def asJavaPrimitiveStream[S](implicit unboxer: StreamUnboxer[A, S]): S = unboxer(stream)
  }

  /** Provides the `toScala` extension method that collects the elements of a Java `IntStream`
   *  into a Scala collection.
   *
   *  @param stream the Java `IntStream` to convert
   */
  implicit class IntStreamHasToScala(stream: IntStream) {
    /** Copies the elements of this stream into a Scala collection.
     *
     *  Converting a parallel streams to an [[scala.jdk.Accumulator]] using `stream.toScala(Accumulator)`
     *  builds the result in parallel.
     *
     *  A `toScala(Accumulator)` call automatically converts the `IntStream` to a primitive
     *  [[scala.jdk.IntAccumulator]].
     *
     *  When converting a parallel stream to a different Scala collection, the stream is first
     *  converted into an [[scala.jdk.Accumulator]], which supports parallel building. The accumulator is
     *  then converted to the target collection. Note that the stream is processed eagerly while
     *  building the accumulator, even if the target collection is lazy.
     *
     *  Sequential streams are directly converted to the target collection. If the target collection
     *  is lazy, the conversion is lazy as well.
     *
     *  @tparam C1 the type of the target Scala collection
     *  @param factory the factory used to build the target collection
     *  @param info implicit evidence connecting `Int` to a specialized `IntAccumulator`, or a generic fallback
     *  @return the elements of this stream collected into a Scala collection of type `C1`
     */
    def toScala[C1](factory: collection.Factory[Int, C1])(implicit info: AccumulatorFactoryInfo[Int, C1]): C1 = {
      def intAcc = stream.collect(IntAccumulator.supplier, IntAccumulator.adder, IntAccumulator.merger)
      if (info.companion == AnyAccumulator) stream.collect(AnyAccumulator.supplier[Int], AnyAccumulator.unboxedIntAdder, AnyAccumulator.merger[Int]).asInstanceOf[C1]
      else if (info.companion == IntAccumulator) intAcc.asInstanceOf[C1]
      else if (stream.isParallel) intAcc.to(factory)
      else factory.fromSpecific(stream.iterator.asInstanceOf[java.util.Iterator[Int]].asScala)
    }
  }

  /** Provides the `toScala` extension method that collects the elements of a Java `LongStream`
   *  into a Scala collection.
   *
   *  @param stream the Java `LongStream` to convert
   */
  implicit class LongStreamHasToScala(stream: LongStream) {
    /** Copies the elements of this stream into a Scala collection.
     *
     *  Converting a parallel streams to an [[scala.jdk.Accumulator]] using `stream.toScala(Accumulator)`
     *  builds the result in parallel.
     *
     *  A `toScala(Accumulator)` call automatically converts the `LongStream` to a primitive
     *  [[scala.jdk.LongAccumulator]].
     *
     *  When converting a parallel stream to a different Scala collection, the stream is first
     *  converted into an [[scala.jdk.Accumulator]], which supports parallel building. The accumulator is
     *  then converted to the target collection. Note that the stream is processed eagerly while
     *  building the accumulator, even if the target collection is lazy.
     *
     *  Sequential streams are directly converted to the target collection. If the target collection
     *  is lazy, the conversion is lazy as well.
     *
     *  @tparam C1 the type of the target Scala collection
     *  @param factory the factory used to build the target collection
     *  @param info implicit evidence connecting `Long` to a specialized `LongAccumulator`, or a generic fallback
     *  @return the elements of this stream collected into a Scala collection of type `C1`
     */
    def toScala[C1](factory: collection.Factory[Long, C1])(implicit info: AccumulatorFactoryInfo[Long, C1]): C1 = {
      def longAcc = stream.collect(LongAccumulator.supplier, LongAccumulator.adder, LongAccumulator.merger)
      if (info.companion == AnyAccumulator) stream.collect(AnyAccumulator.supplier[Long], AnyAccumulator.unboxedLongAdder, AnyAccumulator.merger[Long]).asInstanceOf[C1]
      else if (info.companion == LongAccumulator) longAcc.asInstanceOf[C1]
      else if (stream.isParallel) longAcc.to(factory)
      else factory.fromSpecific(stream.iterator.asInstanceOf[java.util.Iterator[Long]].asScala)
    }
  }

  /** Provides the `toScala` extension method that collects the elements of a Java `DoubleStream`
   *  into a Scala collection.
   *
   *  @param stream the Java `DoubleStream` to convert
   */
  implicit class DoubleStreamHasToScala(stream: DoubleStream) {
    /** Copies the elements of this stream into a Scala collection.
     *
     *  Converting a parallel streams to an [[scala.jdk.Accumulator]] using `stream.toScala(Accumulator)`
     *  builds the result in parallel.
     *
     *  A `toScala(Accumulator)` call automatically converts the `DoubleStream` to a primitive
     *  [[scala.jdk.DoubleAccumulator]].
     *
     *  When converting a parallel stream to a different Scala collection, the stream is first
     *  converted into an [[scala.jdk.Accumulator]], which supports parallel building. The accumulator is
     *  then converted to the target collection. Note that the stream is processed eagerly while
     *  building the accumulator, even if the target collection is lazy.
     *
     *  Sequential streams are directly converted to the target collection. If the target collection
     *  is lazy, the conversion is lazy as well.
     *
     *  @tparam C1 the type of the target Scala collection
     *  @param factory the factory used to build the target collection
     *  @param info implicit evidence connecting `Double` to a specialized `DoubleAccumulator`, or a generic fallback
     *  @return the elements of this stream collected into a Scala collection of type `C1`
     */
    def toScala[C1](factory: collection.Factory[Double, C1])(implicit info: AccumulatorFactoryInfo[Double, C1]): C1 = {
      def doubleAcc = stream.collect(DoubleAccumulator.supplier, DoubleAccumulator.adder, DoubleAccumulator.merger)
      if (info.companion == AnyAccumulator) stream.collect(AnyAccumulator.supplier[Double], AnyAccumulator.unboxedDoubleAdder, AnyAccumulator.merger[Double]).asInstanceOf[C1]
      else if (info.companion == DoubleAccumulator) doubleAcc.asInstanceOf[C1]
      else if (stream.isParallel) doubleAcc.to(factory)
      else factory.fromSpecific(stream.iterator.asInstanceOf[java.util.Iterator[Double]].asScala)
    }
  }
}

object StreamExtensions {
  /** An implicit StreamShape instance connects element types with the corresponding specialized
   *  Stream and Stepper types. This is used in `asJavaStream` extension methods to create
   *  generic or primitive streams according to the element type.
   *
   *  @tparam T the element type of the collection
   *  @tparam S the type of Java Stream (e.g., `Stream[T]`, `IntStream`, `LongStream`, `DoubleStream`)
   *  @tparam St the type of `Stepper` used to traverse elements
   */
  sealed trait StreamShape[T, S <: BaseStream[?, ?], St <: Stepper[?]] {
    /** Creates a Java Stream over the elements of the given stepper by delegating to
     *  `mkStream`.
     *
     *  @param st the stepper providing the elements
     *  @param par whether the returned stream is parallel (`true`) or sequential (`false`)
     *  @return a Java Stream of type `S` over the stepper's elements
     */
    final def fromStepper(st: St, par: Boolean): S = mkStream(st, par)
    /** Creates a Java Stream of type `S` from the given stepper's spliterator.
     *
     *  @param st the stepper providing the elements
     *  @param par whether the returned stream is parallel (`true`) or sequential (`false`)
     *  @return a Java Stream of type `S` over the stepper's elements
     */
    protected def mkStream(st: St, par: Boolean): S
  }

  object StreamShape extends StreamShapeLowPriority1 {
    // primitive
    /** The `StreamShape` mapping `Int` elements to `IntStream` and `IntStepper`. */
    implicit val intStreamShape   : StreamShape[Int   , IntStream   , IntStepper]    = mkIntStreamShape[Int]
    /** The `StreamShape` mapping `Long` elements to `LongStream` and `LongStepper`. */
    implicit val longStreamShape  : StreamShape[Long  , LongStream  , LongStepper]   = mkLongStreamShape[Long]
    /** The `StreamShape` mapping `Double` elements to `DoubleStream` and `DoubleStepper`. */
    implicit val doubleStreamShape: StreamShape[Double, DoubleStream, DoubleStepper] = mkDoubleStreamShape[Double]

    // widening
    /** The `StreamShape` mapping `Byte` elements to `IntStream` and `IntStepper`; each element is widened to an `Int`. */
    implicit val byteStreamShape : StreamShape[Byte , IntStream   , IntStepper]    = mkIntStreamShape[Byte]
    /** The `StreamShape` mapping `Short` elements to `IntStream` and `IntStepper`; each element is widened to an `Int`. */
    implicit val shortStreamShape: StreamShape[Short, IntStream   , IntStepper]    = mkIntStreamShape[Short]
    /** The `StreamShape` mapping `Char` elements to `IntStream` and `IntStepper`; each element is widened to an `Int`. */
    implicit val charStreamShape : StreamShape[Char , IntStream   , IntStepper]    = mkIntStreamShape[Char]
    /** The `StreamShape` mapping `Float` elements to `DoubleStream` and `DoubleStepper`; each element is widened to a `Double`. */
    implicit val floatStreamShape: StreamShape[Float, DoubleStream, DoubleStepper] = mkDoubleStreamShape[Float]

    // boxed java primitives

    /** The `StreamShape` mapping boxed `java.lang.Integer` elements to the primitive `IntStream` and `IntStepper`. */
    implicit val jIntegerStreamShape   : StreamShape[jl.Integer  , IntStream   , IntStepper   ] = mkIntStreamShape[jl.Integer]
    /** The `StreamShape` mapping boxed `java.lang.Long` elements to the primitive `LongStream` and `LongStepper`. */
    implicit val jLongStreamShape      : StreamShape[jl.Long     , LongStream  , LongStepper  ] = mkLongStreamShape[jl.Long]
    /** The `StreamShape` mapping boxed `java.lang.Double` elements to the primitive `DoubleStream` and `DoubleStepper`. */
    implicit val jDoubleStreamShape    : StreamShape[jl.Double   , DoubleStream, DoubleStepper] = mkDoubleStreamShape[jl.Double]
    /** The `StreamShape` mapping boxed `java.lang.Byte` elements to `IntStream` and `IntStepper`; each element is widened to an `Int`. */
    implicit val jByteStreamShape      : StreamShape[jl.Byte     , IntStream   , IntStepper   ] = mkIntStreamShape[jl.Byte]
    /** The `StreamShape` mapping boxed `java.lang.Short` elements to `IntStream` and `IntStepper`; each element is widened to an `Int`. */
    implicit val jShortStreamShape     : StreamShape[jl.Short    , IntStream   , IntStepper   ] = mkIntStreamShape[jl.Short]
    /** The `StreamShape` mapping boxed `java.lang.Character` elements to `IntStream` and `IntStepper`; each element is widened to an `Int`. */
    implicit val jCharacterStreamShape : StreamShape[jl.Character, IntStream   , IntStepper   ] = mkIntStreamShape[jl.Character]
    /** The `StreamShape` mapping boxed `java.lang.Float` elements to `DoubleStream` and `DoubleStepper`; each element is widened to a `Double`. */
    implicit val jFloatStreamShape     : StreamShape[jl.Float    , DoubleStream, DoubleStepper] = mkDoubleStreamShape[jl.Float]

    private def mkIntStreamShape[T]: StreamShape[T, IntStream, IntStepper] = new StreamShape[T, IntStream, IntStepper] {
      protected def mkStream(st: IntStepper, par: Boolean): IntStream = StreamSupport.intStream(st.spliterator, par)
    }

    private def mkLongStreamShape[T]: StreamShape[T, LongStream, LongStepper] = new StreamShape[T, LongStream, LongStepper] {
      protected def mkStream(st: LongStepper, par: Boolean): LongStream = StreamSupport.longStream(st.spliterator, par)
    }

    private def mkDoubleStreamShape[T]: StreamShape[T, DoubleStream, DoubleStepper] = new StreamShape[T, DoubleStream, DoubleStepper] {
      protected def mkStream(st: DoubleStepper, par: Boolean): DoubleStream = StreamSupport.doubleStream(st.spliterator, par)
    }
  }

  /** Defines the low-priority fallback `StreamShape` for arbitrary element types, used when no
   *  specialized shape applies.
   */
  trait StreamShapeLowPriority1 {
    // reference
    /** Returns the fallback `StreamShape` mapping elements of any type `T` to a generic (boxed)
     *  `Stream[T]`. Applies only when `T` has no specialized shape.
     *
     *  @tparam T the element type
     *  @return the single cached `StreamShape` instance, cast to element type `T`
     */
    implicit def anyStreamShape[T]: StreamShape[T, Stream[T], Stepper[T]] = anyStreamShapePrototype.asInstanceOf[StreamShape[T, Stream[T], Stepper[T]]]

    private val anyStreamShapePrototype: StreamShape[AnyRef, Stream[AnyRef], Stepper[AnyRef]] = new StreamShape[AnyRef, Stream[AnyRef], Stepper[AnyRef]] {
      def mkStream(s: Stepper[AnyRef], par: Boolean): Stream[AnyRef] = StreamSupport.stream(s.spliterator.asInstanceOf[Spliterator[AnyRef]], par)
    }
  }

  /** Connects a stream element type `A` to the corresponding, potentially specialized, Stream type.
   *  Used in the `stream.asJavaPrimitiveStream` extension method.
   *
   *  @tparam A the boxed element type of the stream
   *  @tparam S the target primitive stream type (e.g., `IntStream`, `LongStream`, `DoubleStream`)
   */
  sealed trait StreamUnboxer[A, S] {
    /** Converts the given boxed Java Stream to the corresponding primitive stream.
     *
     *  @param s the boxed Java Stream to convert
     *  @return a primitive stream of type `S` with the unboxed elements of `s`
     */
    def apply(s: Stream[A]): S
  }
  object StreamUnboxer {
    /** The `StreamUnboxer` converting a `Stream[Int]` to a primitive `IntStream`. */
    implicit val intStreamUnboxer: StreamUnboxer[Int, IntStream] = new StreamUnboxer[Int, IntStream] {
      def apply(s: Stream[Int]): IntStream = s.mapToInt(x => x)
    }
    /** The `StreamUnboxer` converting a `Stream[java.lang.Integer]` to a primitive `IntStream`. */
    implicit val javaIntegerStreamUnboxer: StreamUnboxer[jl.Integer, IntStream] = intStreamUnboxer.asInstanceOf[StreamUnboxer[jl.Integer, IntStream]]

    /** The `StreamUnboxer` converting a `Stream[Long]` to a primitive `LongStream`. */
    implicit val longStreamUnboxer: StreamUnboxer[Long, LongStream] = new StreamUnboxer[Long, LongStream] {
      def apply(s: Stream[Long]): LongStream = s.mapToLong(x => x)
    }
    /** The `StreamUnboxer` converting a `Stream[java.lang.Long]` to a primitive `LongStream`. */
    implicit val javaLongStreamUnboxer: StreamUnboxer[jl.Long, LongStream] = longStreamUnboxer.asInstanceOf[StreamUnboxer[jl.Long, LongStream]]

    /** The `StreamUnboxer` converting a `Stream[Double]` to a primitive `DoubleStream`. */
    implicit val doubleStreamUnboxer: StreamUnboxer[Double, DoubleStream] = new StreamUnboxer[Double, DoubleStream] {
      def apply(s: Stream[Double]): DoubleStream = s.mapToDouble(x => x)
    }
    /** The `StreamUnboxer` converting a `Stream[java.lang.Double]` to a primitive `DoubleStream`. */
    implicit val javaDoubleStreamUnboxer: StreamUnboxer[jl.Double, DoubleStream] = doubleStreamUnboxer.asInstanceOf[StreamUnboxer[jl.Double, DoubleStream]]
  }



  /** An implicit `AccumulatorFactoryInfo` connects primitive element types to the corresponding
   *  specialized [[scala.jdk.Accumulator]] factory. This is used in the `stream.toScala` extension methods
   *  to ensure collecting a primitive stream into a primitive accumulator does not box.
   *
   *  When converting to a collection other than `Accumulator`, the generic
   *  `noAccumulatorFactoryInfo` is passed.
   *
   *  @tparam A the element type of the stream
   *  @tparam C the target collection type, potentially a specialized `Accumulator`
   */
  trait AccumulatorFactoryInfo[A, C] {
    /** The companion object of the target `Accumulator` type ([[scala.jdk.AnyAccumulator]],
     *  [[scala.jdk.IntAccumulator]], etc.), or `null` when the target collection is not an
     *  accumulator.
     */
    val companion: AnyRef | Null
  }
  /** Defines the low-priority fallback `AccumulatorFactoryInfo`, used when the target collection
   *  type is not an `Accumulator`.
   */
  trait LowPriorityAccumulatorFactoryInfo {
    /** Returns the fallback `AccumulatorFactoryInfo`, whose `companion` is `null`. Applies only
     *  when the target collection type `C` is not an `Accumulator`.
     *
     *  @tparam A the element type of the stream
     *  @tparam C the type of the target collection
     *  @return the single cached fallback instance, cast to `A` and `C`
     */
    implicit def noAccumulatorFactoryInfo[A, C]: AccumulatorFactoryInfo[A, C] = noAccumulatorFactoryInfoPrototype.asInstanceOf[AccumulatorFactoryInfo[A, C]]
    private val noAccumulatorFactoryInfoPrototype: AccumulatorFactoryInfo[AnyRef, AnyRef] = new AccumulatorFactoryInfo[AnyRef, AnyRef] {
      val companion: AnyRef | Null = null
    }
  }
  object AccumulatorFactoryInfo extends LowPriorityAccumulatorFactoryInfo {
    /** Returns the `AccumulatorFactoryInfo` for collecting elements of any type into an
     *  [[scala.jdk.AnyAccumulator]].
     *
     *  @tparam A the element type of the stream
     *  @return the single cached instance, whose `companion` is `AnyAccumulator`, cast to
     *          element type `A`
     */
    implicit def anyAccumulatorFactoryInfo[A]: AccumulatorFactoryInfo[A, AnyAccumulator[A]] = anyAccumulatorFactoryInfoPrototype.asInstanceOf[AccumulatorFactoryInfo[A, AnyAccumulator[A]]]

    private object anyAccumulatorFactoryInfoPrototype extends AccumulatorFactoryInfo[AnyRef, AnyAccumulator[AnyRef]] {
      /** The [[scala.jdk.AnyAccumulator]] companion object. */
      val companion: AnyRef | Null = AnyAccumulator
    }

    /** The `AccumulatorFactoryInfo` for collecting `Int` elements into an [[scala.jdk.IntAccumulator]] without boxing. */
    implicit val intAccumulatorFactoryInfo: AccumulatorFactoryInfo[Int, IntAccumulator] = new AccumulatorFactoryInfo[Int, IntAccumulator] {
      val companion: AnyRef | Null = IntAccumulator
    }

    /** The `AccumulatorFactoryInfo` for collecting `Long` elements into a [[scala.jdk.LongAccumulator]] without boxing. */
    implicit val longAccumulatorFactoryInfo: AccumulatorFactoryInfo[Long, LongAccumulator] = new AccumulatorFactoryInfo[Long, LongAccumulator] {
      val companion: AnyRef | Null = LongAccumulator
    }

    /** The `AccumulatorFactoryInfo` for collecting `Double` elements into a [[scala.jdk.DoubleAccumulator]] without boxing. */
    implicit val doubleAccumulatorFactoryInfo: AccumulatorFactoryInfo[Double, DoubleAccumulator] = new AccumulatorFactoryInfo[Double, DoubleAccumulator] {
      val companion: AnyRef | Null = DoubleAccumulator
    }

    /** The `AccumulatorFactoryInfo` for collecting boxed `java.lang.Integer` elements into an [[scala.jdk.IntAccumulator]], reusing `intAccumulatorFactoryInfo`. */
    implicit val jIntegerAccumulatorFactoryInfo: AccumulatorFactoryInfo[jl.Integer, IntAccumulator] = intAccumulatorFactoryInfo.asInstanceOf[AccumulatorFactoryInfo[jl.Integer, IntAccumulator]]
    /** The `AccumulatorFactoryInfo` for streams of boxed `java.lang.Long` elements, reusing
     *  `longAccumulatorFactoryInfo`.
     */
    implicit val jLongAccumulatorFactoryInfo: AccumulatorFactoryInfo[jl.Long, IntAccumulator] = longAccumulatorFactoryInfo.asInstanceOf[AccumulatorFactoryInfo[jl.Long, IntAccumulator]]
    /** The `AccumulatorFactoryInfo` for streams of boxed `java.lang.Double` elements, reusing
     *  `doubleAccumulatorFactoryInfo`.
     */
    implicit val jDoubleAccumulatorFactoryInfo: AccumulatorFactoryInfo[jl.Double, IntAccumulator] = doubleAccumulatorFactoryInfo.asInstanceOf[AccumulatorFactoryInfo[jl.Double, IntAccumulator]]
  }
}
