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

package scala.jdk.javaapi

import scala.language.`2.13`
import java.util.stream.{DoubleStream, IntStream, LongStream, Stream, StreamSupport}
import java.{lang => jl}

/** This object contains methods to create Java Streams that operate on Scala collections
 *  (sequentially or in parallel). For more information on Java streams, consult the documentation
 *  ([[https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/stream/package-summary.html]]).
 *
 *  The explicit conversion methods defined here are intended to be used in Java code. For Scala
 *  code, it is recommended to use the extension methods defined in [[scala.jdk.StreamConverters]].
 *
 *  Note: to convert between Scala collections and classic Java collections, use
 *  [[CollectionConverters]].
 *
 *  For details how the stream converters work, see [[scala.jdk.StreamConverters]].
 *
 *  @define parNote Note: parallel processing is only efficient for collections that have a
 *                 [[scala.collection.Stepper]] implementation which supports efficient splitting. For collections
 *                 where this is the case, the [[scala.collection.IterableOnce.stepper `stepper`]]
 *                 method has a return type marked `with EfficientSplit`.
 *
 *  @define primitiveNote Note: this method uses the boxed type `java.lang.X` instead of the
 *                       primitive type `scala.X` to improve compatibility when using it in
 *                       Java code (the Scala compiler emits `C[Int]` as `C[Object]` in bytecode
 *                       due to [scala/bug#4214](https://github.com/scala/bug/issues/4214)). In
 *                       Scala code, add `import scala.jdk.StreamConverters._` and use the
 *                       extension methods instead.
 */
object StreamConverters {
  /////////////////////////////////////
  // sequential streams for collections
  /////////////////////////////////////

  /** Creates a sequential [[java.util.stream.Stream Java Stream]] for a Scala collection.
   *
   *  @tparam A the element type of the collection
   *  @param cc the Scala collection to convert to a Java Stream
   *  @return a sequential `Stream` for the collection
   */
  def asJavaSeqStream[A](cc: IterableOnce[A]): Stream[A] = StreamSupport.stream(cc.stepper.spliterator, false)

  /** Creates a sequential [[java.util.stream.IntStream Java IntStream]] for a Scala collection.
   *
   *  $primitiveNote
   *
   *  @param cc the Scala collection of integers to convert
   *  @return a sequential `IntStream` for the collection
   */
  def asJavaSeqIntStream         (cc: IterableOnce[jl.Integer]):   IntStream = StreamSupport.intStream(cc.stepper.spliterator, false)
  /** Creates a sequential [[java.util.stream.IntStream Java IntStream]] for a Scala collection.
   *
   *  $primitiveNote
   *
   *  @param cc the Scala collection of bytes to convert
   *  @return a sequential `IntStream` for the collection
   */
  def asJavaSeqIntStreamFromByte (cc: IterableOnce[jl.Byte]):      IntStream = StreamSupport.intStream(cc.stepper.spliterator, false)
  /** Creates a sequential [[java.util.stream.IntStream Java IntStream]] for a Scala collection.
   *
   *  $primitiveNote
   *
   *  @param cc the Scala collection of shorts to convert
   *  @return a sequential `IntStream` for the collection
   */
  def asJavaSeqIntStreamFromShort(cc: IterableOnce[jl.Short]):     IntStream = StreamSupport.intStream(cc.stepper.spliterator, false)
  /** Creates a sequential [[java.util.stream.IntStream Java IntStream]] for a Scala collection.
   *
   *  $primitiveNote
   *
   *  @param cc the Scala collection of characters to convert
   *  @return a sequential `IntStream` for the collection
   */
  def asJavaSeqIntStreamFromChar (cc: IterableOnce[jl.Character]): IntStream = StreamSupport.intStream(cc.stepper.spliterator, false)

  /** Creates a sequential [[java.util.stream.DoubleStream Java DoubleStream]] for a Scala collection.
   *
   *  $primitiveNote
   *
   *  @param cc the Scala collection of doubles to convert
   *  @return a sequential `DoubleStream` for the collection
   */
  def asJavaSeqDoubleStream         (cc: IterableOnce[jl.Double]): DoubleStream = StreamSupport.doubleStream(cc.stepper.spliterator, false)
  /** Creates a sequential [[java.util.stream.DoubleStream Java DoubleStream]] for a Scala collection.
   *
   *  $primitiveNote
   *
   *  @param cc the Scala collection of floats to convert
   *  @return a sequential `DoubleStream` for the collection
   */
  def asJavaSeqDoubleStreamFromFloat(cc: IterableOnce[jl.Float]):  DoubleStream = StreamSupport.doubleStream(cc.stepper.spliterator, false)

  /** Creates a sequential [[java.util.stream.LongStream Java LongStream]] for a Scala collection.
   *
   *  $primitiveNote
   *
   *  @param cc the Scala collection of longs to convert
   *  @return a sequential `LongStream` for the collection
   */
  def asJavaSeqLongStream(cc: IterableOnce[jl.Long]): LongStream = StreamSupport.longStream(cc.stepper.spliterator, false)

  // Map Key Streams

  /** Creates a sequential [[java.util.stream.Stream Java Stream]] for the keys of a Scala Map.
   *
   *  @tparam K the key type of the map
   *  @tparam V the value type of the map
   *  @param m the Scala Map whose keys to stream over
   *  @return a sequential `Stream` for the map's keys
   */
  def asJavaSeqKeyStream[K, V](m: collection.Map[K, V]): Stream[K] = StreamSupport.stream(m.keyStepper.spliterator, false)

  /** Creates a sequential [[java.util.stream.IntStream Java IntStream]] for the keys of a Scala Map.
   *
   *  $primitiveNote
   *
   *  @tparam V the value type of the map
   *  @param m the Scala Map whose keys to stream over
   *  @return a sequential `IntStream` for the map's keys
   */
  def asJavaSeqKeyIntStream         [V](m: collection.Map[jl.Integer, V]):   IntStream = StreamSupport.intStream(m.keyStepper.spliterator, false)
  /** Creates a sequential [[java.util.stream.IntStream Java IntStream]] for the keys of a Scala Map.
   *
   *  $primitiveNote
   *
   *  @tparam V the value type of the map
   *  @param m the Scala Map whose keys to stream over
   *  @return a sequential `IntStream` for the map's keys
   */
  def asJavaSeqKeyIntStreamFromByte [V](m: collection.Map[jl.Byte, V]):      IntStream = StreamSupport.intStream(m.keyStepper.spliterator, false)
  /** Creates a sequential [[java.util.stream.IntStream Java IntStream]] for the keys of a Scala Map.
   *
   *  $primitiveNote
   *
   *  @tparam V the value type of the map
   *  @param m the Scala Map whose keys to stream over
   *  @return a sequential `IntStream` for the map's keys
   */
  def asJavaSeqKeyIntStreamFromShort[V](m: collection.Map[jl.Short, V]):     IntStream = StreamSupport.intStream(m.keyStepper.spliterator, false)
  /** Creates a sequential [[java.util.stream.IntStream Java IntStream]] for the keys of a Scala Map.
   *
   *  $primitiveNote
   *
   *  @tparam V the value type of the map
   *  @param m the Scala Map whose keys to stream over
   *  @return a sequential `IntStream` for the map's keys
   */
  def asJavaSeqKeyIntStreamFromChar [V](m: collection.Map[jl.Character, V]): IntStream = StreamSupport.intStream(m.keyStepper.spliterator, false)

  /** Creates a sequential [[java.util.stream.DoubleStream Java DoubleStream]] for the keys of a Scala Map.
   *
   *  $primitiveNote
   *
   *  @tparam V the value type of the map
   *  @param m the Scala Map whose keys to stream over
   *  @return a sequential `DoubleStream` for the map's keys
   */
  def asJavaSeqKeyDoubleStream         [V](m: collection.Map[jl.Double, V]): DoubleStream = StreamSupport.doubleStream(m.keyStepper.spliterator, false)
  /** Creates a sequential [[java.util.stream.DoubleStream Java DoubleStream]] for the keys of a Scala Map.
   *
   *  $primitiveNote
   *
   *  @tparam V the value type of the map
   *  @param m the Scala Map whose keys to stream over
   *  @return a sequential `DoubleStream` for the map's keys
   */
  def asJavaSeqKeyDoubleStreamFromFloat[V](m: collection.Map[jl.Float, V]):  DoubleStream = StreamSupport.doubleStream(m.keyStepper.spliterator, false)

  /** Creates a sequential [[java.util.stream.LongStream Java LongStream]] for the keys of a Scala Map.
   *
   *  $primitiveNote
   *
   *  @tparam V the value type of the map
   *  @param m the Scala Map whose keys to stream over
   *  @return a sequential `LongStream` for the map's keys
   */
  def asJavaSeqKeyLongStream[V](m: collection.Map[jl.Long, V]): LongStream = StreamSupport.longStream(m.keyStepper.spliterator, false)

  // Map Value Streams

  /** Creates a sequential [[java.util.stream.Stream Java Stream]] for the values of a Scala Map.
   *
   *  @tparam K the key type of the map
   *  @tparam V the value type of the map
   *  @param m the Scala Map whose values to stream over
   *  @return a sequential `Stream` for the map's values
   */
  def asJavaSeqValueStream[K, V](m: collection.Map[K, V]): Stream[V] = StreamSupport.stream(m.valueStepper.spliterator, false)

  /** Creates a sequential [[java.util.stream.IntStream Java IntStream]] for the values of a Scala Map.
   *
   *  $primitiveNote
   *
   *  @tparam K the key type of the map
   *  @param m the Scala Map whose values to stream over
   *  @return a sequential `IntStream` for the map's values
   */
  def asJavaSeqValueIntStream         [K](m: collection.Map[K, jl.Integer]):   IntStream = StreamSupport.intStream(m.valueStepper.spliterator, false)
  /** Creates a sequential [[java.util.stream.IntStream Java IntStream]] for the values of a Scala Map.
   *
   *  $primitiveNote
   *
   *  @tparam K the key type of the map
   *  @param m the Scala Map whose values to stream over
   *  @return a sequential `IntStream` for the map's values
   */
  def asJavaSeqValueIntStreamFromByte [K](m: collection.Map[K, jl.Byte]):      IntStream = StreamSupport.intStream(m.valueStepper.spliterator, false)
  /** Creates a sequential [[java.util.stream.IntStream Java IntStream]] for the values of a Scala Map.
   *
   *  $primitiveNote
   *
   *  @tparam K the key type of the map
   *  @param m the Scala Map whose values to stream over
   *  @return a sequential `IntStream` for the map's values
   */
  def asJavaSeqValueIntStreamFromShort[K](m: collection.Map[K, jl.Short]):     IntStream = StreamSupport.intStream(m.valueStepper.spliterator, false)
  /** Creates a sequential [[java.util.stream.IntStream Java IntStream]] for the values of a Scala Map.
   *
   *  $primitiveNote
   *
   *  @tparam K the key type of the map
   *  @param m the Scala Map whose values to stream over
   *  @return a sequential `IntStream` for the map's values
   */
  def asJavaSeqValueIntStreamFromChar [K](m: collection.Map[K, jl.Character]): IntStream = StreamSupport.intStream(m.valueStepper.spliterator, false)

  /** Creates a sequential [[java.util.stream.DoubleStream Java DoubleStream]] for the values of a Scala Map.
   *
   *  $primitiveNote
   *
   *  @tparam K the key type of the map
   *  @param m the Scala Map whose values to stream over
   *  @return a sequential `DoubleStream` for the map's values
   */
  def asJavaSeqValueDoubleStream         [K](m: collection.Map[K, jl.Double]): DoubleStream = StreamSupport.doubleStream(m.valueStepper.spliterator, false)
  /** Creates a sequential [[java.util.stream.DoubleStream Java DoubleStream]] for the values of a Scala Map.
   *
   *  $primitiveNote
   *
   *  @tparam K the key type of the map
   *  @param m the Scala Map whose values to stream over
   *  @return a sequential `DoubleStream` for the map's values
   */
  def asJavaSeqValueDoubleStreamFromFloat[K](m: collection.Map[K, jl.Float]):  DoubleStream = StreamSupport.doubleStream(m.valueStepper.spliterator, false)

  /** Creates a sequential [[java.util.stream.LongStream Java LongStream]] for the values of a Scala Map.
   *
   *  $primitiveNote
   *
   *  @tparam K the key type of the map
   *  @param m the Scala Map whose values to stream over
   *  @return a sequential `LongStream` for the map's values
   */
  def asJavaSeqValueLongStream[K](m: collection.Map[K, jl.Long]): LongStream = StreamSupport.longStream(m.valueStepper.spliterator, false)

  ///////////////////////////////////
  // parallel streams for collections
  ///////////////////////////////////

  /** Creates a parallel [[java.util.stream.Stream Java Stream]] for a Scala collection.
   *
   *  $parNote
   *
   *  @tparam A the element type of the collection
   *  @param cc the Scala collection to convert to a parallel Java Stream
   *  @return a parallel `Stream` for the collection
   */
  def asJavaParStream[A](cc: IterableOnce[A]): Stream[A] = StreamSupport.stream(cc.stepper.spliterator, true)

  /** Creates a parallel [[java.util.stream.IntStream Java IntStream]] for a Scala collection.
   *
   *  $parNote
   *
   *  $primitiveNote
   *
   *  @param cc the Scala collection of integers to convert
   *  @return a parallel `IntStream` for the collection
   */
  def asJavaParIntStream         (cc: IterableOnce[jl.Integer]):   IntStream = StreamSupport.intStream(cc.stepper.spliterator, true)
  /** Creates a parallel [[java.util.stream.IntStream Java IntStream]] for a Scala collection.
   *
   *  $parNote
   *
   *  $primitiveNote
   *
   *  @param cc the Scala collection of bytes to convert
   *  @return a parallel `IntStream` for the collection
   */
  def asJavaParIntStreamFromByte (cc: IterableOnce[jl.Byte]):      IntStream = StreamSupport.intStream(cc.stepper.spliterator, true)
  /** Creates a parallel [[java.util.stream.IntStream Java IntStream]] for a Scala collection.
   *
   *  $parNote
   *
   *  $primitiveNote
   *
   *  @param cc the Scala collection of shorts to convert
   *  @return a parallel `IntStream` for the collection
   */
  def asJavaParIntStreamFromShort(cc: IterableOnce[jl.Short]):     IntStream = StreamSupport.intStream(cc.stepper.spliterator, true)
  /** Creates a parallel [[java.util.stream.IntStream Java IntStream]] for a Scala collection.
   *
   *  $parNote
   *
   *  $primitiveNote
   *
   *  @param cc the Scala collection of characters to convert
   *  @return a parallel `IntStream` for the collection
   */
  def asJavaParIntStreamFromChar (cc: IterableOnce[jl.Character]): IntStream = StreamSupport.intStream(cc.stepper.spliterator, true)

  /** Creates a parallel [[java.util.stream.DoubleStream Java DoubleStream]] for a Scala collection.
   *
   *  $parNote
   *
   *  $primitiveNote
   *
   *  @param cc the Scala collection of doubles to convert
   *  @return a parallel `DoubleStream` for the collection
   */
  def asJavaParDoubleStream         (cc: IterableOnce[jl.Double]): DoubleStream = StreamSupport.doubleStream(cc.stepper.spliterator, true)
  /** Creates a parallel [[java.util.stream.DoubleStream Java DoubleStream]] for a Scala collection.
   *
   *  $parNote
   *
   *  $primitiveNote
   *
   *  @param cc the Scala collection of floats to convert
   *  @return a parallel `DoubleStream` for the collection
   */
  def asJavaParDoubleStreamFromFloat(cc: IterableOnce[jl.Float]):  DoubleStream = StreamSupport.doubleStream(cc.stepper.spliterator, true)

  /** Creates a parallel [[java.util.stream.LongStream Java LongStream]] for a Scala collection.
   *
   *  $parNote
   *
   *  $primitiveNote
   *
   *  @param cc the Scala collection of longs to convert
   *  @return a parallel `LongStream` for the collection
   */
  def asJavaParLongStream(cc: IterableOnce[jl.Long]): LongStream = StreamSupport.longStream(cc.stepper.spliterator, true)


  // Map Key Streams

  /** Creates a parallel [[java.util.stream.Stream Java Stream]] for the keys of a Scala Map.
   *
   *  $parNote
   *
   *  @tparam K the key type of the map
   *  @tparam V the value type of the map
   *  @param m the Scala Map whose keys to stream over
   *  @return a parallel `Stream` for the map's keys
   */
  def asJavaParKeyStream[K, V](m: collection.Map[K, V]): Stream[K] = StreamSupport.stream(m.keyStepper.spliterator, true)

  /** Creates a parallel [[java.util.stream.IntStream Java IntStream]] for the keys of a Scala Map.
   *
   *  $parNote
   *
   *  $primitiveNote
   *
   *  @tparam V the value type of the map
   *  @param m the Scala Map whose keys to stream over
   *  @return a parallel `IntStream` for the map's keys
   */
  def asJavaParKeyIntStream         [V](m: collection.Map[jl.Integer, V]):   IntStream = StreamSupport.intStream(m.keyStepper.spliterator, true)
  /** Creates a parallel [[java.util.stream.IntStream Java IntStream]] for the keys of a Scala Map.
   *
   *  $parNote
   *
   *  $primitiveNote
   *
   *  @tparam V the value type of the map
   *  @param m the Scala Map whose keys to stream over
   *  @return a parallel `IntStream` for the map's keys
   */
  def asJavaParKeyIntStreamFromByte [V](m: collection.Map[jl.Byte, V]):      IntStream = StreamSupport.intStream(m.keyStepper.spliterator, true)
  /** Creates a parallel [[java.util.stream.IntStream Java IntStream]] for the keys of a Scala Map.
   *
   *  $parNote
   *
   *  $primitiveNote
   *
   *  @tparam V the value type of the map
   *  @param m the Scala Map whose keys to stream over
   *  @return a parallel `IntStream` for the map's keys
   */
  def asJavaParKeyIntStreamFromShort[V](m: collection.Map[jl.Short, V]):     IntStream = StreamSupport.intStream(m.keyStepper.spliterator, true)
  /** Creates a parallel [[java.util.stream.IntStream Java IntStream]] for the keys of a Scala Map.
   *
   *  $parNote
   *
   *  $primitiveNote
   *
   *  @tparam V the value type of the map
   *  @param m the Scala Map whose keys to stream over
   *  @return a parallel `IntStream` for the map's keys
   */
  def asJavaParKeyIntStreamFromChar [V](m: collection.Map[jl.Character, V]): IntStream = StreamSupport.intStream(m.keyStepper.spliterator, true)

  /** Creates a parallel [[java.util.stream.DoubleStream Java DoubleStream]] for the keys of a Scala Map.
   *
   *  $parNote
   *
   *  $primitiveNote
   *
   *  @tparam V the value type of the map
   *  @param m the Scala Map whose keys to stream over
   *  @return a parallel `DoubleStream` for the map's keys
   */
  def asJavaParKeyDoubleStream         [V](m: collection.Map[jl.Double, V]): DoubleStream = StreamSupport.doubleStream(m.keyStepper.spliterator, true)
  /** Creates a parallel [[java.util.stream.DoubleStream Java DoubleStream]] for the keys of a Scala Map.
   *
   *  $parNote
   *
   *  $primitiveNote
   *
   *  @tparam V the value type of the map
   *  @param m the Scala Map whose keys to stream over
   *  @return a parallel `DoubleStream` for the map's keys
   */
  def asJavaParKeyDoubleStreamFromFloat[V](m: collection.Map[jl.Float, V]):  DoubleStream = StreamSupport.doubleStream(m.keyStepper.spliterator, true)

  /** Creates a parallel [[java.util.stream.LongStream Java LongStream]] for the keys of a Scala Map.
   *
   *  $parNote
   *
   *  $primitiveNote
   *
   *  @tparam V the value type of the map
   *  @param m the Scala Map whose keys to stream over
   *  @return a parallel `LongStream` for the map's keys
   */
  def asJavaParKeyLongStream[V](m: collection.Map[jl.Long, V]): LongStream = StreamSupport.longStream(m.keyStepper.spliterator, true)

  // Map Value Streams

  /** Creates a parallel [[java.util.stream.Stream Java Stream]] for the values of a Scala Map.
   *
   *  $parNote
   *
   *  @tparam K the key type of the map
   *  @tparam V the value type of the map
   *  @param m the Scala Map whose values to stream over
   *  @return a parallel `Stream` for the map's values
   */
  def asJavaParValueStream[K, V](m: collection.Map[K, V]): Stream[V] = StreamSupport.stream(m.valueStepper.spliterator, true)

  /** Creates a parallel [[java.util.stream.IntStream Java IntStream]] for the values of a Scala Map.
   *
   *  $parNote
   *
   *  $primitiveNote
   *
   *  @tparam K the key type of the map
   *  @param m the Scala Map whose values to stream over
   *  @return a parallel `IntStream` for the map's values
   */
  def asJavaParValueIntStream         [K](m: collection.Map[K, jl.Integer]):   IntStream = StreamSupport.intStream(m.valueStepper.spliterator, true)
  /** Creates a parallel [[java.util.stream.IntStream Java IntStream]] for the values of a Scala Map.
   *
   *  $parNote
   *
   *  $primitiveNote
   *
   *  @tparam K the key type of the map
   *  @param m the Scala Map whose values to stream over
   *  @return a parallel `IntStream` for the map's values
   */
  def asJavaParValueIntStreamFromByte [K](m: collection.Map[K, jl.Byte]):      IntStream = StreamSupport.intStream(m.valueStepper.spliterator, true)
  /** Creates a parallel [[java.util.stream.IntStream Java IntStream]] for the values of a Scala Map.
   *
   *  $parNote
   *
   *  $primitiveNote
   *
   *  @tparam K the key type of the map
   *  @param m the Scala Map whose values to stream over
   *  @return a parallel `IntStream` for the map's values
   */
  def asJavaParValueIntStreamFromShort[K](m: collection.Map[K, jl.Short]):     IntStream = StreamSupport.intStream(m.valueStepper.spliterator, true)
  /** Creates a parallel [[java.util.stream.IntStream Java IntStream]] for the values of a Scala Map.
   *
   *  $parNote
   *
   *  $primitiveNote
   *
   *  @tparam K the key type of the map
   *  @param m the Scala Map whose values to stream over
   *  @return a parallel `IntStream` for the map's values
   */
  def asJavaParValueIntStreamFromChar [K](m: collection.Map[K, jl.Character]): IntStream = StreamSupport.intStream(m.valueStepper.spliterator, true)

  /** Creates a parallel [[java.util.stream.DoubleStream Java DoubleStream]] for the values of a Scala Map.
   *
   *  $parNote
   *
   *  $primitiveNote
   *
   *  @tparam K the key type of the map
   *  @param m the Scala Map whose values to stream over
   *  @return a parallel `DoubleStream` for the map's values
   */
  def asJavaParValueDoubleStream         [K](m: collection.Map[K, jl.Double]): DoubleStream = StreamSupport.doubleStream(m.valueStepper.spliterator, true)
  /** Creates a parallel [[java.util.stream.DoubleStream Java DoubleStream]] for the values of a Scala Map.
   *
   *  $parNote
   *
   *  $primitiveNote
   *
   *  @tparam K the key type of the map
   *  @param m the Scala Map whose values to stream over
   *  @return a parallel `DoubleStream` for the map's values
   */
  def asJavaParValueDoubleStreamFromFloat[K](m: collection.Map[K, jl.Float]):  DoubleStream = StreamSupport.doubleStream(m.valueStepper.spliterator, true)

  /** Creates a parallel [[java.util.stream.LongStream Java LongStream]] for the values of a Scala Map.
   *
   *  $parNote
   *
   *  $primitiveNote
   *
   *  @tparam K the key type of the map
   *  @param m the Scala Map whose values to stream over
   *  @return a parallel `LongStream` for the map's values
   */
  def asJavaParValueLongStream[K](m: collection.Map[K, jl.Long]): LongStream = StreamSupport.longStream(m.valueStepper.spliterator, true)
}
