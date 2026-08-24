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
package impl

import scala.language.`2.13`
import java.util.Spliterator

import scala.collection.{AnyStepper, DoubleStepper, IntStepper, LongStepper, Stepper}
import scala.jdk.{AnyAccumulator, DoubleAccumulator, IntAccumulator, LongAccumulator}

private[collection] class AnyIteratorStepper[A](_underlying: Iterator[A] | Null)
  extends IteratorStepperBase[A, AnyStepper[A], AnyIteratorStepper[A]](_underlying)
    with AnyStepper[A] {
  /** Returns a new, empty `AnyIteratorStepper` with a `null` iterator, for `trySplit`
   *  to populate via its `proxied` field.
   */
  protected def semiclone(): AnyIteratorStepper[A] = new AnyIteratorStepper(null)

  /** Returns the next element, taken from the proxy stepper if one is set, otherwise
   *  from the underlying iterator.
   *
   *  @throws NoSuchElementException if no elements remain
   */
  def nextStep(): A = if (proxied ne null) proxied.nextStep() else underlying.nn.next()

  /** Splits this stepper by copying up to a chunk of elements from the iterator into an
   *  [[scala.jdk.AnyAccumulator]]: if the iterator still has elements afterwards, returns
   *  a new stepper proxying the accumulated chunk and grows the chunk size for the next
   *  split; if the copy exhausts the iterator, this stepper becomes a proxy for the
   *  accumulator's stepper and splits that instead.  A stepper already backed by a proxy
   *  splits the proxy.
   *
   *  @return a stepper over a prefix of the remaining elements, or `null` if too few
   *          elements remain to split
   */
  def trySplit(): AnyStepper[A] | Null = if (proxied ne null) proxied.trySplit() else {
    val acc = new AnyAccumulator[A]
    var i = 0
    val n = nextChunkSize & 0xFFFFFFFC
    while (i < n && underlying.nn.hasNext) { acc += underlying.nn.next(); i += 1 }
    if (i < n || !underlying.nn.hasNext) {
      proxied = acc.stepper
      proxied.trySplit()
    }
    else {
      val ans = semiclone()
      ans.proxied = acc.stepper
      nextChunkSize = if ((nextChunkSize&3) == 3) { if (n < 0x40000000) n*2 else n } else nextChunkSize + 1
      ans
    }
  }
}

private[collection] class DoubleIteratorStepper(_underlying: Iterator[Double] | Null)
  extends IteratorStepperBase[Double, DoubleStepper, DoubleIteratorStepper](_underlying)
    with DoubleStepper {
  /** Returns a new, empty `DoubleIteratorStepper` with a `null` iterator, for `trySplit`
   *  to populate via its `proxied` field.
   */
  protected def semiclone(): DoubleIteratorStepper = new DoubleIteratorStepper(null)

  /** Returns the next element, taken from the proxy stepper if one is set, otherwise
   *  from the underlying iterator.
   *
   *  @throws NoSuchElementException if no elements remain
   */
  def nextStep(): Double = if (proxied ne null) proxied.nextStep() else underlying.nn.next()

  /** Splits this stepper by copying up to a chunk of elements from the iterator into a
   *  [[scala.jdk.DoubleAccumulator]]: if the iterator still has elements afterwards, returns
   *  a new stepper proxying the accumulated chunk and grows the chunk size for the next
   *  split; if the copy exhausts the iterator, this stepper becomes a proxy for the
   *  accumulator's stepper and splits that instead.  A stepper already backed by a proxy
   *  splits the proxy.
   *
   *  @return a stepper over a prefix of the remaining elements, or `null` if too few
   *          elements remain to split
   */
  def trySplit(): DoubleStepper | Null = if (proxied ne null) proxied.trySplit() else {
    val acc = new DoubleAccumulator
    var i = 0
    val n = nextChunkSize & 0xFFFFFFFC
    while (i < n && underlying.nn.hasNext) { acc += underlying.nn.next(); i += 1 }
    if (i < n || !underlying.nn.hasNext) {
      proxied = acc.stepper
      proxied.trySplit()
    }
    else {
      val ans = semiclone()
      ans.proxied = acc.stepper
      nextChunkSize = if ((nextChunkSize&3) == 3) { if (n < 0x40000000) n*2 else n } else nextChunkSize + 1
      ans
    }
  }
}

private[collection] class IntIteratorStepper(_underlying: Iterator[Int] | Null)
  extends IteratorStepperBase[Int, IntStepper, IntIteratorStepper](_underlying)
    with IntStepper {
  /** Returns a new, empty `IntIteratorStepper` with a `null` iterator, for `trySplit`
   *  to populate via its `proxied` field.
   */
  protected def semiclone(): IntIteratorStepper = new IntIteratorStepper(null)

  /** Returns the next element, taken from the proxy stepper if one is set, otherwise
   *  from the underlying iterator.
   *
   *  @throws NoSuchElementException if no elements remain
   */
  def nextStep(): Int = if (proxied ne null) proxied.nextStep() else underlying.nn.next()

  /** Splits this stepper by copying up to a chunk of elements from the iterator into an
   *  [[scala.jdk.IntAccumulator]]: if the iterator still has elements afterwards, returns
   *  a new stepper proxying the accumulated chunk and grows the chunk size for the next
   *  split; if the copy exhausts the iterator, this stepper becomes a proxy for the
   *  accumulator's stepper and splits that instead.  A stepper already backed by a proxy
   *  splits the proxy.
   *
   *  @return a stepper over a prefix of the remaining elements, or `null` if too few
   *          elements remain to split
   */
  def trySplit(): IntStepper | Null = if (proxied ne null) proxied.trySplit() else {
    val acc = new IntAccumulator
    var i = 0
    val n = nextChunkSize & 0xFFFFFFFC
    while (i < n && underlying.nn.hasNext) { acc += underlying.nn.next(); i += 1 }
    if (i < n || !underlying.nn.hasNext) {
      proxied = acc.stepper
      proxied.trySplit()
    }
    else {
      val ans = semiclone()
      ans.proxied = acc.stepper
      nextChunkSize = if ((nextChunkSize&3) == 3) { if (n < 0x40000000) n*2 else n } else nextChunkSize + 1
      ans
    }
  }
}

private[collection] class LongIteratorStepper(_underlying: Iterator[Long] | Null)
  extends IteratorStepperBase[Long, LongStepper, LongIteratorStepper](_underlying)
    with LongStepper {
  /** Returns a new, empty `LongIteratorStepper` with a `null` iterator, for `trySplit`
   *  to populate via its `proxied` field.
   */
  protected def semiclone(): LongIteratorStepper = new LongIteratorStepper(null)

  /** Returns the next element, taken from the proxy stepper if one is set, otherwise
   *  from the underlying iterator.
   *
   *  @throws NoSuchElementException if no elements remain
   */
  def nextStep(): Long = if (proxied ne null) proxied.nextStep() else underlying.nn.next()

  /** Splits this stepper by copying up to a chunk of elements from the iterator into a
   *  [[scala.jdk.LongAccumulator]]: if the iterator still has elements afterwards, returns
   *  a new stepper proxying the accumulated chunk and grows the chunk size for the next
   *  split; if the copy exhausts the iterator, this stepper becomes a proxy for the
   *  accumulator's stepper and splits that instead.  A stepper already backed by a proxy
   *  splits the proxy.
   *
   *  @return a stepper over a prefix of the remaining elements, or `null` if too few
   *          elements remain to split
   */
  def trySplit(): LongStepper | Null = if (proxied ne null) proxied.trySplit() else {
    val acc = new LongAccumulator
    var i = 0
    val n = nextChunkSize & 0xFFFFFFFC
    while (i < n && underlying.nn.hasNext) { acc += underlying.nn.next(); i += 1 }
    if (i < n || !underlying.nn.hasNext) {
      proxied = acc.stepper
      proxied.trySplit()
    }
    else {
      val ans = semiclone()
      ans.proxied = acc.stepper
      nextChunkSize = if ((nextChunkSize&3) == 3) { if (n < 0x40000000) n*2 else n } else nextChunkSize + 1
      ans
    }
  }
}

/** Common functionality for Steppers that step through an Iterator, caching the results as needed when a split is requested.
 *
 *  @tparam A the element type of the iterator being stepped through
 *  @tparam SP the specific `Stepper` subtype, bounded by `Stepper[A]`, used for proxied delegation and split results
 *  @tparam Semi the concrete stepper subtype returned by `semiclone()`, must extend `SP`
 *  @param underlying the source `Iterator` to step through, or `null` for steppers created via `semiclone()` during a split, which delegate to `proxied` instead
 */
private[convert] abstract class IteratorStepperBase[A, SP <: Stepper[A], Semi <: SP](final protected val underlying: Iterator[A] | Null) {
  /** Controls how many elements are loaded into an accumulator on the next split: the
   *  upper bits hold the chunk size (initially 16) and the low two bits count splits, so
   *  that the chunk size doubles on every fourth split, up to `2^30^`.
   */
  final protected var nextChunkSize = 16
  /** A stepper over elements copied out of the iterator into an accumulator: set on a
   *  split-off prefix stepper, and on this stepper once a split exhausts the iterator.
   *  When non-null, all operations delegate to it.
   */
  @annotation.stableNull
  final protected var proxied: SP | Null = null
  /** Creates a new, empty stepper of the concrete type with a `null` iterator; `trySplit`
   *  stores an accumulated chunk of elements in its `proxied` field.
   *
   *  @return the new, empty stepper
   */
  protected def semiclone(): Semi        // Must initialize with null iterator!
  /** Returns the Java `Spliterator` characteristics: `ORDERED`, plus `SIZED` and
   *  `SUBSIZED` once this stepper delegates to an accumulator of known size.
   */
  def characteristics: Int = if (proxied ne null) Spliterator.ORDERED | Spliterator.SIZED | Spliterator.SUBSIZED else Spliterator.ORDERED
  /** Returns the exact number of elements remaining when this stepper delegates to an
   *  accumulator, otherwise `Long.MaxValue` to signal an unknown remaining count.
   */
  def estimateSize: Long = if (proxied ne null) proxied.estimateSize else Long.MaxValue
  /** Returns `true` if more elements remain, consulting the proxy stepper if one is set,
   *  otherwise the underlying iterator.
   */
  def hasStep: Boolean = if (proxied ne null) proxied.hasStep else underlying.nn.hasNext
}
