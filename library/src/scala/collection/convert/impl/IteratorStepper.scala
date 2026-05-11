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
  protected def semiclone(): AnyIteratorStepper[A] = new AnyIteratorStepper(null)

  def nextStep(): A = if (proxied ne null) proxied.nextStep() else underlying.nn.next()

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
  protected def semiclone(): DoubleIteratorStepper = new DoubleIteratorStepper(null)

  def nextStep(): Double = if (proxied ne null) proxied.nextStep() else underlying.nn.next()

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
  protected def semiclone(): IntIteratorStepper = new IntIteratorStepper(null)

  def nextStep(): Int = if (proxied ne null) proxied.nextStep() else underlying.nn.next()

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
  protected def semiclone(): LongIteratorStepper = new LongIteratorStepper(null)

  def nextStep(): Long = if (proxied ne null) proxied.nextStep() else underlying.nn.next()

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
 */
private[convert] abstract class IteratorStepperBase[A, SP <: Stepper[A], Semi <: SP](final protected val underlying: Iterator[A] | Null) {
  final protected var nextChunkSize = 16
  @annotation.stableNull
  final protected var proxied: SP | Null = null
  protected def semiclone(): Semi        // Must initialize with null iterator!
  def characteristics: Int = if (proxied ne null) Spliterator.ORDERED | Spliterator.SIZED | Spliterator.SUBSIZED else Spliterator.ORDERED
  def estimateSize: Long = if (proxied ne null) proxied.estimateSize else Long.MaxValue
  def hasStep: Boolean = if (proxied ne null) proxied.hasStep else underlying.nn.hasNext
}
