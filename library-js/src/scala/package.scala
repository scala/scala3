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

import scala.annotation.migration

import scala.language.`2.13`

/**
 * Core Scala types. They are always available without an explicit import.
 * @contentDiagram hideNodes "scala.Serializable"
 */
package object scala {
  type Cloneable    = java.lang.Cloneable
  type Serializable = java.io.Serializable

  type Throwable = java.lang.Throwable
  type Exception = java.lang.Exception
  type Error     = java.lang.Error

  type RuntimeException                = java.lang.RuntimeException
  type NullPointerException            = java.lang.NullPointerException
  type ClassCastException              = java.lang.ClassCastException
  type IndexOutOfBoundsException       = java.lang.IndexOutOfBoundsException
  type ArrayIndexOutOfBoundsException  = java.lang.ArrayIndexOutOfBoundsException
  type StringIndexOutOfBoundsException = java.lang.StringIndexOutOfBoundsException
  type UnsupportedOperationException   = java.lang.UnsupportedOperationException
  type IllegalArgumentException        = java.lang.IllegalArgumentException
  type NoSuchElementException          = java.util.NoSuchElementException
  type NumberFormatException           = java.lang.NumberFormatException
  type AbstractMethodError             = java.lang.AbstractMethodError
  type InterruptedException            = java.lang.InterruptedException

  // A dummy used by the specialization annotation.
  /** A dummy [[Specializable]] instance standing for `AnyRef` in arguments to the `@specialized` annotation. */
  val AnyRef = new Specializable {
    override def toString = "object AnyRef"
  }

  @deprecated("Use IterableOnce instead of TraversableOnce", "2.13.0")
  type TraversableOnce[+A] = scala.collection.IterableOnce[A]

  type IterableOnce[+A] = scala.collection.IterableOnce[A]

  @deprecated("Use Iterable instead of Traversable", "2.13.0")
  type Traversable[+A] = scala.collection.Iterable[A]
  /** The [[scala.collection.Iterable]] companion object, under its old name `Traversable`. */
  @deprecated("Use Iterable instead of Traversable", "2.13.0")
  val Traversable = scala.collection.Iterable

  type Iterable[+A] = scala.collection.Iterable[A]
  /** The [[scala.collection.Iterable]] companion object. */
  val Iterable = scala.collection.Iterable

  @migration("scala.Seq is now scala.collection.immutable.Seq instead of scala.collection.Seq", "2.13.0")
  type Seq[+A] = scala.collection.immutable.Seq[A]
  /** The [[scala.collection.immutable.Seq]] companion object. */
  val Seq = scala.collection.immutable.Seq

  @migration("scala.IndexedSeq is now scala.collection.immutable.IndexedSeq instead of scala.collection.IndexedSeq", "2.13.0")
  type IndexedSeq[+A] = scala.collection.immutable.IndexedSeq[A]
  /** The [[scala.collection.immutable.IndexedSeq]] companion object. */
  val IndexedSeq = scala.collection.immutable.IndexedSeq

  type Iterator[+A] = scala.collection.Iterator[A]
  /** The [[scala.collection.Iterator]] companion object. */
  val Iterator = scala.collection.Iterator

  @deprecated("Use scala.collection.BufferedIterator instead of scala.BufferedIterator", "2.13.0")
  type BufferedIterator[+A] = scala.collection.BufferedIterator[A]

  type List[+A] = scala.collection.immutable.List[A]
  /** The [[scala.collection.immutable.List]] companion object. */
  val List = scala.collection.immutable.List

  /** The empty list, [[scala.collection.immutable.Nil]]. */
  val Nil = scala.collection.immutable.Nil

  type ::[A] = scala.collection.immutable.::[A]
  val :: = scala.collection.immutable.::

  val +: = scala.collection.+:
  val :+ = scala.collection.:+

  @deprecated("Use LazyList instead of Stream", "2.13.0")
  type Stream[+A] = scala.collection.immutable.Stream[A]
  /** The [[scala.collection.immutable.Stream]] companion object. */
  @deprecated("Use LazyList instead of Stream", "2.13.0")
  val Stream = scala.collection.immutable.Stream

  type LazyList[+A] = scala.collection.immutable.LazyList[A]
  /** The [[scala.collection.immutable.LazyList]] companion object. */
  val LazyList = scala.collection.immutable.LazyList
  // This should be an alias to LazyList.#:: but we need to support Stream, too
  //val #:: = scala.collection.immutable.LazyList.#::
  object #:: {
    /** Decomposes a non-empty lazy list into its head, which is evaluated, and
     *  its tail, enabling patterns of the form `case head #:: tail =>`.
     *
     *  @tparam A the element type of the lazy list
     *  @param s the lazy list to decompose
     *  @return `Some` of the head and tail of `s` if it is non-empty, `None` otherwise;
     *          obtaining the tail evaluates it, so matching forces one element beyond the head
     */
    def unapply[A](s: LazyList[A]): Option[(A, LazyList[A])] =
      if (s.nonEmpty) Some((s.head, s.tail)) else None
    /** Decomposes a non-empty stream into its head, which is evaluated, and
     *  its tail, enabling patterns of the form `case head #:: tail =>`.
     *
     *  @tparam A the element type of the stream
     *  @param s the stream to decompose
     *  @return `Some` of the head and tail of `s` if it is non-empty, `None` otherwise
     */
    def unapply[A](s: Stream[A]): Option[(A, Stream[A])] =
      if (s.nonEmpty) Some((s.head, s.tail)) else None
  }

  type Vector[+A] = scala.collection.immutable.Vector[A]
  /** The [[scala.collection.immutable.Vector]] companion object. */
  val Vector = scala.collection.immutable.Vector

  type StringBuilder = scala.collection.mutable.StringBuilder
  /** The [[scala.collection.mutable.StringBuilder]] companion object. */
  val StringBuilder = scala.collection.mutable.StringBuilder

  type Range = scala.collection.immutable.Range
  /** The [[scala.collection.immutable.Range]] companion object. */
  val Range = scala.collection.immutable.Range

  // Numeric types which were moved into scala.math.*

  type BigDecimal = scala.math.BigDecimal
  /** The [[scala.math.BigDecimal]] companion object. */
  lazy val BigDecimal = scala.math.BigDecimal

  type BigInt = scala.math.BigInt
  /** The [[scala.math.BigInt]] companion object. */
  lazy val BigInt = scala.math.BigInt

  type Equiv[T] = scala.math.Equiv[T]
  /** The [[scala.math.Equiv]] companion object. */
  val Equiv = scala.math.Equiv

  type Fractional[T] = scala.math.Fractional[T]
  /** The [[scala.math.Fractional]] companion object. */
  val Fractional = scala.math.Fractional

  type Integral[T] = scala.math.Integral[T]
  /** The [[scala.math.Integral]] companion object. */
  val Integral = scala.math.Integral

  type Numeric[T] = scala.math.Numeric[T]
  /** The [[scala.math.Numeric]] companion object. */
  val Numeric = scala.math.Numeric

  type Ordered[T] = scala.math.Ordered[T]
  /** The [[scala.math.Ordered]] companion object. */
  val Ordered = scala.math.Ordered

  type Ordering[T] = scala.math.Ordering[T]
  /** The [[scala.math.Ordering]] companion object. */
  val Ordering = scala.math.Ordering

  type PartialOrdering[T] = scala.math.PartialOrdering[T]
  type PartiallyOrdered[T] = scala.math.PartiallyOrdered[T]

  type Either[+A, +B] = scala.util.Either[A, B]
  /** The [[scala.util.Either]] companion object. */
  val Either = scala.util.Either

  type Left[+A, +B] = scala.util.Left[A, B]
  /** The [[scala.util.Left]] companion object. */
  val Left = scala.util.Left

  type Right[+A, +B] = scala.util.Right[A, B]
  /** The [[scala.util.Right]] companion object. */
  val Right = scala.util.Right

}
