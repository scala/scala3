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

package scala
package collection
package immutable

import scala.language.`2.13`
import language.experimental.captureChecking

import scala.collection.generic.CommonErrors

/** Trait that overrides operations to take advantage of strict builders.
 *
 *  @tparam A the element type of the collection
 *  @tparam CC the type constructor of the collection (higher-kinded)
 *  @tparam C the type of the collection itself
 */
transparent trait StrictOptimizedSeqOps[+A, +CC[B] <: caps.Pure, +C]
  extends Any
    with SeqOps[A, CC, C]
    with collection.StrictOptimizedSeqOps[A, CC, C]
    with StrictOptimizedIterableOps[A, CC, C] {

  /** Selects all the elements of this sequence ignoring duplicates as
   *  determined by `==` after applying the transforming function `f`.
   *
   *  Fills a strict builder in a single traversal, keeping the first of each
   *  group of elements on which `f` agrees. Since this sequence is immutable it
   *  can be returned as is when it has at most one element, or when the traversal
   *  found no duplicate; in the latter case the builder was still filled, and its
   *  result is discarded.
   *
   *  @tparam B the type of the elements after being transformed by `f`
   *  @param f the transforming function whose result is used to determine the
   *           uniqueness of each element
   *  @return a sequence consisting of all the elements of this sequence
   *          without duplicates, or this sequence itself if it has none
   */
  override def distinctBy[B](f: A -> B): C = {
    if (lengthCompare(1) <= 0) coll
    else {
      val builder = newSpecificBuilder
      val seen = mutable.HashSet.empty[B]
      val it = this.iterator
      var different = false
      while (it.hasNext) {
        val next = it.next()
        if (seen.add(f(next))) builder += next else different = true
      }
      if (different) builder.result() else coll
    }
  }

  /** Returns a copy of this sequence with the element at `index` replaced by `elem`.
   *
   *  Fills a size-hinted strict builder in a single traversal rather than
   *  building a view.
   *
   *  @tparam B the element type of the returned sequence
   *  @param index the position of the element to replace
   *  @param elem the replacing element
   *  @return a new sequence that agrees with this sequence everywhere except at
   *          `index`, where it holds `elem`
   *  @throws IndexOutOfBoundsException if `index` is negative or not less than the length of this sequence
   */
  override def updated[B >: A](index: Int, elem: B): CC[B] = {
    if (index < 0)
      throw (
        if (knownSize >= 0) CommonErrors.indexOutOfBounds(index = index, max = knownSize)
        else CommonErrors.indexOutOfBounds(index = index)
      )
    val b = iterableFactory.newBuilder[B]
    b.sizeHint(this)
    var i = 0
    val it = iterator
    while (i < index && it.hasNext) {
      b += it.next()
      i += 1
    }
    if (!it.hasNext)
      throw CommonErrors.indexOutOfBounds(index = index, max = i - 1)
    b += elem
    it.next()
    while (it.hasNext) b += it.next()
    b.result()
  }

  /** Returns a copy of this sequence with `replaced` elements from `from` onwards
   *  replaced by the elements of `other`.
   *
   *  Fills a strict builder in a single traversal rather than building a view.
   *  A negative `from` is treated as `0` and a `from` beyond the end of this
   *  sequence appends `other` at the end; a negative `replaced` removes nothing,
   *  and one larger than the number of remaining elements removes all of them.
   *
   *  @tparam B the element type of the returned sequence
   *  @param from the index of the first replaced element
   *  @param other the replacement elements
   *  @param replaced the number of elements to drop from this sequence at `from`
   *  @return a new sequence consisting of the elements of this sequence before
   *          `from`, then the elements of `other`, then the elements of this
   *          sequence from `from + replaced` onwards, where a negative `from`
   *          counts as 0 and a negative `replaced` as 0
   */
  override def patch[B >: A](from: Int, other: IterableOnce[B]^, replaced: Int): CC[B] = {
    val b = iterableFactory.newBuilder[B]
    var i = 0
    val it = iterator
    while (i < from && it.hasNext) {
      b += it.next()
      i += 1
    }
    b ++= other
    i = replaced
    while (i > 0 && it.hasNext) {
      it.next()
      i -= 1
    }
    while (it.hasNext) b += it.next()
    b.result()
  }

  /** Returns the elements of this sequence sorted according to `ord`.
   *
   *  Forwards to the inherited implementation, which sorts stably; the override
   *  serves only to refine the result type for immutable sequences.
   *
   *  @tparam B the element type used for ordering (a supertype of `A`)
   *  @param ord the ordering used to compare elements
   *  @return a sequence with the same elements as this sequence, ordered by `ord`
   */
  override def sorted[B >: A](implicit ord: Ordering[B]): C = super.sorted(using ord)

}
