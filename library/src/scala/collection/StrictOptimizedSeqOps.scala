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

/** Trait that overrides operations on sequences in order
 *  to take advantage of strict builders.
 *
 *  @tparam A the element type of the sequence
 *  @tparam CC the type constructor of the collection
 *  @tparam C the type of the sequence itself
 */
transparent trait StrictOptimizedSeqOps [+A, +CC[_] <: caps.Pure, +C]
  extends Any with SeqOps[A, CC, C] with StrictOptimizedIterableOps[A, CC, C] with caps.Pure {

  /** Selects all the elements of this sequence ignoring duplicates as
   *  determined by `==` after applying the transforming function `f`.
   *
   *  Overrides the default view-based implementation to fill a strict builder
   *  in a single traversal, keeping the first of each group of elements on
   *  which `f` agrees.
   *
   *  @tparam B the type of the elements after being transformed by `f`
   *  @param f the transforming function whose result is used to determine the
   *           uniqueness of each element
   *  @return a new sequence consisting of all the elements of this sequence
   *          without duplicates
   */
  override def distinctBy[B](f: A -> B): C = {
    val builder = newSpecificBuilder
    val seen = mutable.HashSet.empty[B]
    val it = this.iterator
    while (it.hasNext) {
      val next = it.next()
      if (seen.add(f(next))) builder += next
    }
    builder.result()
  }

  /** Returns a new sequence consisting of `elem` followed by the elements of
   *  this sequence.
   *
   *  Overrides the default view-based implementation to fill a size-hinted
   *  strict builder directly.
   *
   *  @tparam B the element type of the returned sequence
   *  @param elem the prepended element
   *  @return a new sequence consisting of `elem` followed by all elements of
   *          this sequence
   */
  override def prepended[B >: A](elem: B): CC[B] = {
    val b = iterableFactory.newBuilder[B]
    b.sizeHint(this, delta = 1)
    b += elem
    b ++= this
    b.result()
  }

  /** Returns a new sequence consisting of the elements of this sequence
   *  followed by `elem`.
   *
   *  Overrides the default view-based implementation to fill a size-hinted
   *  strict builder directly.
   *
   *  @tparam B the element type of the returned sequence
   *  @param elem the appended element
   *  @return a new sequence consisting of all elements of this sequence
   *          followed by `elem`
   */
  override def appended[B >: A](elem: B): CC[B] = {
    val b = iterableFactory.newBuilder[B]
    b.sizeHint(this, delta = 1)
    b ++= this
    b += elem
    b.result()
  }

  /** Returns a new sequence consisting of the elements of this sequence
   *  followed by the elements of `suffix`.
   *
   *  Overrides the default view-based implementation to fill a strict builder
   *  directly.
   *
   *  @tparam B the element type of the returned sequence
   *  @param suffix the iterable to append
   *  @return a new sequence consisting of all elements of this sequence
   *          followed by all elements of `suffix`
   */
  override def appendedAll[B >: A](suffix: IterableOnce[B]^): CC[B] =
    strictOptimizedConcat(suffix, iterableFactory.newBuilder)

  /** Returns a new sequence consisting of the elements of `prefix` followed by
   *  the elements of this sequence.
   *
   *  Overrides the default view-based implementation to fill a strict builder
   *  directly.
   *
   *  @tparam B the element type of the returned sequence
   *  @param prefix the iterable to prepend
   *  @return a new sequence consisting of all elements of `prefix` followed by
   *          all elements of this sequence
   */
  override def prependedAll[B >: A](prefix: IterableOnce[B]^): CC[B] = {
    val b = iterableFactory.newBuilder[B]
    b ++= prefix
    b ++= this
    b.result()
  }

  /** Returns a copy of this sequence padded at the end with `elem` up to
   *  length `len`.
   *
   *  Overrides the default view-based implementation to fill a size-hinted
   *  strict builder directly.
   *
   *  @tparam B the element type of the returned sequence
   *  @param len the target length of the returned sequence
   *  @param elem the padding value
   *  @return a new sequence consisting of all elements of this sequence
   *          followed by the minimal number of occurrences of `elem` so that
   *          the resulting sequence has length at least `len`; if this
   *          sequence already has at least `len` elements, no padding is added
   */
  override def padTo[B >: A](len: Int, elem: B): CC[B] = {
    val b = iterableFactory.newBuilder[B]
    val L = size
    b.sizeHint(math.max(L, len))
    var diff = len - L
    b ++= this
    while (diff > 0) {
      b += elem
      diff -= 1
    }
    b.result()
  }

  /** Computes the multiset difference between this sequence and another
   *  sequence.
   *
   *  Overrides the default implementation to fill a strict builder in a single
   *  traversal of this sequence, using a map of occurrence counts of `that`.
   *  If either sequence is empty, this sequence is returned as is, without
   *  building a copy.
   *
   *  @tparam B the element type of `that`
   *  @param that the sequence of elements to remove
   *  @return a sequence containing all elements of this sequence except that
   *          each occurrence of an element in `that` cancels out one
   *          occurrence of an equal element of this sequence
   */
  override def diff[B >: A](that: Seq[B]): C =
    if (isEmpty || that.isEmpty) coll
    else {
      val occ = occCounts(that)
      val b = newSpecificBuilder
      for (x <- this) {
        occ.updateWith(x) {
          case None => {
            b.addOne(x)
            None
          }
          case Some(1) => None
          case Some(n) => Some(n - 1)
        }
      }
      b.result()
    }

  /** Computes the multiset intersection between this sequence and another
   *  sequence.
   *
   *  Overrides the default implementation to fill a strict builder in a single
   *  traversal of this sequence, using a map of occurrence counts of `that`.
   *  If either sequence is empty, the empty sequence is returned directly,
   *  without building.
   *
   *  @tparam B the element type of `that`
   *  @param that the sequence of elements to intersect with
   *  @return a sequence containing the elements of this sequence that also
   *          occur in `that`, where each occurrence in `that` accounts for at
   *          most one equal element of this sequence, in the order they appear
   *          in this sequence
   */
  override def intersect[B >: A](that: Seq[B]): C =
    if (isEmpty || that.isEmpty) empty
    else {
      val occ = occCounts(that)
      val b = newSpecificBuilder
      for (x <- this) {
        occ.updateWith(x) {
          case None => None
          case Some(n) => {
            b.addOne(x)
            if (n == 1) None else Some(n - 1)
          }
        }
      }
      b.result()
    }
}
