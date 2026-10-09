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

import scala.language.`2.13`
import language.experimental.captureChecking

import scala.annotation.implicitNotFound
import scala.annotation.unchecked.uncheckedVariance

/** Trait that overrides sorted set operations to take advantage of strict builders.
 *
 *  @tparam A  Elements type
 *  @tparam CC Collection type constructor
 *  @tparam C  Collection type
 */
transparent trait StrictOptimizedSortedSetOps[A, +CC[X] <: SortedSet[X], +C <: SortedSetOps[A, CC, C]]
  extends SortedSetOps[A, CC, C]
    with StrictOptimizedSetOps[A, Set, C] {

  /** Builds a new sorted set by applying a function to all elements of this
   *  set.
   *
   *  Overrides the default view-based implementation to fill a strict builder
   *  directly, which is slightly faster.
   *
   *  @tparam B the element type of the returned set
   *  @param f the function to apply to each element
   *  @param ev the ordering of the resulting set's elements
   *  @return a new sorted set containing the results of applying `f` to every
   *          element of this set
   */
  override def map[B](f: A => B)(implicit @implicitNotFound(SortedSetOps.ordMsg) ev: Ordering[B]): CC[B] =
    strictOptimizedMap(sortedIterableFactory.newBuilder, f)

  /** Builds a new sorted set by applying a function to all elements of this
   *  set and concatenating the results.
   *
   *  Overrides the default view-based implementation to fill a strict builder
   *  directly, which is slightly faster.
   *
   *  @tparam B the element type of the returned set
   *  @param f the function to apply to each element
   *  @param ev the ordering of the resulting set's elements
   *  @return a new sorted set containing the concatenated results of applying
   *          `f` to every element of this set
   */
  override def flatMap[B](f: A => IterableOnce[B]^)(implicit @implicitNotFound(SortedSetOps.ordMsg) ev: Ordering[B]): CC[B] =
    strictOptimizedFlatMap(sortedIterableFactory.newBuilder, f)

  /** Returns a new sorted set of pairs formed from this set and another
   *  iterable collection by combining corresponding elements.
   *
   *  Overrides the default view-based implementation to fill a strict builder
   *  directly. As with the default implementation, if one of the two
   *  collections is longer than the other, its remaining elements are ignored.
   *
   *  @tparam B the type of the second half of the returned pairs
   *  @param that the iterable providing the second half of each result pair
   *  @param ev the ordering of the resulting set's pair elements
   *  @return a new sorted set containing pairs consisting of corresponding
   *          elements of this set and `that`
   */
  override def zip[B](that: IterableOnce[B]^)(implicit @implicitNotFound(SortedSetOps.zipOrdMsg) ev: Ordering[(A @uncheckedVariance, B)]): CC[(A @uncheckedVariance, B)] =
    strictOptimizedZip(that, sortedIterableFactory.newBuilder[(A, B)])

  /** Builds a new sorted set by applying a partial function to all elements of
   *  this set on which the function is defined.
   *
   *  Overrides the default view-based implementation to fill a strict builder
   *  directly, using a single `applyOrElse` call per element to test
   *  definedness and transform the element at the same time.
   *
   *  @tparam B the element type of the returned set
   *  @param pf the partial function to apply to each element
   *  @param ev the ordering of the resulting set's elements
   *  @return a new sorted set containing the results of applying `pf` to every
   *          element of this set on which it is defined
   */
  override def collect[B](pf: PartialFunction[A, B]^)(implicit @implicitNotFound(SortedSetOps.ordMsg) ev: Ordering[B]): CC[B] =
    strictOptimizedCollect(sortedIterableFactory.newBuilder, pf)

}
