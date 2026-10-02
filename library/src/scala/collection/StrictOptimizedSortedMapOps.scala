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

import scala.annotation.implicitNotFound

/** Trait that overrides sorted map operations to take advantage of strict builders.
 *
 *  @tparam K  Type of keys
 *  @tparam V  Type of values
 *  @tparam CC Collection type constructor
 *  @tparam C  Collection type
 */
transparent trait StrictOptimizedSortedMapOps[K, +V, +CC[X, Y] <: Map[X, Y] & SortedMapOps[X, Y, CC, ?], +C <: SortedMapOps[K, V, CC, C]]
  extends SortedMapOps[K, V, CC, C]
    with StrictOptimizedMapOps[K, V, Map, C] {

  /** Builds a new sorted map by applying a function to all entries of this map.
   *
   *  Overrides the default view-based implementation to fill a strict builder
   *  directly, which is slightly faster.
   *
   *  @tparam K2 the key type of the returned map
   *  @tparam V2 the value type of the returned map
   *  @param f the function to apply to each entry
   *  @param ordering the ordering of the resulting map's keys
   *  @return a new sorted map resulting from applying `f` to each entry of
   *          this map
   */
  override def map[K2, V2](f: ((K, V)) => (K2, V2))(implicit @implicitNotFound(SortedMapOps.ordMsg) ordering: Ordering[K2]): CC[K2, V2] =
    strictOptimizedMap(sortedMapFactory.newBuilder, f)

  /** Builds a new sorted map by applying a function to all entries of this map
   *  and concatenating the resulting collections of entries.
   *
   *  Overrides the default view-based implementation to fill a strict builder
   *  directly, which is slightly faster.
   *
   *  @tparam K2 the key type of the returned map
   *  @tparam V2 the value type of the returned map
   *  @param f the function to apply to each entry
   *  @param ordering the ordering of the resulting map's keys
   *  @return a new sorted map resulting from applying `f` to each entry of
   *          this map and adding all resulting entries
   */
  override def flatMap[K2, V2](f: ((K, V)) => IterableOnce[(K2, V2)]^)(implicit @implicitNotFound(SortedMapOps.ordMsg) ordering: Ordering[K2]): CC[K2, V2] =
    strictOptimizedFlatMap(sortedMapFactory.newBuilder, f)

  /** Returns a new sorted map containing the entries of this map followed by
   *  the entries of `xs`, using this map's key ordering.
   *
   *  Overrides the default view-based implementation to fill a strict builder
   *  directly. As with the default implementation, if a key occurs in both
   *  maps, the entry from `xs` wins.
   *
   *  @tparam V2 the value type of the returned map
   *  @param xs the entries to add
   *  @return a new sorted map containing all entries of this map and of `xs`
   */
  override def concat[V2 >: V](xs: IterableOnce[(K, V2)]^): CC[K, V2] =
    strictOptimizedConcat(xs, sortedMapFactory.newBuilder(using ordering))

  /** Builds a new sorted map by applying a partial function to all entries of
   *  this map on which the function is defined.
   *
   *  Overrides the default view-based implementation to fill a strict builder
   *  directly, using a single `applyOrElse` call per entry to test definedness
   *  and transform the entry at the same time.
   *
   *  @tparam K2 the key type of the returned map
   *  @tparam V2 the value type of the returned map
   *  @param pf the partial function to apply to each entry
   *  @param ordering the ordering of the resulting map's keys
   *  @return a new sorted map containing the results of applying `pf` to every
   *          entry of this map on which it is defined
   */
  override def collect[K2, V2](pf: PartialFunction[(K, V), (K2, V2)]^)(implicit @implicitNotFound(SortedMapOps.ordMsg) ordering: Ordering[K2]): CC[K2, V2] =
    strictOptimizedCollect(sortedMapFactory.newBuilder, pf)

  /** Returns a new sorted map containing the entries of this map together with
   *  two or more additional entries, using this map's key ordering.
   *
   *  Adds `elem1` and `elem2` with single-entry `+` calls, then concatenates
   *  `elems` only if it is non-empty. A later entry for a key overrides an
   *  earlier one.
   *
   *  @tparam V1 the value type of the returned map
   *  @param elem1 the first entry to add
   *  @param elem2 the second entry to add
   *  @param elems the remaining entries to add
   *  @return a new sorted map containing all entries of this map plus the
   *          given entries
   */
  @deprecated("Use ++ with an explicit collection argument instead of + with varargs", "2.13.0")
  override def + [V1 >: V](elem1: (K, V1), elem2: (K, V1), elems: (K, V1)*): CC[K, V1] = {
    val m = ((this + elem1).asInstanceOf[Map[K, V]] + elem2).asInstanceOf[CC[K, V1]]
    if(elems.isEmpty) m else m.concat(elems).asInstanceOf[CC[K, V1]]
  }
}
