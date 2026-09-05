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

/** Trait that overrides map operations to take advantage of strict builders.
 *
 *  @tparam K  Type of keys
 *  @tparam V  Type of values
 *  @tparam CC Collection type constructor
 *  @tparam C  Collection type
 */
transparent trait StrictOptimizedMapOps[K, +V, +CC[_, _] <: IterableOps[?, AnyConstr, ?], +C]
  extends MapOps[K, V, CC, C]
    with StrictOptimizedIterableOps[(K, V), Iterable, C] {

  /** Builds a new map by applying a function to all entries of this map.
   *
   *  Overrides the default view-based implementation to fill a strict builder
   *  directly, which is slightly faster.
   *
   *  @tparam K2 the key type of the returned map
   *  @tparam V2 the value type of the returned map
   *  @param f the function to apply to each entry
   *  @return a new map resulting from applying `f` to each entry of this map
   */
  override def map[K2, V2](f: ((K, V)) => (K2, V2)): CC[K2, V2] =
    strictOptimizedMap(mapFactory.newBuilder, f)

  /** Builds a new map by applying a function to all entries of this map and
   *  concatenating the resulting collections of entries.
   *
   *  Overrides the default view-based implementation to fill a strict builder
   *  directly, which is slightly faster.
   *
   *  @tparam K2 the key type of the returned map
   *  @tparam V2 the value type of the returned map
   *  @param f the function to apply to each entry
   *  @return a new map resulting from applying `f` to each entry of this map
   *          and adding all resulting entries
   */
  override def flatMap[K2, V2](f: ((K, V)) => IterableOnce[(K2, V2)]^): CC[K2, V2] =
    strictOptimizedFlatMap(mapFactory.newBuilder, f)

  /** Returns a new map containing the entries of this map followed by the
   *  entries of `suffix`.
   *
   *  Overrides the default view-based implementation to fill a strict builder
   *  directly. As with the default implementation, if a key occurs in both
   *  maps, the entry from `suffix` wins.
   *
   *  @tparam V2 the value type of the returned map
   *  @param suffix the entries to add
   *  @return a new map containing all entries of this map and of `suffix`
   */
  override def concat[V2 >: V](suffix: IterableOnce[(K, V2)]^): CC[K, V2] =
    strictOptimizedConcat(suffix, mapFactory.newBuilder)

  /** Builds a new map by applying a partial function to all entries of this
   *  map on which the function is defined.
   *
   *  Overrides the default view-based implementation to fill a strict builder
   *  directly, using a single `applyOrElse` call per entry to test definedness
   *  and transform the entry at the same time.
   *
   *  @tparam K2 the key type of the returned map
   *  @tparam V2 the value type of the returned map
   *  @param pf the partial function to apply to each entry
   *  @return a new map containing the results of applying `pf` to every entry
   *          of this map on which it is defined
   */
  override def collect[K2, V2](pf: PartialFunction[(K, V), (K2, V2)]^): CC[K2, V2] =
    strictOptimizedCollect(mapFactory.newBuilder, pf)

  /** Returns a new map containing the entries of this map together with two or
   *  more additional entries.
   *
   *  Overrides the default implementation to fill a strict builder directly.
   *  As with the default implementation, a later entry for a key overrides an
   *  earlier one.
   *
   *  @tparam V1 the value type of the returned map
   *  @param elem1 the first entry to add
   *  @param elem2 the second entry to add
   *  @param elems the remaining entries to add
   *  @return a new map containing all entries of this map plus the given entries
   */
  @deprecated("Use ++ with an explicit collection argument instead of + with varargs", "2.13.0")
  override def + [V1 >: V](elem1: (K, V1), elem2: (K, V1), elems: (K, V1)*): CC[K, V1] = {
    val b = mapFactory.newBuilder[K, V1]
    b ++= this
    b += elem1
    b += elem2
    if (elems.nonEmpty) b ++= elems
    b.result()
  }
}
