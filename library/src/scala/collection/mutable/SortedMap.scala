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
package collection.mutable

import scala.language.`2.13`
import language.experimental.captureChecking
import scala.collection.{SortedMapFactory, SortedMapFactoryDefaults}

/** Base type for mutable sorted map collections
 *
 *  @tparam K the type of the keys in this sorted map; an implicit `Ordering[K]` is required for most operations
 *  @tparam V the type of the values associated with the keys
 */
trait SortedMap[K, V]
  extends collection.SortedMap[K, V]
    with Map[K, V]
    with SortedMapOps[K, V, SortedMap, SortedMap[K, V]]
    with SortedMapFactoryDefaults[K, V, SortedMap, Iterable, Map] {

  /** Returns this map itself, with its static type widened to its unsorted counterpart `Map` */
  override def unsorted: Map[K, V] = this

  /** Returns the [[SortedMap]] object, the default factory for mutable sorted maps, which creates `TreeMap`s */
  override def sortedMapFactory: SortedMapFactory[SortedMap] = SortedMap

  /** The same sorted map with a given default function.
   *  Note: The default is only used for `apply`. Other methods like `get`, `contains`, `iterator`, `keys`, etc.
   *  are not affected by `withDefault`.
   *
   *  Invoking transformer methods (e.g. `map`) will not preserve the default value.
   *
   *  @param d     the function mapping keys to values, used for non-present keys
   *  @return      a wrapper of the map with a default value
   */
  override def withDefault(d: K -> V): SortedMap[K, V] = new SortedMap.WithDefault[K, V](this, d)

  /** The same map with a given default value.
   *  Note: The default is only used for `apply`. Other methods like `get`, `contains`, `iterator`, `keys`, etc.
   *  are not affected by `withDefaultValue`.
   *
   *  Invoking transformer methods (e.g. `map`) will not preserve the default value.
   *
   *  @param d default value used for non-present keys
   *  @return a wrapper of the map with a default value
   */
  override def withDefaultValue(d: V): SortedMap[K, V] = new SortedMap.WithDefault[K, V](this, _ => d)
}

transparent trait SortedMapOps[K, V, +CC[X, Y] <: Map[X, Y] & SortedMapOps[X, Y, CC, ?], +C <: SortedMapOps[K, V, CC, C]]
  extends collection.SortedMapOps[K, V, CC, C]
    with MapOps[K, V, Map, C] {

  /** Widens the type of this map to its unsorted counterpart. */
  def unsorted: Map[K, V]

  /** Returns a copy of this map, created with `clone()`, in which `value` is associated with `key`; this map itself
   *  is unchanged.
   *
   *  @tparam V1 the type of the added value, a supertype of `V`
   *  @param key the key to associate the value with
   *  @param value the value to associate with `key`
   *  @return a mutable copy of this map with `key` bound to `value`
   */
  @deprecated("Use m.clone().addOne((k,v)) instead of m.updated(k, v)", "2.13.0")
  override def updated[V1 >: V](key: K, value: V1): CC[K, V1] =
    clone().asInstanceOf[CC[K, V1]].addOne((key, value))
}

@SerialVersionUID(3L)
object SortedMap extends SortedMapFactory.Delegate[SortedMap](TreeMap) {

  /** A mutable sorted map whose `apply` returns `defaultValue(key)` for keys that are not present instead of
   *  throwing an exception. All operations are delegated to the underlying sorted map; methods such as `get`,
   *  `contains`, `iterator`, and `keys` are not affected by the default.
   *
   *  @tparam K the type of the keys
   *  @tparam V the type of the values
   *  @param underlying the sorted map to which all operations are delegated
   *  @param defaultValue the function computing a default value for keys that are not present
   */
  @SerialVersionUID(3L)
  final class WithDefault[K, V](underlying: SortedMap[K, V], defaultValue: K -> V)
    extends Map.WithDefault[K, V](underlying, defaultValue)
      with SortedMap[K, V]
      with SortedMapOps[K, V, SortedMap, WithDefault[K, V]]
      with Serializable {

    /** Returns the sorted map factory of the underlying map; maps created by that factory do not have this map's default */
    override def sortedMapFactory: SortedMapFactory[SortedMap] = underlying.sortedMapFactory

    /** Returns an iterator over the key-value pairs of the underlying map whose keys are greater than or equal to
     *  `start`, in the map's key ordering.
     *
     *  @param start the lower bound (inclusive) on the keys of the entries to return
     */
    def iteratorFrom(start: K): scala.collection.Iterator[(K, V)] = underlying.iteratorFrom(start)

    /** Returns an iterator over the keys of the underlying map that are greater than or equal to `start`, in the
     *  map's key ordering.
     *
     *  @param start the lower bound (inclusive) on the keys to return
     */
    def keysIteratorFrom(start: K): scala.collection.Iterator[K] = underlying.keysIteratorFrom(start)

    /** Returns the key ordering of the underlying sorted map */
    implicit def ordering: Ordering[K] = underlying.ordering

    /** Returns a ranged projection of the underlying map, wrapped in a new `WithDefault` with the same default
     *  function.
     *
     *  @param from the lower bound (inclusive) of the projection wrapped in a `Some`, or `None` if there is no lower bound
     *  @param until the upper bound (exclusive) of the projection wrapped in a `Some`, or `None` if there is no upper bound
     *  @return a `WithDefault` wrapper around the ranged projection of the underlying map
     */
    def rangeImpl(from: Option[K], until: Option[K]): WithDefault[K, V] =
      new WithDefault[K, V](underlying.rangeImpl(from, until), defaultValue)

    // Need to override following methods to match type signatures of `SortedMap.WithDefault`
    // for operations preserving default value
    /** Removes the entry with key `elem` from the underlying map, if one exists.
     *
     *  @param elem the key of the entry to remove
     *  @return this map, with its default function intact
     */
    override def subtractOne(elem: K): WithDefault.this.type = { underlying.subtractOne(elem); this }

    /** Adds the key-value pair `elem` to the underlying map, replacing the value of an existing entry with an equal
     *  key.
     *
     *  @param elem the key-value pair to add
     *  @return this map, with its default function intact
     */
    override def addOne(elem: (K, V)): WithDefault.this.type = { underlying.addOne(elem); this }

    /** Returns a new `WithDefault` with the same default function, wrapping an empty map of the same type as the underlying map */
    override def empty: WithDefault[K, V] = new WithDefault[K, V](underlying.empty, defaultValue)

    /** Returns a new sorted map containing the entries of the underlying map followed by those of `suffix`, with
     *  the same default function as this map.
     *
     *  @tparam V2 the type of the values of the resulting map, a supertype of `V`
     *  @param suffix the key-value pairs to append
     *  @return a new sorted map with the combined entries and this map's default function
     */
    override def concat[V2 >: V](suffix: collection.IterableOnce[(K, V2)]^): SortedMap[K, V2] =
      underlying.concat(suffix).withDefault(defaultValue)

    /** Returns a new `WithDefault` with the same default function, wrapping a map that is built from the key-value
     *  pairs of `coll` with the underlying map's factory.
     *
     *  @param coll the key-value pairs of the new map
     *  @return a new `WithDefault` containing the entries of `coll`
     */
    override protected def fromSpecific(coll: scala.collection.IterableOnce[(K, V)]^): WithDefault[K, V] =
      new WithDefault[K, V](sortedMapFactory.from(coll), defaultValue)

    /** Returns a builder that collects key-value pairs into a new sorted map and wraps the result in a `WithDefault` with the same default function */
    override protected def newSpecificBuilder: Builder[(K, V), WithDefault[K, V]] =
      SortedMap.newBuilder.mapResult((p: SortedMap[K, V]) => new WithDefault[K, V](p, defaultValue))
  }
}
