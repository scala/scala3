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

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable.Builder

/** An immutable map whose key-value pairs are sorted according to an [[scala.math.Ordering]] on the keys.
 *
 *  Allows for range queries to be performed on its keys, and implementations must guarantee that traversal happens in
 *  sorted order, according to the map's [[scala.math.Ordering]].
 *
 *  @example ```scala sc:compile
 *  import scala.collection.immutable.SortedMap
 *
 *  // Make a SortedMap via the companion object factory
 *  val weekdays = SortedMap(
 *    2 -> "Monday",
 *    3 -> "Tuesday",
 *    4 -> "Wednesday",
 *    5 -> "Thursday",
 *    6 -> "Friday"
 *  )
 *  // TreeMap(2 -> Monday, 3 -> Tuesday, 4 -> Wednesday, 5 -> Thursday, 6 -> Friday)
 *
 *  val days = weekdays ++ List(1 -> "Sunday", 7 -> "Saturday")
 *  // TreeMap(1 -> Sunday, 2 -> Monday, 3 -> Tuesday, 4 -> Wednesday, 5 -> Thursday, 6 -> Friday, 7 -> Saturday)
 *
 *  val day3 = days.get(3) // Some("Tuesday")
 *
 *  val rangeOfDays = days.range(2, 5) // TreeMap(2 -> Monday, 3 -> Tuesday, 4 -> Wednesday)
 *
 *  val daysUntil2 = days.rangeUntil(2) // TreeMap(1 -> Sunday)
 *  val daysTo2 = days.rangeTo(2) // TreeMap(1 -> Sunday, 2 -> Monday)
 *  val daysAfter5 = days.rangeFrom(5) //  TreeMap(5 -> Thursday, 6 -> Friday, 7 -> Saturday)
 *  ```
 *
 *  @tparam K the type of the keys contained in this tree map.
 *  @tparam V the type of the values associated with the keys.
 */
trait SortedMap[K, +V]
  extends Map[K, V]
    with collection.SortedMap[K, V]
    with SortedMapOps[K, V, SortedMap, SortedMap[K, V]]
    with SortedMapFactoryDefaults[K, V, SortedMap, Iterable, Map] {

  /** Returns this map itself, with its static type widened to its unsorted counterpart `Map` */
  override def unsorted: Map[K, V] = this

  /** Returns the [[SortedMap$ `SortedMap`]] object, the default factory for immutable sorted maps, which creates `TreeMap`s */
  override def sortedMapFactory: SortedMapFactory[SortedMap] = SortedMap

  /** The same map with a given default function.
   *  Note: The default is only used for `apply`. Other methods like `get`, `contains`, `iterator`, `keys`, etc.
   *  are not affected by `withDefault`.
   *
   *  Invoking transformer methods (e.g. `map`) will not preserve the default value.
   *
   *  @tparam V1 the type of the values in the resulting map, a supertype of `V`
   *  @param d     the function mapping keys to values, used for non-present keys
   *  @return      a wrapper of the map with a default value
   */
  override def withDefault[V1 >: V](d: K -> V1): SortedMap[K, V1] = new SortedMap.WithDefault[K, V1](this, d)

  /** The same map with a given default value.
   *  Note: The default is only used for `apply`. Other methods like `get`, `contains`, `iterator`, `keys`, etc.
   *  are not affected by `withDefaultValue`.
   *
   *  Invoking transformer methods (e.g. `map`) will not preserve the default value.
   *
   *  @tparam V1 the type of the values in the resulting map, a supertype of `V`
   *  @param d     default value used for non-present keys
   *  @return      a wrapper of the map with a default value
   */
  override def withDefaultValue[V1 >: V](d: V1): SortedMap[K, V1] = new SortedMap.WithDefault[K, V1](this, _ => d)
}

transparent trait SortedMapOps[K, +V, +CC[X, +Y] <: Map[X, Y] & SortedMapOps[X, Y, CC, ?], +C <: SortedMapOps[K, V, CC, C]]
  extends MapOps[K, V, Map, C] with collection.SortedMapOps[K, V, CC, C] { self =>

  /** Returns this map, typed as both the concrete map type `C` and `CC[K, V]`,
   *  so that it can be used where either type is required.
   */
  protected def coll: C & CC[K, V]

  /** Widens the type of this map to its unsorted counterpart. */
  def unsorted: Map[K, V]

  /** Returns a sorted set of the keys of this map, sorted by this map's ordering.
   *
   *  The set is backed by this map: it holds a reference to the map rather than
   *  copying the keys.
   */
  override def keySet: SortedSet[K] = new LazyImmutableKeySortedSet

  /** The implementation class of the set returned by `keySet`. */
  private class LazyImmutableKeySortedSet extends LazyKeySortedSet with SortedSet[K] {
    override def diff(that: scala.collection.Set[K]): SortedSet[K] = super.diff(that)
    override def rangeImpl(from: Option[K], until: Option[K]): SortedSet[K] = {
      val map = self.rangeImpl(from, until)
      new map.LazyImmutableKeySortedSet
    }
    def incl(elem: K): SortedSet[K] = fromSpecific(this).incl(elem)
    def excl(elem: K): SortedSet[K] = fromSpecific(this).excl(elem)
  }

  /** The implementation class of the set returned by `keySet` */
  @deprecated("ImmutableKeySortedSet is no longer used by the .keySet implementation", since = "3.8.0")
  protected class ImmutableKeySortedSet extends AbstractSet[K] with SortedSet[K] with GenKeySet with GenKeySortedSet {
    /** Creates a ranged projection of this key set, backed by the corresponding ranged
     *  projection of the underlying map.
     *
     *  @param from the lower bound (inclusive) of the projection, `None` if there is no lower bound
     *  @param until the upper bound (exclusive) of the projection, `None` if there is no upper bound
     *  @return the key set of the underlying map restricted to the given range
     */
    def rangeImpl(from: Option[K], until: Option[K]): SortedSet[K] = {
      val map = self.rangeImpl(from, until)
      new map.ImmutableKeySortedSet
    }
    /** Returns a sorted set containing `elem` and the keys of the underlying map.
     *
     *  The keys are first copied out of the map, so the result no longer tracks it.
     *
     *  @param elem the element to add
     */
    def incl(elem: K): SortedSet[K] = fromSpecific(this).incl(elem)
    /** Returns a sorted set containing the keys of the underlying map except `elem`.
     *
     *  The keys are first copied out of the map, so the result no longer tracks it.
     *
     *  @param elem the element to remove
     */
    def excl(elem: K): SortedSet[K] = fromSpecific(this).excl(elem)
  }

  // We override these methods to fix their return type (which would be `Map` otherwise)
  /** Returns a sorted map with `key` bound to `value` and otherwise the bindings of
   *  this map, replacing any existing binding for `key`.
   *
   *  @tparam V1 the type of the added value, a supertype of `V`
   *  @param key the key
   *  @param value the value
   *  @return a sorted map with the new binding added
   */
  def updated[V1 >: V](key: K, value: V1): CC[K, V1]
  @`inline` final override def +[V1 >: V](kv: (K, V1)): CC[K, V1] = updated(kv._1, kv._2)
  /** Updates the binding for `key` from its current optional value, `Some` if the key
   *  is bound and `None` if it is not.
   *
   *  If `remappingFunction` returns `Some(v)` the key is bound to `v`; if it returns
   *  `None` the binding is removed, or stays absent. An exception thrown by the
   *  function is rethrown and this map is left unchanged; so is a result that is the
   *  very same object the key is already bound to, in which case this map is returned.
   *
   *  The override serves only to give the result the sorted map type `CC`; the body is
   *  the one inherited from `MapOps`.
   *
   *  @tparam V1 the type of the values in the returned map, a supertype of `V`
   *  @param key the key whose binding is to be updated
   *  @param remappingFunction a function from the current optional value to the new one
   *  @return a sorted map with the updated binding for `key`
   */
  override def updatedWith[V1 >: V](key: K)(remappingFunction: Option[V] => Option[V1]): CC[K, V1] = {
    // Implementation has been copied from `MapOps`
    val previousValue = this.get(key)
    remappingFunction(previousValue) match {
      case None            => previousValue.fold(coll)(_ => this.removed(key).coll)
      case Some(nextValue) =>
        if (previousValue.exists(_.asInstanceOf[AnyRef] eq nextValue.asInstanceOf[AnyRef])) coll
        else coll.updated(key, nextValue)
    }
  }
  /** Returns a sorted map with the same keys as this map, each bound to the result of
   *  applying `f` to that key and its value.
   *
   *  The keys are unchanged, so the result keeps this map's ordering.
   *
   *  @tparam W the type of the values in the resulting map
   *  @param f the function applied to each key and its value
   *  @return a sorted map with `f` applied to every binding
   */
  override def transform[W](f: (K, V) => W): CC[K, W] = map({ case (k, v) => (k, f(k, v)) })(using ordering)
}

transparent trait StrictOptimizedSortedMapOps[K, +V, +CC[X, +Y] <: Map[X, Y] & SortedMapOps[X, Y, CC, ?], +C <: SortedMapOps[K, V, CC, C]]
  extends SortedMapOps[K, V, CC, C]
    with collection.StrictOptimizedSortedMapOps[K, V, CC, C]
    with StrictOptimizedMapOps[K, V, Map, C] {

  /** Returns a sorted map containing the bindings of this map and those of `xs`, with a
   *  binding from `xs` replacing one for an equal key in this map.
   *
   *  The bindings of `xs` are added one at a time to this map rather than through a
   *  builder, so nothing is copied when `xs` is empty.
   *
   *  @tparam V2 the type of the values of the resulting map, a supertype of `V`
   *  @param xs the bindings to add
   *  @return a sorted map with the combined bindings
   */
  override def concat[V2 >: V](xs: collection.IterableOnce[(K, V2)]^): CC[K, V2] = {
    var result: CC[K, V2] = coll
    val it = xs.iterator
    while (it.hasNext) result = result + it.next()
    result
  }
}

@SerialVersionUID(3L)
object SortedMap extends SortedMapFactory.Delegate[SortedMap](TreeMap) {

  /** Returns an immutable sorted map containing the bindings of `it`, ordered by the
   *  given `Ordering` on the keys.
   *
   *  If `it` is already a sorted map whose ordering is equal to the requested one it is
   *  returned unchanged; otherwise its bindings are copied into a new [[TreeMap]].
   *
   *  @tparam K the key type
   *  @tparam V the value type
   *  @param it the collection whose bindings are to be contained
   */
  override def from[K: Ordering, V](it: IterableOnce[(K, V)]^): SortedMap[K, V] = (it: @unchecked) match {
    case sm: SortedMap[K, V] if Ordering[K] == sm.ordering => sm
    case _ => super.from(it)
  }

  /** An immutable sorted map whose `apply` returns `defaultValue(key)` for keys that are
   *  not present instead of throwing an exception. All operations are delegated to the
   *  underlying sorted map; methods such as `get`, `contains`, `iterator`, and `keys`
   *  are not affected by the default.
   *
   *  @tparam K the type of the keys
   *  @tparam V the type of the values
   *  @param underlying the sorted map to which all operations are delegated
   *  @param defaultValue the function computing a default value for keys that are not present
   */
  final class WithDefault[K, +V](underlying: SortedMap[K, V], defaultValue: K -> V)
    extends Map.WithDefault[K, V](underlying, defaultValue)
      with SortedMap[K, V]
      with SortedMapOps[K, V, SortedMap, WithDefault[K, V]] with Serializable {

    /** Returns the key ordering of the underlying sorted map */
    implicit def ordering: Ordering[K] = underlying.ordering

    /** Returns the sorted map factory of the underlying map; maps created by that factory do not have this map's default */
    override def sortedMapFactory: SortedMapFactory[SortedMap] = underlying.sortedMapFactory

    /** Returns an iterator over the key-value pairs of the underlying map whose keys are
     *  greater than or equal to `start`, in the map's key ordering.
     *
     *  @param start the lower bound (inclusive) on the keys of the entries to return
     */
    def iteratorFrom(start: K): scala.collection.Iterator[(K, V)] = underlying.iteratorFrom(start)

    /** Returns an iterator over the keys of the underlying map that are greater than or
     *  equal to `start`, in the map's key ordering.
     *
     *  @param start the lower bound (inclusive) on the keys to return
     */
    def keysIteratorFrom(start: K): scala.collection.Iterator[K] = underlying.keysIteratorFrom(start)

    /** Returns a ranged projection of the underlying map, wrapped in a new `WithDefault`
     *  with the same default function.
     *
     *  @param from the lower bound (inclusive) of the projection wrapped in a `Some`, or `None` if there is no lower bound
     *  @param until the upper bound (exclusive) of the projection wrapped in a `Some`, or `None` if there is no upper bound
     *  @return a `WithDefault` wrapper around the ranged projection of the underlying map
     */
    def rangeImpl(from: Option[K], until: Option[K]): WithDefault[K, V] =
      new WithDefault[K, V](underlying.rangeImpl(from, until), defaultValue)

    // Need to override following methods to match type signatures of `SortedMap.WithDefault`
    // for operations preserving default value

    /** Returns a new `WithDefault` with `key` bound to `value` in the underlying map,
     *  replacing any existing binding, and with the same default function as this map.
     *
     *  @tparam V1 the type of the added value, a supertype of `V`
     *  @param key the key
     *  @param value the value
     *  @return a new `WithDefault` with the binding added and this map's default
     */
    override def updated[V1 >: V](key: K, value: V1): WithDefault[K, V1] =
      new WithDefault[K, V1](underlying.updated(key, value), defaultValue)

    /** Returns a new `WithDefault` wrapping the bindings of the underlying map together
     *  with those of `xs`, with the same default function as this map.
     *
     *  @tparam V2 the type of the values of the resulting map, a supertype of `V`
     *  @param xs the bindings to add
     *  @return a new `WithDefault` with the combined bindings and this map's default
     */
    override def concat [V2 >: V](xs: collection.IterableOnce[(K, V2)]^): WithDefault[K, V2] =
      new WithDefault( underlying.concat(xs) , defaultValue)

    /** Returns a new `WithDefault` without a binding for `key`, wrapping the underlying
     *  map with that key removed and keeping this map's default function.
     *
     *  @param key the key to remove
     */
    override def removed(key: K): WithDefault[K, V] = new WithDefault[K, V](underlying.removed(key), defaultValue)

    /** Returns a new `WithDefault` with the same default function, wrapping an empty map of the same type as the underlying map */
    override def empty: WithDefault[K, V] = new WithDefault[K, V](underlying.empty, defaultValue)

    /** Returns a new `WithDefault` with the same default function, wrapping a map that is
     *  built from the key-value pairs of `coll` with the underlying map's factory.
     *
     *  @param coll the key-value pairs of the new map
     *  @return a new `WithDefault` containing the bindings of `coll`
     */
    override protected def fromSpecific(coll: (scala.collection.IterableOnce[(K, V)]^) @uncheckedVariance): WithDefault[K, V] =
      new WithDefault[K, V](sortedMapFactory.from(coll), defaultValue)

    /** Returns a builder that collects key-value pairs into a new sorted map and wraps the result in a `WithDefault` with the same default function */
    override protected def newSpecificBuilder: Builder[(K, V), WithDefault[K, V]] @uncheckedVariance =
      SortedMap.newBuilder.mapResult((p: SortedMap[K, V]) => new WithDefault[K, V](p, defaultValue))
  }
}
