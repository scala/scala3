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
import scala.collection.generic.DefaultSerializable
import scala.collection.immutable.Map.Map4
import scala.collection.mutable.{Builder, ReusableBuilder}
import SeqMap.{SeqMap1, SeqMap2, SeqMap3, SeqMap4}

/** Base type of immutable Maps.
 *
 *  @tparam K the type of the keys in this map
 *  @tparam V the type of the values associated with the keys
 */
trait Map[K, +V]
  extends Iterable[(K, V)]
     with collection.Map[K, V]
     with MapOps[K, V, Map, Map[K, V]]
     with MapFactoryDefaults[K, V, Map, Iterable] {

  /** Returns the `immutable.Map` companion object as the factory for maps of this kind. */
  override def mapFactory: scala.collection.MapFactory[Map] = Map

  /** Returns this map, typed as an immutable `Map[K2, V2]`.
   *
   *  Since this map is already immutable, a non-empty default map implementation is
   *  returned unchanged, without copying; implementations with a reified key type,
   *  such as sorted maps, are rebuilt as a default `Map`. Any empty map yields the
   *  shared `Map.empty`, so an empty `HashMap`, `ListMap` or `VectorMap` is not
   *  returned unchanged.
   *
   *  @tparam K2 the key type of the resulting map, a supertype of `K`
   *  @tparam V2 the value type of the resulting map, a supertype of `V`
   *  @param ev evidence that the element type `(K, V)` conforms to `(K2, V2)`;
   *            never called
   *  @return this map, widened to `Map[K2, V2]`
   */
  override final def toMap[K2, V2](implicit ev: (K, V) <:< (K2, V2)): Map[K2, V2] = Map.from(this.asInstanceOf[Map[K2, V2]])

  /** The same map with a given default function.
   *  Note: The default is only used for `apply`. Other methods like `get`, `contains`, `iterator`, `keys`, etc.
   *  are not affected by `withDefault`.
   *
   *  Invoking transformer methods (e.g. `map`) will not preserve the default value.
   *
   *  @tparam V1 the type of the values returned by the default function, which must be a supertype of `V`
   *  @param d     the function mapping keys to values, used for non-present keys
   *  @return      a wrapper of the map with a default value
   */
  def withDefault[V1 >: V](d: K -> V1): Map[K, V1] = new Map.WithDefault[K, V1](this, d)

  /** The same map with a given default value.
   *  Note: The default is only used for `apply`. Other methods like `get`, `contains`, `iterator`, `keys`, etc.
   *  are not affected by `withDefaultValue`.
   *
   *  Invoking transformer methods (e.g. `map`) will not preserve the default value.
   *
   *  @tparam V1 the type of the default value, which must be a supertype of `V`
   *  @param d     default value used for non-present keys
   *  @return      a wrapper of the map with a default value
   */
  def withDefaultValue[V1 >: V](d: V1): Map[K, V1] = new Map.WithDefault[K, V1](this, _ => d)
}

/** Base trait of immutable Maps implementations
 *
 *  @define coll immutable map
 *  @define Coll `immutable.Map`
 *
 *  @tparam K the type of the keys in this map
 *  @tparam V the type of the values associated with the keys
 *  @tparam CC the type constructor of the resulting map (e.g., `Map`, `HashMap`)
 *  @tparam C the type of the map itself, used as the return type of operations that preserve the concrete map type
 */
transparent trait MapOps[K, +V, +CC[X, +Y] <: MapOps[X, Y, CC, ?], +C <: MapOps[K, V, CC, C]]
  extends IterableOps[(K, V), Iterable, C]
    with collection.MapOps[K, V, CC, C]
    with caps.Pure {

  /** Returns this map, typed as both the concrete map type `C` and `CC[K, V]`,
   *  so that it can be used where either type is required.
   */
  protected def coll: C & CC[K, V]

  /** Removes a key from this map, returning a new map.
   *
   *  @param key the key to be removed
   *  @return a new map without a binding for *key*
   */
  def removed(key: K): C

  /** Alias for `removed`.
   *
   *  @param key the key to remove from this map
   */
  @`inline` final def - (key: K): C = removed(key)

  /** Returns a $coll with the two given keys and all keys in `keys` removed, by
   *  chaining `removed`.
   *
   *  @param key1 the first key to remove
   *  @param key2 the second key to remove
   *  @param keys the remaining keys to remove
   *  @return a $coll without bindings for any of the given keys; the built-in
   *          immutable maps return themselves when none of the keys is bound
   */
  @deprecated("Use -- with an explicit collection", "2.13.0")
  def - (key1: K, key2: K, keys: K*): C = removed(key1).removed(key2).removedAll(keys)

  /** Creates a new $coll from this $coll by removing all elements of another
   *  collection.
   *
   *  $willForceEvaluation
   *
   *  @param keys   the collection containing the removed elements.
   *  @return a new $coll that contains all elements of the current $coll
   *  except one less occurrence of each of the elements of `elems`.
   */
  def removedAll(keys: IterableOnce[K]^): C = keys.iterator.foldLeft[C](coll)(_ - _)

  /** Alias for `removedAll`.
   *
   *  @param keys the collection of keys to remove from this map
   */
  @`inline` final override def -- (keys: IterableOnce[K]^): C = removedAll(keys)

  /** Creates a new map obtained by updating this map with a given key/value pair.
   *  @tparam   V1 the type of the added value
   *  @param    key the key
   *  @param    value the value
   *  @return   A new map with the new key/value mapping added to this map.
   */
  def updated[V1 >: V](key: K, value: V1): CC[K, V1]

  /** Updates a mapping for the specified key and its current optionally mapped value
   *  (`Some` if there is current mapping, `None` if not).
   *
   *  If the remapping function returns `Some(v)`, the mapping is updated with the new value `v`.
   *  If the remapping function returns `None`, the mapping is removed (or remains absent if initially absent).
   *  If the function itself throws an exception, the exception is rethrown, and the current mapping is left unchanged.
   *
   *  @tparam V1 the type of the values in the returned map, which must be a supertype of `V`
   *  @param key the key value
   *  @param remappingFunction a function that receives current optionally mapped value and returns a new mapping
   *  @return A new map with the updated mapping with the key
   */
  def updatedWith[V1 >: V](key: K)(remappingFunction: Option[V] => Option[V1]): CC[K,V1] = {
    val previousValue = this.get(key)
    remappingFunction(previousValue) match {
      case None            => previousValue.fold(coll)(_ => this.removed(key).coll)
      case Some(nextValue) =>
        if (previousValue.exists(_.asInstanceOf[AnyRef] eq nextValue.asInstanceOf[AnyRef])) coll
        else coll.updated(key, nextValue)
    }
  }

  /** Alias for `updated`
   *
   *  @tparam V1 the type of the value in the key/value pair.
   *  @param kv the key/value pair.
   *  @return A new map with the new binding added to this map.
   */
  override def + [V1 >: V](kv: (K, V1)): CC[K, V1] = updated(kv._1, kv._2)

  /** This function transforms all the values of mappings contained
   *  in this map with function `f`.
   *
   *  @tparam W the type of the transformed values
   *  @param f A function over keys and values
   *  @return  the updated map
   */
  def transform[W](f: (K, V) => W): CC[K, W] = map { case (k, v) => (k, f(k, v)) }

  /** Returns an immutable set of the keys contained in this map.
   *
   *  The returned set holds a reference to this map and reads the keys from it
   *  lazily, rather than copying them.
   *
   *  @return a set containing the keys of this map
   */
  override def keySet: Set[K] = new LazyImmutableKeySet

  /** The implementation class of the set returned by `keySet`. */
  private[immutable] class LazyImmutableKeySet extends MapOps.LazyKeySet(this) with Set[K] {
    /** Returns a new set containing the keys of this set that are not also
     *  contained in `that`.
     *
     *  @param that the set of keys to exclude
     *  @return an immutable set of the keys in this set but not in `that`
     */
    override def diff(that: collection.Set[K]): Set[K] = super.diff(that)
    /** Returns a set containing `elem` and all keys of this set.
     *
     *  If `elem` is already a key of the underlying map, returns this set
     *  itself; otherwise returns the key set of a new map that also binds
     *  `elem`, to the unit value `()`.
     *
     *  @param elem the key to add
     *  @return a set containing the keys of this set and `elem`
     */
    override def incl(elem: K): Set[K] = if (this(elem)) this else MapOps.this.updated(elem, ()).keySet
    /** Returns a set containing all keys of this set except `elem`.
     *
     *  If `elem` is not a key of the underlying map, returns this set itself;
     *  otherwise returns the key set of a new map with the binding for `elem`
     *  removed.
     *
     *  @param elem the key to remove
     *  @return a set containing the keys of this set except `elem`
     */
    override def excl(elem: K): Set[K] = if (this(elem)) MapOps.this.removed(elem).keySet else this
  }

  /** The implementation class of the set returned by `keySet`. */
  @deprecated("ImmutableKeySet is no longer used in .keySet implementations", since = "3.8.0")
  protected[immutable] class ImmutableKeySet extends AbstractSet[K] with GenKeySet with DefaultSerializable {
    /** Returns a set containing `elem` and all keys of this set.
     *
     *  If `elem` is already present, returns this set itself; otherwise
     *  returns a new strict set built by copying the elements of this set and
     *  adding `elem`.
     *
     *  @param elem the key to add
     *  @return a set containing the keys of this set and `elem`
     */
    def incl(elem: K): Set[K] = if (this(elem)) this else empty ++ this + elem
    /** Returns a set containing all keys of this set except `elem`.
     *
     *  If `elem` is not present, returns this set itself; otherwise returns a
     *  new strict set built by copying the elements of this set and removing
     *  `elem`.
     *
     *  @param elem the key to remove
     *  @return a set containing the keys of this set except `elem`
     */
    def excl(elem: K): Set[K] = if (this(elem)) empty ++ this - elem else this
  }

}

transparent trait StrictOptimizedMapOps[K, +V, +CC[X, +Y] <: MapOps[X, Y, CC, ?], +C <: MapOps[K, V, CC, C]]
  extends MapOps[K, V, CC, C]
    with collection.StrictOptimizedMapOps[K, V, CC, C]
    with StrictOptimizedIterableOps[(K, V), Iterable, C] {

  /** Returns a $coll containing the key/value pairs of this $coll followed
   *  by those of `that`, built eagerly by adding the pairs one at a time to this
   *  $coll, so an empty `that` leaves this $coll itself as the result.
   *
   *  Pairs in `that` override pairs of this $coll with the same key.
   *
   *  @tparam V1 the value type of the returned map, a supertype of `V`
   *  @param that the key/value pairs to add
   *  @return a $coll with the combined bindings
   */
  override def concat [V1 >: V](that: collection.IterableOnce[(K, V1)]^): CC[K, V1] = {
    var result: CC[K, V1] = coll
    val it = that.iterator
    while (it.hasNext) result = result + it.next()
    result
  }
}


/** $factoryInfo
 *  @define coll immutable map
 *  @define Coll `immutable.Map`
 */
@SerialVersionUID(3L)
object Map extends MapFactory[Map] {

  /** An immutable map that wraps `underlying` and adds a default function,
   *  used by `apply` when a requested key is not present.
   *
   *  Only `apply` uses the default; `get`, `contains`, `iterator`, and other
   *  queries are unaffected. Operations that produce a map of the same type,
   *  such as `updated`, `removed`, `concat`, and `filter`, preserve the
   *  default; transformations to other element types, such as `map` and
   *  `flatMap`, return maps without it.
   *
   *  @tparam K the type of the keys in this map
   *  @tparam V the type of the values associated with the keys
   *  @param underlying the map providing the actual bindings
   *  @param defaultValue the function computing a default value for a missing key
   */
  @SerialVersionUID(3L)
  class WithDefault[K, +V](val underlying: Map[K, V], val defaultValue: K -> V)
    extends AbstractMap[K, V]
      with MapOps[K, V, Map, WithDefault[K, V]] with Serializable {

    /** Returns the value associated with `key` in the underlying map as an
     *  option. The default value is not used.
     *
     *  @param key the key value
     *  @return an option value containing the value associated with `key`, or
     *          `None` if `key` is not present in the underlying map
     */
    def get(key: K): Option[V] = underlying.get(key)

    /** Defines the default value computation for the map, returned by `apply`
     *  when a key is not found.
     *
     *  @param key the given key value for which a binding is missing
     *  @return the result of applying `defaultValue` to `key`
     */
    override def default(key: K): V = defaultValue(key)

    /** Returns the factory for `Iterable` collections used by the underlying map. */
    override def iterableFactory: IterableFactory[Iterable] = underlying.iterableFactory

    /** Returns an iterator over the key/value pairs of the underlying map. */
    def iterator: Iterator[(K, V)] = underlying.iterator

    /** Returns `true` if the underlying map contains no bindings. */
    override def isEmpty: Boolean = underlying.isEmpty

    /** Returns the factory of the underlying map. Maps it builds do not have a default value. */
    override def mapFactory: MapFactory[Map] = underlying.mapFactory

    /** Returns a new `WithDefault` map containing the bindings of the
     *  underlying map followed by those of `xs`, with the same default value
     *  function as this map.
     *
     *  Bindings in `xs` override bindings of this map with the same key.
     *
     *  @tparam V2 the value type of the returned map, a supertype of `V`
     *  @param xs the key/value pairs to add
     *  @return a new `WithDefault` map with the combined bindings and this map's default
     */
    override def concat [V2 >: V](xs: collection.IterableOnce[(K, V2)]^): WithDefault[K, V2] =
      new WithDefault(underlying.concat(xs), defaultValue)

    /** Returns a new `WithDefault` map with the binding for `key` removed from
     *  the underlying map, and with the same default value function as this map.
     *
     *  @param key the key to remove
     *  @return a new `WithDefault` map without a binding for `key`
     */
    def removed(key: K): WithDefault[K, V] = new WithDefault[K, V](underlying.removed(key), defaultValue)

    /** Returns a new `WithDefault` map with `key` bound to `value`, replacing
     *  any existing binding, and with the same default value function as this
     *  map.
     *
     *  @tparam V1 the type of the added value, a supertype of `V`
     *  @param key the key
     *  @param value the value
     *  @return a new `WithDefault` map with the binding added and this map's default
     */
    def updated[V1 >: V](key: K, value: V1): WithDefault[K, V1] =
      new WithDefault[K, V1](underlying.updated(key, value), defaultValue)

    /** Returns a new, empty `WithDefault` map with the same default value function as this map. */
    override def empty: WithDefault[K, V] = new WithDefault[K, V](underlying.empty, defaultValue)

    /** Returns a new `WithDefault` map containing the elements of `coll` and
     *  the same default value function as this map.
     *
     *  @param coll the key/value pairs for the new map
     *  @return a new `WithDefault` map with those bindings and this map's default
     */
    override protected def fromSpecific(coll: (collection.IterableOnce[(K, V)]^) @uncheckedVariance): WithDefault[K, V] =
      new WithDefault[K, V](mapFactory.from(coll), defaultValue)

    /** Returns a builder that produces a `WithDefault` map with the same default value function as this map. */
    override protected def newSpecificBuilder: Builder[(K, V), WithDefault[K, V]] @uncheckedVariance =
      Map.newBuilder.mapResult((p: Map[K, V]) => new WithDefault[K, V](p, defaultValue))
  }

  /** Returns the empty immutable map.
   *
   *  A single empty map instance is shared: the same instance is returned for
   *  every call, cast to the requested key and value types.
   *
   *  @tparam K the type of the keys
   *  @tparam V the type of the values
   *  @return the empty immutable map
   */
  def empty[K, V]: Map[K, V] = EmptyMap.asInstanceOf[Map[K, V]]

  /** Returns an immutable map containing the key/value pairs of `it`.
   *
   *  An empty `Iterable` yields the shared empty map. Otherwise, if `it` is already
   *  one of the default immutable map implementations, such as a `HashMap`,
   *  `ListMap`, `VectorMap`, or one of the specialized small maps, it is returned
   *  unchanged. In every other case, including for maps with a reified key type such
   *  as sorted maps, a new map is built from its elements, with later bindings
   *  overriding earlier ones with the same key.
   *
   *  @tparam K the type of the keys
   *  @tparam V the type of the values
   *  @param it the source collection of key/value pairs
   *  @return an immutable map with the bindings of `it`
   */
  def from[K, V](it: IterableOnce[(K, V)]^): Map[K, V] =
    (it: @unchecked) match {
      case it: Iterable[?] if it.isEmpty => empty[K, V]
      // Since IterableOnce[(K, V)] launders the variance of K,
      // identify only our implementations which can be soundly substituted.
      // For example, the ordering used by sorted maps would fail on widened key type. (scala/bug#12745)
      // The following type test is not sufficient: case m: Map[K, V] => m
      case m: HashMap[K, V]    => m
      case m: Map1[K, V]       => m
      case m: Map2[K, V]       => m
      case m: Map3[K, V]       => m
      case m: Map4[K, V]       => m
      //case m: WithDefault[K, V] => m    // cf SortedMap.WithDefault
      //case m: SeqMap[K, V]     => SeqMap.from(it) // inlined here to avoid hard dependency
      case m: ListMap[K, V]    => m
      case m: TreeSeqMap[K, V] => m
      case m: VectorMap[K, V]  => m
      case m: SeqMap1[K, V]    => m
      case m: SeqMap2[K, V]    => m
      case m: SeqMap3[K, V]    => m
      case m: SeqMap4[K, V]    => m

      // Maps with a reified key type must be rebuilt, such as `SortedMap` and `IntMap`.
      case _ => newBuilder[K, V].addAll(it).result()
    }

  /** Returns a new builder for an immutable map.
   *
   *  @tparam K the type of the keys
   *  @tparam V the type of the values
   *  @return a builder that produces the specialized small maps for up to four
   *          distinct keys, and a `HashMap` beyond that
   */
  def newBuilder[K, V]: Builder[(K, V), Map[K, V]] = new MapBuilderImpl

  @SerialVersionUID(3L)
  private object EmptyMap extends AbstractMap[Any, Nothing] with Serializable {
    /** Returns `0`: this map has no bindings. */
    override def size: Int = 0
    /** Returns `0`: the size is always known. */
    override def knownSize: Int = 0
    /** Returns `true`: this is the empty map. */
    override def isEmpty: Boolean = true
    /** Throws a `NoSuchElementException`: the empty map contains no keys.
     *
     *  @param key the requested key, reported in the exception message
     *  @throws NoSuchElementException always
     */
    override def apply(key: Any) = throw new NoSuchElementException("key not found: " + key)
    /** Returns `false`: the empty map contains no keys.
     *
     *  @param key the key to test; never used
     */
    override def contains(key: Any) = false
    /** Returns `None`: the empty map contains no keys.
     *
     *  @param key the key to look up; never used
     *  @return `None`, for every key
     */
    def get(key: Any): Option[Nothing] = None
    /** Returns the result of evaluating `default`: the empty map contains no
     *  keys.
     *
     *  @tparam V1 the type of `default`
     *  @param key the key to look up; never used
     *  @param default the value to evaluate and return
     *  @return the value of `default`
     */
    override def getOrElse [V1](key: Any, default: => V1): V1 = default
    /** Returns the empty iterator: this map has no key/value pairs. */
    def iterator: Iterator[(Any, Nothing)] = Iterator.empty
    /** Returns the empty iterator: this map has no keys. */
    override def keysIterator: Iterator[Any] = Iterator.empty
    /** Returns the empty iterator: this map has no values. */
    override def valuesIterator: Iterator[Nothing] = Iterator.empty
    /** Returns a new `Map1` containing the single binding `key -> value`.
     *
     *  @tparam V1 the type of the added value
     *  @param key the key
     *  @param value the value
     *  @return a one-binding map containing `key -> value`
     */
    def updated [V1] (key: Any, value: V1): Map[Any, V1] = new Map1(key, value)
    /** Returns this map itself: the empty map has no binding to remove.
     *
     *  @param key the key to remove; never used
     *  @return this empty map
     */
    def removed(key: Any): Map[Any, Nothing] = this
    /** Returns a map containing the key/value pairs of `suffix`.
     *
     *  If `suffix` is already an immutable map, it is returned unchanged;
     *  otherwise a new immutable map is built from its elements.
     *
     *  @tparam V2 the type of the values in `suffix`
     *  @param suffix the key/value pairs of the resulting map
     *  @return `suffix` itself if it is an immutable map, or a new immutable
     *          map with its bindings
     */
    override def concat[V2 >: Nothing](suffix: IterableOnce[(Any, V2)]^): Map[Any, V2] = (suffix: @unchecked) match {
      case m: immutable.Map[Any, V2] => m
      case _ => super.concat(suffix)
    }
  }

  /** An immutable map with exactly one binding, stored directly in fields.
   *
   *  @tparam K the type of the key
   *  @tparam V the type of the value
   *  @param key1 the key of the single binding
   *  @param value1 the value of the single binding
   */
  @SerialVersionUID(3L)
  final class Map1[K, +V](key1: K, value1: V) extends AbstractMap[K, V] with StrictOptimizedIterableOps[(K, V), Iterable, Map[K, V]] with Serializable {
    /** Returns `1`: this map has exactly one binding. */
    override def size: Int = 1
    /** Returns `1`: the size is always known. */
    override def knownSize: Int = 1
    /** Returns `false`: this map always has one binding. */
    override def isEmpty: Boolean = false
    /** Returns the value associated with `key`.
     *
     *  @param key the key to look up
     *  @return the value associated with `key`
     *  @throws NoSuchElementException if `key` is not the key of this map
     */
    override def apply(key: K): V = if (key == key1) value1 else throw new NoSuchElementException("key not found: " + key)
    /** Returns `true` if `key` is the key of this map, `false` otherwise.
     *
     *  @param key the key to test
     */
    override def contains(key: K): Boolean = key == key1
    /** Returns the value associated with `key` as an option.
     *
     *  @param key the key to look up
     *  @return `Some` of the value associated with `key`, or `None` if `key`
     *          is not the key of this map
     */
    def get(key: K): Option[V] =
      if (key == key1) Some(value1) else None
    /** Returns the value associated with `key`, or `default` if `key` is not
     *  present.
     *
     *  @tparam V1 the type of the result, a supertype of `V`
     *  @param key the key to look up
     *  @param default the value to return if `key` is not present; evaluated
     *                 only in that case
     *  @return the value associated with `key`, or the value of `default`
     */
    override def getOrElse [V1 >: V](key: K, default: => V1): V1 =
      if (key == key1) value1 else default
    /** Returns an iterator over the single key/value pair of this map. */
    def iterator: Iterator[(K, V)] = Iterator.single((key1, value1))
    /** Returns an iterator over the single key of this map. */
    override def keysIterator: Iterator[K] = Iterator.single(key1)
    /** Returns an iterator over the single value of this map. */
    override def valuesIterator: Iterator[V] = Iterator.single(value1)
    /** Returns a new map with `key` bound to `value`.
     *
     *  If `key` equals the key of this map, returns a new `Map1` with the
     *  value replaced; otherwise returns a `Map2` with both bindings.
     *
     *  @tparam V1 the type of the added value, a supertype of `V`
     *  @param key the key
     *  @param value the value
     *  @return a new map of one or two bindings containing `key -> value`
     */
    def updated[V1 >: V](key: K, value: V1): Map[K, V1] =
      if (key == key1) new Map1(key1, value)
      else new Map2(key1, value1, key, value)
    /** Returns the empty map if `key` is the key of this map, or this map
     *  itself otherwise.
     *
     *  @param key the key to remove
     *  @return a map without a binding for `key`
     */
    def removed(key: K): Map[K, V] =
      if (key == key1) Map.empty else this
    /** Applies `f` to the single key/value pair of this map.
     *
     *  @tparam U the result type of `f`; the result is discarded
     *  @param f the function to apply
     */
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1))
    }
    /** Returns `true` if the single key/value pair of this map satisfies `p`.
     *
     *  @param p the predicate to test
     */
    override def exists(p: ((K, V)) => Boolean): Boolean = p((key1, value1))
    /** Returns `true` if the single key/value pair of this map satisfies `p`.
     *  With exactly one binding, `forall` and `exists` coincide.
     *
     *  @param p the predicate to test
     */
    override def forall(p: ((K, V)) => Boolean): Boolean = p((key1, value1))
    /** Returns this map if its single key/value pair satisfies `pred` (or
     *  fails it, when `isFlipped` is `true`), or the empty map otherwise.
     *
     *  @param pred the predicate to test
     *  @param isFlipped if `true`, keeps the pair only when `pred` is not satisfied
     *  @return this map, or the empty map
     */
    override protected[collection] def filterImpl(pred: ((K, V)) => Boolean, isFlipped: Boolean): Map[K, V] =
      if (pred((key1, value1)) != isFlipped) this else Map.empty
    /** Returns a map obtained by applying `f` to the key and value of this map.
     *
     *  If the transformed value is reference-equal to the current value, this
     *  map itself is returned; otherwise a new `Map1` with the same key and
     *  the transformed value.
     *
     *  @tparam W the type of the transformed value
     *  @param f the transformation function, applied to the key and value
     *  @return a one-binding map with the same key and the transformed value
     */
    override def transform[W](f: (K, V) => W): Map[K, W] = {
      val walue1 = f(key1, value1)
      if (walue1.asInstanceOf[AnyRef] eq value1.asInstanceOf[AnyRef]) this.asInstanceOf[Map[K, W]]
      else new Map1(key1, walue1)
    }
    /** Returns a hash code computed with MurmurHash3 from the single key/value
     *  pair, equal to the hash code of any other map with the same binding.
     */
    override def hashCode(): Int = {
      import scala.util.hashing.MurmurHash3
      var a, b = 0
      val N = 1
      var c = 1

      var h = MurmurHash3.tuple2Hash(key1, value1)
      a += h
      b ^= h
      c *= h | 1

      h = MurmurHash3.mapSeed
      h = MurmurHash3.mix(h, a)
      h = MurmurHash3.mix(h, b)
      h = MurmurHash3.mixLast(h, c)
      MurmurHash3.finalizeHash(h, N)
    }
  }

  /** An immutable map with exactly two bindings, stored directly in fields.
   *
   *  @tparam K the type of the keys
   *  @tparam V the type of the values
   *  @param key1 the key of the first binding
   *  @param value1 the value of the first binding
   *  @param key2 the key of the second binding
   *  @param value2 the value of the second binding
   */
  @SerialVersionUID(3L)
  final class Map2[K, +V](key1: K, value1: V, key2: K, value2: V) extends AbstractMap[K, V] with StrictOptimizedIterableOps[(K, V), Iterable, Map[K, V]] with Serializable {
    /** Returns `2`: this map has exactly two bindings. */
    override def size: Int = 2
    /** Returns `2`: the size is always known. */
    override def knownSize: Int = 2
    /** Returns `false`: this map always has two bindings. */
    override def isEmpty: Boolean = false
    /** Returns the value associated with `key`.
     *
     *  @param key the key to look up
     *  @return the value associated with `key`
     *  @throws NoSuchElementException if `key` is not a key of this map
     */
    override def apply(key: K): V =
      if (key == key1) value1
      else if (key == key2) value2
      else throw new NoSuchElementException("key not found: " + key)
    /** Returns `true` if `key` is one of the keys of this map, `false` otherwise.
     *
     *  @param key the key to test
     */
    override def contains(key: K): Boolean = (key == key1) || (key == key2)
    /** Returns the value associated with `key` as an option.
     *
     *  @param key the key to look up
     *  @return `Some` of the value associated with `key`, or `None` if `key`
     *          is not a key of this map
     */
    def get(key: K): Option[V] =
      if (key == key1) Some(value1)
      else if (key == key2) Some(value2)
      else None
    /** Returns the value associated with `key`, or `default` if `key` is not
     *  present.
     *
     *  @tparam V1 the type of the result, a supertype of `V`
     *  @param key the key to look up
     *  @param default the value to return if `key` is not present; evaluated
     *                 only in that case
     *  @return the value associated with `key`, or the value of `default`
     */
    override def getOrElse [V1 >: V](key: K, default: => V1): V1 =
      if (key == key1) value1
      else if (key == key2) value2
      else default
    /** Returns an iterator over the two key/value pairs of this map. */
    def iterator: Iterator[(K, V)] = new Map2Iterator[(K, V)] {
      override protected def nextResult(k: K, v: V): (K, V) = (k, v)
    }
    /** Returns an iterator over the two keys of this map. */
    override def keysIterator: Iterator[K] = new Map2Iterator[K] {
      override protected def nextResult(k: K, v: V): K = k
    }
    /** Returns an iterator over the two values of this map. */
    override def valuesIterator: Iterator[V] = new Map2Iterator[V] {
      override protected def nextResult(k: K, v: V): V = v
    }

    private abstract class Map2Iterator[A] extends AbstractIterator[A] {
      private var i = 0
      /** Returns `true` if this iterator has not yet passed both bindings. */
      override def hasNext: Boolean = i < 2
      /** Returns the result for the next binding of the map and advances this
       *  iterator.
       *
       *  @return the result of `nextResult` applied to the next key/value pair
       *  @throws NoSuchElementException if there are no more elements
       */
      override def next(): A = {
        val result = i match {
          case 0 => nextResult(key1, value1)
          case 1 => nextResult(key2, value2)
          case _ => Iterator.empty.next()
        }
        i += 1
        result
      }
      /** Advances this iterator past the next `n` elements and returns this
       *  same iterator, without creating an intermediate one.
       *
       *  `n` is added to the current position without being clamped at 0, so a
       *  negative `n` moves the position back and replays elements already
       *  returned, where the inherited `Iterator.drop` would treat it as 0.
       *
       *  @param n the number of elements to skip
       *  @return this iterator
       */
      override def drop(n: Int): Iterator[A] = { i += n; this }
      /** Returns the iteration result derived from the key and value of a
       *  binding.
       *
       *  @param k the key of the binding
       *  @param v the value of the binding
       *  @return the element this iterator produces for the binding
       */
      protected def nextResult(k: K, v: V @uncheckedVariance): A
    }
    /** Returns a new map with `key` bound to `value`.
     *
     *  If `key` equals one of the keys of this map, returns a new `Map2` with
     *  that key's value replaced; otherwise returns a `Map3` with the new
     *  binding added after the existing ones.
     *
     *  @tparam V1 the type of the added value, a supertype of `V`
     *  @param key the key
     *  @param value the value
     *  @return a new map of two or three bindings containing `key -> value`
     */
    def updated[V1 >: V](key: K, value: V1): Map[K, V1] =
      if (key == key1) new Map2(key1, value, key2, value2)
      else if (key == key2) new Map2(key1, value1, key2, value)
      else new Map3(key1, value1, key2, value2, key, value)
    /** Returns a `Map1` of the remaining binding if `key` is a key of this
     *  map, or this map itself otherwise.
     *
     *  @param key the key to remove
     *  @return a map without a binding for `key`
     */
    def removed(key: K): Map[K, V] =
      if (key == key1) new Map1(key2, value2)
      else if (key == key2) new Map1(key1, value1)
      else this
    /** Applies `f` to each key/value pair of this map.
     *
     *  @tparam U the result type of `f`; the results are discarded
     *  @param f the function to apply
     */
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1)); f((key2, value2))
    }
    /** Returns `true` if at least one key/value pair of this map satisfies
     *  `p`. The predicate is not applied to the second pair if the first one
     *  satisfies it.
     *
     *  @param p the predicate to test
     */
    override def exists(p: ((K, V)) => Boolean): Boolean = p((key1, value1)) || p((key2, value2))
    /** Returns `true` if both key/value pairs of this map satisfy `p`. The
     *  predicate is not applied to the second pair if the first one fails it.
     *
     *  @param p the predicate to test
     */
    override def forall(p: ((K, V)) => Boolean): Boolean = p((key1, value1)) && p((key2, value2))
    /** Returns a map containing the key/value pairs of this map that satisfy
     *  `pred` (or fail it, when `isFlipped` is `true`).
     *
     *  @param pred the predicate to test
     *  @param isFlipped if `true`, keeps a pair only when `pred` is not satisfied
     *  @return this map if both pairs are kept, a `Map1` if one is, or the
     *          empty map if none are
     */
    override protected[collection] def filterImpl(pred: ((K, V)) => Boolean, isFlipped: Boolean): Map[K, V] = {
      var k1 = null.asInstanceOf[K]
      var v1 = null.asInstanceOf[V]
      var n = 0
      if (pred((key1, value1)) != isFlipped) {             {k1 = key1; v1 = value1}; n += 1}
      if (pred((key2, value2)) != isFlipped) { if (n == 0) {k1 = key2; v1 = value2}; n += 1}

      n match {
        case 0 => Map.empty
        case 1 => new Map1(k1, v1)
        case 2 => this
      }
    }
    /** Returns a map obtained by applying `f` to each key and its value.
     *
     *  If every transformed value is reference-equal to the current one, this
     *  map itself is returned; otherwise a new `Map2` with the same keys and
     *  the transformed values.
     *
     *  @tparam W the type of the transformed values
     *  @param f the transformation function, applied to each key and its value
     *  @return a two-binding map with the same keys and the transformed values
     */
    override def transform[W](f: (K, V) => W): Map[K, W] = {
      val walue1 = f(key1, value1)
      val walue2 = f(key2, value2)
      if ((walue1.asInstanceOf[AnyRef] eq value1.asInstanceOf[AnyRef]) &&
          (walue2.asInstanceOf[AnyRef] eq value2.asInstanceOf[AnyRef])) this.asInstanceOf[Map[K, W]]
      else new Map2(key1, walue1, key2, walue2)
    }
    /** Returns a hash code computed with MurmurHash3 from the two key/value
     *  pairs, equal to the hash code of any other map with the same bindings.
     */
    override def hashCode(): Int = {
      import scala.util.hashing.MurmurHash3
      var a, b = 0
      val N = 2
      var c = 1

      var h = MurmurHash3.tuple2Hash(key1, value1)
      a += h
      b ^= h
      c *= h | 1

      h = MurmurHash3.tuple2Hash(key2, value2)
      a += h
      b ^= h
      c *= h | 1

      h = MurmurHash3.mapSeed
      h = MurmurHash3.mix(h, a)
      h = MurmurHash3.mix(h, b)
      h = MurmurHash3.mixLast(h, c)
      MurmurHash3.finalizeHash(h, N)
    }
  }

  /** An immutable map with exactly three bindings, stored directly in fields.
   *
   *  @tparam K the type of the keys
   *  @tparam V the type of the values
   *  @param key1 the key of the first binding
   *  @param value1 the value of the first binding
   *  @param key2 the key of the second binding
   *  @param value2 the value of the second binding
   *  @param key3 the key of the third binding
   *  @param value3 the value of the third binding
   */
  @SerialVersionUID(3L)
  class Map3[K, +V](key1: K, value1: V, key2: K, value2: V, key3: K, value3: V) extends AbstractMap[K, V] with StrictOptimizedIterableOps[(K, V), Iterable, Map[K, V]] with Serializable {
    /** Returns `3`: this map has exactly three bindings. */
    override def size: Int = 3
    /** Returns `3`: the size is always known. */
    override def knownSize: Int = 3
    /** Returns `false`: this map always has three bindings. */
    override def isEmpty: Boolean = false
    /** Returns the value associated with `key`.
     *
     *  @param key the key to look up
     *  @return the value associated with `key`
     *  @throws NoSuchElementException if `key` is not a key of this map
     */
    override def apply(key: K): V =
      if (key == key1) value1
      else if (key == key2) value2
      else if (key == key3) value3
      else throw new NoSuchElementException("key not found: " + key)
    /** Returns `true` if `key` is one of the keys of this map, `false` otherwise.
     *
     *  @param key the key to test
     */
    override def contains(key: K): Boolean = (key == key1) || (key == key2) || (key == key3)
    /** Returns the value associated with `key` as an option.
     *
     *  @param key the key to look up
     *  @return `Some` of the value associated with `key`, or `None` if `key`
     *          is not a key of this map
     */
    def get(key: K): Option[V] =
      if (key == key1) Some(value1)
      else if (key == key2) Some(value2)
      else if (key == key3) Some(value3)
      else None
    /** Returns the value associated with `key`, or `default` if `key` is not
     *  present.
     *
     *  @tparam V1 the type of the result, a supertype of `V`
     *  @param key the key to look up
     *  @param default the value to return if `key` is not present; evaluated
     *                 only in that case
     *  @return the value associated with `key`, or the value of `default`
     */
    override def getOrElse [V1 >: V](key: K, default: => V1): V1 =
      if (key == key1) value1
      else if (key == key2) value2
      else if (key == key3) value3
      else default
    /** Returns an iterator over the three key/value pairs of this map. */
    def iterator: Iterator[(K, V)] = new Map3Iterator[(K, V)] {
      override protected def nextResult(k: K, v: V): (K, V) = (k, v)
    }
    /** Returns an iterator over the three keys of this map. */
    override def keysIterator: Iterator[K] = new Map3Iterator[K] {
      override protected def nextResult(k: K, v: V): K = k
    }
    /** Returns an iterator over the three values of this map. */
    override def valuesIterator: Iterator[V] = new Map3Iterator[V] {
      override protected def nextResult(k: K, v: V): V = v
    }

    private abstract class Map3Iterator[A] extends AbstractIterator[A] {
      private var i = 0
      /** Returns `true` if this iterator has not yet passed all three bindings. */
      override def hasNext: Boolean = i < 3
      /** Returns the result for the next binding of the map and advances this
       *  iterator.
       *
       *  @return the result of `nextResult` applied to the next key/value pair
       *  @throws NoSuchElementException if there are no more elements
       */
      override def next(): A = {
        val result = i match {
          case 0 => nextResult(key1, value1)
          case 1 => nextResult(key2, value2)
          case 2 => nextResult(key3, value3)
          case _ => Iterator.empty.next()
        }
        i += 1
        result
      }
      /** Advances this iterator past the next `n` elements and returns this
       *  same iterator, without creating an intermediate one.
       *
       *  `n` is added to the current position without being clamped at 0, so a
       *  negative `n` moves the position back and replays elements already
       *  returned, where the inherited `Iterator.drop` would treat it as 0.
       *
       *  @param n the number of elements to skip
       *  @return this iterator
       */
      override def drop(n: Int): Iterator[A] = { i += n; this }
      /** Returns the iteration result derived from the key and value of a
       *  binding.
       *
       *  @param k the key of the binding
       *  @param v the value of the binding
       *  @return the element this iterator produces for the binding
       */
      protected def nextResult(k: K, v: V @uncheckedVariance): A
    }
    /** Returns a new map with `key` bound to `value`.
     *
     *  If `key` equals one of the keys of this map, returns a new `Map3` with
     *  that key's value replaced; otherwise returns a `Map4` with the new
     *  binding added after the existing ones.
     *
     *  @tparam V1 the type of the added value, a supertype of `V`
     *  @param key the key
     *  @param value the value
     *  @return a new map of three or four bindings containing `key -> value`
     */
    def updated[V1 >: V](key: K, value: V1): Map[K, V1] =
      if (key == key1)      new Map3(key1, value, key2, value2, key3, value3)
      else if (key == key2) new Map3(key1, value1, key2, value, key3, value3)
      else if (key == key3) new Map3(key1, value1, key2, value2, key3, value)
      else new Map4(key1, value1, key2, value2, key3, value3, key, value)
    /** Returns a `Map2` of the remaining bindings if `key` is a key of this
     *  map, or this map itself otherwise.
     *
     *  @param key the key to remove
     *  @return a map without a binding for `key`
     */
    def removed(key: K): Map[K, V] =
      if (key == key1)      new Map2(key2, value2, key3, value3)
      else if (key == key2) new Map2(key1, value1, key3, value3)
      else if (key == key3) new Map2(key1, value1, key2, value2)
      else this
    /** Applies `f` to each key/value pair of this map.
     *
     *  @tparam U the result type of `f`; the results are discarded
     *  @param f the function to apply
     */
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1)); f((key2, value2)); f((key3, value3))
    }
    /** Returns `true` if at least one key/value pair of this map satisfies
     *  `p`. The predicate is not applied to the remaining pairs once one
     *  satisfies it.
     *
     *  @param p the predicate to test
     */
    override def exists(p: ((K, V)) => Boolean): Boolean = p((key1, value1)) || p((key2, value2)) || p((key3, value3))
    /** Returns `true` if all three key/value pairs of this map satisfy `p`.
     *  The predicate is not applied to the remaining pairs once one fails it.
     *
     *  @param p the predicate to test
     */
    override def forall(p: ((K, V)) => Boolean): Boolean = p((key1, value1)) && p((key2, value2)) && p((key3, value3))
    /** Returns a map containing the key/value pairs of this map that satisfy
     *  `pred` (or fail it, when `isFlipped` is `true`).
     *
     *  @param pred the predicate to test
     *  @param isFlipped if `true`, keeps a pair only when `pred` is not satisfied
     *  @return this map if all three pairs are kept, or a smaller map, down to
     *          the empty map, of the pairs that are
     */
    override protected[collection] def filterImpl(pred: ((K, V)) => Boolean, isFlipped: Boolean): Map[K, V] = {
      var k1, k2 = null.asInstanceOf[K]
      var v1, v2 = null.asInstanceOf[V]
      var n = 0
      if (pred((key1, value1)) != isFlipped) {             { k1 = key1; v1 = value1 };                                             n += 1}
      if (pred((key2, value2)) != isFlipped) { if (n == 0) { k1 = key2; v1 = value2 } else             { k2 = key2; v2 = value2 }; n += 1}
      if (pred((key3, value3)) != isFlipped) { if (n == 0) { k1 = key3; v1 = value3 } else if (n == 1) { k2 = key3; v2 = value3 }; n += 1}

      n match {
        case 0 => Map.empty
        case 1 => new Map1(k1, v1)
        case 2 => new Map2(k1, v1, k2, v2)
        case 3 => this
      }
    }
    /** Returns a map obtained by applying `f` to each key and its value.
     *
     *  If every transformed value is reference-equal to the current one, this
     *  map itself is returned; otherwise a new `Map3` with the same keys and
     *  the transformed values.
     *
     *  @tparam W the type of the transformed values
     *  @param f the transformation function, applied to each key and its value
     *  @return a three-binding map with the same keys and the transformed values
     */
    override def transform[W](f: (K, V) => W): Map[K, W] = {
      val walue1 = f(key1, value1)
      val walue2 = f(key2, value2)
      val walue3 = f(key3, value3)
      if ((walue1.asInstanceOf[AnyRef] eq value1.asInstanceOf[AnyRef]) &&
          (walue2.asInstanceOf[AnyRef] eq value2.asInstanceOf[AnyRef]) &&
          (walue3.asInstanceOf[AnyRef] eq value3.asInstanceOf[AnyRef])) this.asInstanceOf[Map[K, W]]
      else new Map3(key1, walue1, key2, walue2, key3, walue3)
    }
    /** Returns a hash code computed with MurmurHash3 from the three key/value
     *  pairs, equal to the hash code of any other map with the same bindings.
     */
    override def hashCode(): Int = {
      import scala.util.hashing.MurmurHash3
      var a, b = 0
      val N = 3
      var c = 1

      var h = MurmurHash3.tuple2Hash(key1, value1)
      a += h
      b ^= h
      c *= h | 1

      h = MurmurHash3.tuple2Hash(key2, value2)
      a += h
      b ^= h
      c *= h | 1

      h = MurmurHash3.tuple2Hash(key3, value3)
      a += h
      b ^= h
      c *= h | 1

      h = MurmurHash3.mapSeed
      h = MurmurHash3.mix(h, a)
      h = MurmurHash3.mix(h, b)
      h = MurmurHash3.mixLast(h, c)
      MurmurHash3.finalizeHash(h, N)
    }
  }

  /** An immutable map with exactly four bindings, stored directly in fields.
   *
   *  @tparam K the type of the keys
   *  @tparam V the type of the values
   *  @param key1 the key of the first binding
   *  @param value1 the value of the first binding
   *  @param key2 the key of the second binding
   *  @param value2 the value of the second binding
   *  @param key3 the key of the third binding
   *  @param value3 the value of the third binding
   *  @param key4 the key of the fourth binding
   *  @param value4 the value of the fourth binding
   */
  @SerialVersionUID(3L)
  final class Map4[K, +V](key1: K, value1: V, key2: K, value2: V, key3: K, value3: V, key4: K, value4: V)
    extends AbstractMap[K, V] with StrictOptimizedIterableOps[(K, V), Iterable, Map[K, V]] with Serializable {

    /** Returns `4`: this map has exactly four bindings. */
    override def size: Int = 4
    /** Returns `4`: the size is always known. */
    override def knownSize: Int = 4
    /** Returns `false`: this map always has four bindings. */
    override def isEmpty: Boolean = false
    /** Returns the value associated with `key`.
     *
     *  @param key the key to look up
     *  @return the value associated with `key`
     *  @throws NoSuchElementException if `key` is not a key of this map
     */
    override def apply(key: K): V =
      if (key == key1) value1
      else if (key == key2) value2
      else if (key == key3) value3
      else if (key == key4) value4
      else throw new NoSuchElementException("key not found: " + key)
    /** Returns `true` if `key` is one of the keys of this map, `false` otherwise.
     *
     *  @param key the key to test
     */
    override def contains(key: K): Boolean = (key == key1) || (key == key2) || (key == key3) || (key == key4)
    /** Returns the value associated with `key` as an option.
     *
     *  @param key the key to look up
     *  @return `Some` of the value associated with `key`, or `None` if `key`
     *          is not a key of this map
     */
    def get(key: K): Option[V] =
      if (key == key1) Some(value1)
      else if (key == key2) Some(value2)
      else if (key == key3) Some(value3)
      else if (key == key4) Some(value4)
      else None
    /** Returns the value associated with `key`, or `default` if `key` is not
     *  present.
     *
     *  @tparam V1 the type of the result, a supertype of `V`
     *  @param key the key to look up
     *  @param default the value to return if `key` is not present; evaluated
     *                 only in that case
     *  @return the value associated with `key`, or the value of `default`
     */
    override def getOrElse [V1 >: V](key: K, default: => V1): V1 =
      if (key == key1) value1
      else if (key == key2) value2
      else if (key == key3) value3
      else if (key == key4) value4
      else default
    /** Returns an iterator over the four key/value pairs of this map. */
    def iterator: Iterator[(K, V)] = new Map4Iterator[(K, V)] {
      override protected def nextResult(k: K, v: V): (K, V) = (k, v)
    }
    /** Returns an iterator over the four keys of this map. */
    override def keysIterator: Iterator[K] = new Map4Iterator[K] {
      override protected def nextResult(k: K, v: V): K = k
    }
    /** Returns an iterator over the four values of this map. */
    override def valuesIterator: Iterator[V] = new Map4Iterator[V] {
      override protected def nextResult(k: K, v: V): V = v
    }

    private abstract class Map4Iterator[A] extends AbstractIterator[A] {
      private var i = 0
      /** Returns `true` if this iterator has not yet passed all four bindings. */
      override def hasNext: Boolean = i < 4
      /** Returns the result for the next binding of the map and advances this
       *  iterator.
       *
       *  @return the result of `nextResult` applied to the next key/value pair
       *  @throws NoSuchElementException if there are no more elements
       */
      override def next(): A = {
        val result = i match {
          case 0 => nextResult(key1, value1)
          case 1 => nextResult(key2, value2)
          case 2 => nextResult(key3, value3)
          case 3 => nextResult(key4, value4)
          case _ => Iterator.empty.next()
        }
        i += 1
        result
      }
      /** Advances this iterator past the next `n` elements and returns this
       *  same iterator, without creating an intermediate one.
       *
       *  `n` is added to the current position without being clamped at 0, so a
       *  negative `n` moves the position back and replays elements already
       *  returned, where the inherited `Iterator.drop` would treat it as 0.
       *
       *  @param n the number of elements to skip
       *  @return this iterator
       */
      override def drop(n: Int): Iterator[A] = { i += n; this }
      /** Returns the iteration result derived from the key and value of a
       *  binding.
       *
       *  @param k the key of the binding
       *  @param v the value of the binding
       *  @return the element this iterator produces for the binding
       */
      protected def nextResult(k: K, v: V @uncheckedVariance): A
    }
    /** Returns a new map with `key` bound to `value`.
     *
     *  If `key` equals one of the keys of this map, returns a new `Map4` with
     *  that key's value replaced; otherwise returns a `HashMap` of the five
     *  bindings.
     *
     *  @tparam V1 the type of the added value, a supertype of `V`
     *  @param key the key
     *  @param value the value
     *  @return a new map of four or five bindings containing `key -> value`
     */
    def updated[V1 >: V](key: K, value: V1): Map[K, V1] =
      if (key == key1)      new Map4(key1, value, key2, value2, key3, value3, key4, value4)
      else if (key == key2) new Map4(key1, value1, key2, value, key3, value3, key4, value4)
      else if (key == key3) new Map4(key1, value1, key2, value2, key3, value, key4, value4)
      else if (key == key4) new Map4(key1, value1, key2, value2, key3, value3, key4, value)
      else HashMap.empty[K, V1].updated(key1,value1).updated(key2, value2).updated(key3, value3).updated(key4, value4).updated(key, value)
    /** Returns a `Map3` of the remaining bindings if `key` is a key of this
     *  map, or this map itself otherwise.
     *
     *  @param key the key to remove
     *  @return a map without a binding for `key`
     */
    def removed(key: K): Map[K, V] =
      if (key == key1)      new Map3(key2, value2, key3, value3, key4, value4)
      else if (key == key2) new Map3(key1, value1, key3, value3, key4, value4)
      else if (key == key3) new Map3(key1, value1, key2, value2, key4, value4)
      else if (key == key4) new Map3(key1, value1, key2, value2, key3, value3)
      else this
    /** Applies `f` to each key/value pair of this map.
     *
     *  @tparam U the result type of `f`; the results are discarded
     *  @param f the function to apply
     */
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1)); f((key2, value2)); f((key3, value3)); f((key4, value4))
    }
    /** Returns `true` if at least one key/value pair of this map satisfies
     *  `p`. The predicate is not applied to the remaining pairs once one
     *  satisfies it.
     *
     *  @param p the predicate to test
     */
    override def exists(p: ((K, V)) => Boolean): Boolean = p((key1, value1)) || p((key2, value2)) || p((key3, value3)) || p((key4, value4))
    /** Returns `true` if all four key/value pairs of this map satisfy `p`.
     *  The predicate is not applied to the remaining pairs once one fails it.
     *
     *  @param p the predicate to test
     */
    override def forall(p: ((K, V)) => Boolean): Boolean = p((key1, value1)) && p((key2, value2)) && p((key3, value3)) && p((key4, value4))
    /** Returns a map containing the key/value pairs of this map that satisfy
     *  `pred` (or fail it, when `isFlipped` is `true`).
     *
     *  @param pred the predicate to test
     *  @param isFlipped if `true`, keeps a pair only when `pred` is not satisfied
     *  @return this map if all four pairs are kept, or a smaller map, down to
     *          the empty map, of the pairs that are
     */
    override protected[collection] def filterImpl(pred: ((K, V)) => Boolean, isFlipped: Boolean): Map[K, V] = {
      var k1, k2, k3 = null.asInstanceOf[K]
      var v1, v2, v3 = null.asInstanceOf[V]
      var n = 0
      if (pred((key1, value1)) != isFlipped) {             { k1 = key1; v1 = value1 };                                                                                         n += 1}
      if (pred((key2, value2)) != isFlipped) { if (n == 0) { k1 = key2; v1 = value2 } else             { k2 = key2; v2 = value2 };                                             n += 1}
      if (pred((key3, value3)) != isFlipped) { if (n == 0) { k1 = key3; v1 = value3 } else if (n == 1) { k2 = key3; v2 = value3 } else             { k3 = key3; v3 = value3};  n += 1}
      if (pred((key4, value4)) != isFlipped) { if (n == 0) { k1 = key4; v1 = value4 } else if (n == 1) { k2 = key4; v2 = value4 } else if (n == 2) { k3 = key4; v3 = value4 }; n += 1}

      n match {
        case 0 => Map.empty
        case 1 => new Map1(k1, v1)
        case 2 => new Map2(k1, v1, k2, v2)
        case 3 => new Map3(k1, v1, k2, v2, k3, v3)
        case 4 => this
      }
    }
    /** Returns a map obtained by applying `f` to each key and its value.
     *
     *  If every transformed value is reference-equal to the current one, this
     *  map itself is returned; otherwise a new `Map4` with the same keys and
     *  the transformed values.
     *
     *  @tparam W the type of the transformed values
     *  @param f the transformation function, applied to each key and its value
     *  @return a four-binding map with the same keys and the transformed values
     */
    override def transform[W](f: (K, V) => W): Map[K, W] = {
      val walue1 = f(key1, value1)
      val walue2 = f(key2, value2)
      val walue3 = f(key3, value3)
      val walue4 = f(key4, value4)
      if ((walue1.asInstanceOf[AnyRef] eq value1.asInstanceOf[AnyRef]) &&
          (walue2.asInstanceOf[AnyRef] eq value2.asInstanceOf[AnyRef]) &&
          (walue3.asInstanceOf[AnyRef] eq value3.asInstanceOf[AnyRef]) &&
          (walue4.asInstanceOf[AnyRef] eq value4.asInstanceOf[AnyRef])) this.asInstanceOf[Map[K, W]]
      else new Map4(key1, walue1, key2, walue2, key3, walue3, key4, walue4)
    }
    private[immutable] def buildTo[V1 >: V](builder: HashMapBuilder[K, V1]): builder.type =
      builder.addOne(key1, value1).addOne(key2, value2).addOne(key3, value3).addOne(key4, value4)
    /** Returns a hash code computed with MurmurHash3 from the four key/value
     *  pairs, equal to the hash code of any other map with the same bindings.
     */
    override def hashCode(): Int = {
      import scala.util.hashing.MurmurHash3
      var a, b = 0
      val N = 4
      var c = 1

      var h = MurmurHash3.tuple2Hash(key1, value1)
      a += h
      b ^= h
      c *= h | 1

      h = MurmurHash3.tuple2Hash(key2, value2)
      a += h
      b ^= h
      c *= h | 1

      h = MurmurHash3.tuple2Hash(key3, value3)
      a += h
      b ^= h
      c *= h | 1

      h = MurmurHash3.tuple2Hash(key4, value4)
      a += h
      b ^= h
      c *= h | 1

      h = MurmurHash3.mapSeed
      h = MurmurHash3.mix(h, a)
      h = MurmurHash3.mix(h, b)
      h = MurmurHash3.mixLast(h, c)
      MurmurHash3.finalizeHash(h, N)
    }
  }
}

/** Explicit instantiation of the `Map` trait to reduce class file size in subclasses.
 *
 *  @tparam K the type of the keys in this map
 *  @tparam V the type of the values associated with the keys
 */
abstract class AbstractMap[K, +V] extends scala.collection.AbstractMap[K, V] with Map[K, V]

private[immutable] final class MapBuilderImpl[K, V] extends ReusableBuilder[(K, V), Map[K, V]] {
  private var elems: Map[K, V] = Map.empty
  private var switchedToHashMapBuilder: Boolean = false
  private var hashMapBuilder: HashMapBuilder[K, V] = compiletime.uninitialized

  private[immutable] def getOrElse[V0 >: V](key: K, value: V0): V0 =
    if (hashMapBuilder ne null) hashMapBuilder.getOrElse(key, value)
    else elems.getOrElse(key, value)

  /** Clears the contents of this builder: resets the accumulated map to the
   *  empty map and clears the underlying `HashMap` builder, if one was
   *  created. The builder can then be reused.
   */
  override def clear(): Unit = {
    elems = Map.empty
    if (hashMapBuilder != null) {
      hashMapBuilder.clear()
    }
    switchedToHashMapBuilder = false
  }

  /** Returns the map built from the key/value pairs added so far: a
   *  specialized small map for up to four distinct keys, or the result of the
   *  underlying `HashMap` builder beyond that.
   */
  override def result(): Map[K, V] =
    if (switchedToHashMapBuilder) hashMapBuilder.result() else elems

  /** Adds the binding `key -> value` to this builder, replacing any previous
   *  binding for `key`.
   *
   *  Up to four distinct keys are accumulated in the specialized small maps;
   *  adding a fifth distinct key switches this builder to an underlying
   *  `HashMap` builder.
   *
   *  @param key the key
   *  @param value the value
   *  @return this builder
   */
  def addOne(key: K, value: V): this.type = {
    if (switchedToHashMapBuilder) {
      hashMapBuilder.addOne(key, value)
    } else if (elems.size < 4) {
      elems = elems.updated(key, value)
    } else {
      // assert(elems.size == 4)
      if (elems.contains(key)) {
        elems = elems.updated(key, value)
      } else {
        switchedToHashMapBuilder = true
        if (hashMapBuilder == null) {
          hashMapBuilder = new HashMapBuilder
        }
        elems.asInstanceOf[Map4[K, V]].buildTo(hashMapBuilder)
        hashMapBuilder.addOne(key, value)
      }
    }

    this
  }

  /** Returns this builder after adding the key/value pair `elem`, replacing any
   *  previous binding for its key.
   *
   *  @param elem the key/value pair to add
   *  @return this builder
   */
  def addOne(elem: (K, V)) = addOne(elem._1, elem._2)

  /** Adds all key/value pairs of `xs` to this builder.
   *
   *  @param xs the key/value pairs to add
   *  @return this builder
   */
  override def addAll(xs: IterableOnce[(K, V)]^): this.type =
    if (switchedToHashMapBuilder) {
      hashMapBuilder.addAll(xs)
      this
    } else {
      super.addAll(xs)
    }
}
