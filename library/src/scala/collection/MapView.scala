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

import scala.annotation.nowarn
import scala.collection.MapView.SomeMapOps
import scala.collection.mutable.Builder

/** A non-strict view of the key-value pairs of a map.
 *
 *  Transformation operations such as `mapValues` and `filterKeys` wrap this
 *  view without evaluating anything: their function or predicate is applied
 *  each time an entry is looked up or iterated over. Lookups and iteration
 *  read the underlying map anew on each access, so changes to it are visible
 *  through the view.
 *
 *  @tparam K the type of keys
 *  @tparam V the type of values
 */
trait MapView[K, +V]
  extends MapOps[K, V, ({ type l[X, Y] = View[(X, Y)] })#l, View[(K, V)]]
    with View[(K, V)] {

  /** Returns this map view itself, since it is already a view. */
  override def view: MapView[K, V]^{this} = this

  // Ideally this returns a `View`, but bincompat
  /** Creates a view over all keys of this map.
   *
   *  @return the keys of this map as a view.
   */
  @nowarn("msg=overriding method keys")
  override def keys: Iterable[K]^{this} = new MapView.Keys(this)

  // Ideally this returns a `View`, but bincompat
  /** Creates a view over all values of this map.
   *
   *  @return the values of this map as a view.
   */
  override def values: Iterable[V]^{this} = new MapView.Values(this)

  /** Filters this map by retaining only keys satisfying a predicate.
   *  @param  p   the predicate used to test keys
   *  @return an immutable map consisting only of those key value pairs of this map where the key satisfies
   *          the predicate `p`. The resulting map wraps the original map without copying any elements.
   */
  override def filterKeys(p: K => Boolean): MapView[K, V]^{this, p} = new MapView.FilterKeys(this, p)

  /** Transforms this map by applying a function to every retrieved value.
   *
   *  @tparam W the type of the transformed values
   *  @param  f   the function used to transform values of this map.
   *  @return a map view which maps every key of this map
   *          to `f(this(key))`. The resulting map wraps the original map without copying any elements.
   */
  override def mapValues[W](f: V => W): MapView[K, W]^{this, f} = new MapView.MapValues(this, f)

  /** Returns a map view of the entries of this map that satisfy the predicate
   *  `pred`.
   *
   *  The predicate is evaluated lazily, each time the returned view is
   *  traversed or a key is looked up in it.
   *
   *  @param pred the predicate used to test entries
   */
  override def filter(pred: ((K, V)) => Boolean): MapView[K, V]^{this, pred} = new MapView.Filter(this, isFlipped = false, pred)

  /** Returns a map view of the entries of this map that do not satisfy the
   *  predicate `pred`.
   *
   *  The predicate is evaluated lazily, each time the returned view is
   *  traversed or a key is looked up in it.
   *
   *  @param pred the predicate used to test entries
   */
  override def filterNot(pred: ((K, V)) => Boolean): MapView[K, V]^{this, pred} = new MapView.Filter(this, isFlipped = true, pred)

  /** Returns a pair of map views: the first of the entries of this map that
   *  satisfy `p`, the second of those that do not.
   *
   *  Equivalent to `(filter(p), filterNot(p))`: both views are lazy, and `p`
   *  is evaluated each time either of them is traversed or looked up in.
   *
   *  @param p the predicate on which to partition entries
   */
  override def partition(p: ((K, V)) => Boolean): (MapView[K, V]^{this, p}, MapView[K, V]^{this, p}) = (filter(p), filterNot(p))

  /** Returns a map view over the same entries that applies the side-effecting
   *  function `f` to each entry as it is accessed.
   *
   *  `f` is invoked during iteration over the returned view and on each
   *  successful lookup in it.
   *
   *  @tparam U the result type of `f`; the result is discarded
   *  @param f the function to apply to each entry when it is accessed
   */
  override def tapEach[U](f: ((K, V)) => U): MapView[K, V]^{this, f} = new MapView.TapEach(this, f)

  /** Returns the factory used to build map views: the [[MapView]] object. */
  def mapFactory: MapViewFactory = MapView

  /** Returns an empty map view with the same key and value types as this view. */
  override def empty: MapView[K, V] = mapFactory.empty

  /** Creates a non-strict filter of this map view, for use in for-comprehensions.
   *
   *  `p` is not evaluated until one of the returned object's operations is
   *  called, and then once per entry tested.
   *
   *  @param p the predicate used to test entries
   *  @return an object whose `map`, `flatMap`, `foreach`, and `withFilter`
   *          operations apply only to the entries of this view that satisfy `p`
   */
  override def withFilter(p: ((K, V)) => Boolean): MapOps.WithFilter[K, V, View, ({ type l[X, Y] = View[(X, Y)] })#l]^{this, p} = new MapOps.WithFilter(this, p)

  /** Returns the string `"MapView(<not computed>)"`; the elements of this view are not evaluated. */
  override def toString(): String = super[View].toString

  /** The prefix of this view's string representation, `"MapView"`. */
  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected def stringPrefix: String = "MapView"
}

object MapView extends MapViewFactory {

  /** An `IterableOps` whose collection type and collection type constructor are unknown. */
  type SomeIterableConstr[X, Y] = IterableOps[?, AnyConstr, ?]
  /** A `MapOps` whose collection type and collection type constructor are (mostly) unknown. */
  type SomeMapOps[K, +V] = MapOps[K, V, SomeIterableConstr, ?]

  @SerialVersionUID(3L)
  private val EmptyMapView: MapView[Any, Nothing] = new AbstractMapView[Any, Nothing] {
    override def get(key: Any): Option[Nothing] = None
    override def iterator: Iterator[Nothing] = Iterator.empty[Nothing]
    override def knownSize: Int = 0
    override def isEmpty: Boolean = true
    override def filterKeys(p: Any => Boolean): MapView[Any, Nothing] = this
    override def mapValues[W](f: Nothing => W): MapView[Any, W] = this // TODO: W is originally Nothing in return type, but breaks CC
    override def filter(pred: ((Any, Nothing)) => Boolean): MapView[Any, Nothing] = this
    override def filterNot(pred: ((Any, Nothing)) => Boolean): MapView[Any, Nothing] = this
    override def partition(p: ((Any, Nothing)) => Boolean): (MapView[Any, Nothing], MapView[Any, Nothing]) = (this, this)
  }

  /** A map view that does not transform the entries of the underlying map.
   *
   *  @tparam K the type of keys
   *  @tparam V the type of values
   *  @param underlying the map being viewed
   */
  @SerialVersionUID(3L)
  class Id[K, +V](underlying: SomeMapOps[K, V]^) extends AbstractMapView[K, V] {
    /** Returns the value associated with `key` in the underlying map.
     *
     *  @param key the key to look up
     *  @return `Some` of the value bound to `key` in the underlying map, or
     *          `None` if `key` has no binding
     */
    def get(key: K): Option[V] = underlying.get(key)
    /** Returns an iterator over the entries of the underlying map. */
    def iterator: Iterator[(K, V)]^{this} = underlying.iterator
    /** Returns the number of entries in the underlying map, if it can be computed in constant time, otherwise -1. */
    override def knownSize: Int = underlying.knownSize
    /** Returns `true` if the underlying map is empty. */
    override def isEmpty: Boolean = underlying.isEmpty
  }

  // Ideally this is public, but bincompat
  @SerialVersionUID(3L)
  private class Keys[K](underlying: SomeMapOps[K, ?]^) extends AbstractView[K] {
    def iterator: Iterator[K]^{this} = underlying.keysIterator
    override def knownSize: Int = underlying.knownSize
    override def isEmpty: Boolean = underlying.isEmpty
  }

  // Ideally this is public, but bincompat
  @SerialVersionUID(3L)
  private class Values[+V](underlying: SomeMapOps[?, V]^) extends AbstractView[V] {
    def iterator: Iterator[V]^{this} = underlying.valuesIterator
    override def knownSize: Int = underlying.knownSize
    override def isEmpty: Boolean = underlying.isEmpty
  }

  /** A map view that transforms every retrieved value of the underlying map
   *  with the function `f`.
   *
   *  `f` is evaluated each time a value is accessed; results are not cached.
   *
   *  @tparam K the type of keys
   *  @tparam V the type of values of the underlying map
   *  @tparam W the type of values of this view
   *  @param underlying the map being viewed
   *  @param f the function applied to each retrieved value
   */
  @SerialVersionUID(3L)
  class MapValues[K, +V, +W](underlying: SomeMapOps[K, V]^, f: V => W) extends AbstractMapView[K, W] {
    /** Returns an iterator over the entries of the underlying map, applying `f` to each value as it is retrieved. */
    def iterator: Iterator[(K, W)]^{this} = underlying.iterator.map(kv => (kv._1, f(kv._2)))
    /** Returns the transformed value associated with `key`.
     *
     *  Looks up `key` in the underlying map and applies `f` to the value
     *  found; `f` is evaluated on each call, and its result is not cached.
     *
     *  @param key the key to look up
     *  @return `Some(f(v))` where `v` is the value bound to `key` in the
     *          underlying map, or `None` if `key` has no binding
     */
    def get(key: K): Option[W] = underlying.get(key).map(f)
    /** Returns the number of entries in the underlying map, if it can be computed in constant time, otherwise -1. */
    override def knownSize: Int = underlying.knownSize
    /** Returns `true` if the underlying map is empty, without invoking `f`. */
    override def isEmpty: Boolean = underlying.isEmpty
  }

  /** A map view of the entries of the underlying map whose key satisfies the
   *  predicate `p`.
   *
   *  `p` is evaluated each time a key is tested; results are not cached.
   *
   *  @tparam K the type of keys
   *  @tparam V the type of values
   *  @param underlying the map being viewed
   *  @param p the predicate used to test keys
   */
  @SerialVersionUID(3L)
  class FilterKeys[K, +V](underlying: SomeMapOps[K, V]^, p: K => Boolean) extends AbstractMapView[K, V] {
    /** Returns an iterator over the entries of the underlying map whose key satisfies `p`, evaluating `p` on each key as iteration proceeds. */
    def iterator: Iterator[(K, V)]^{this} = underlying.iterator.filter { case (k, _) => p(k) }
    /** Returns the value associated with `key` if `key` satisfies `p`.
     *
     *  `p(key)` is evaluated on each call; when it is `false`, the underlying
     *  map is not consulted.
     *
     *  @param key the key to look up
     *  @return `Some` of the value bound to `key` if `key` satisfies `p` and
     *          has a binding in the underlying map, `None` otherwise
     */
    def get(key: K): Option[V] = if (p(key)) underlying.get(key) else None
    /** Returns 0 if the underlying map is known to be empty, otherwise -1, since the number of keys satisfying `p` is not known without traversal. */
    override def knownSize: Int = if (underlying.knownSize == 0) 0 else super.knownSize
    /** Returns `true` if no key of the underlying map satisfies `p`, evaluating `p` on the keys until one satisfies it. */
    override def isEmpty: Boolean = iterator.isEmpty
  }

  /** A map view of the entries of the underlying map that satisfy the
   *  predicate `p`, or, if `isFlipped` is `true`, of those that do not.
   *
   *  Implements both `filter` and `filterNot` of [[MapView]]; `p` is evaluated
   *  each time an entry is tested, and results are not cached.
   *
   *  @tparam K the type of keys
   *  @tparam V the type of values
   *  @param underlying the map being viewed
   *  @param isFlipped if `true`, the sense of `p` is inverted
   *  @param p the predicate used to test entries
   */
  @SerialVersionUID(3L)
  class Filter[K, +V](underlying: SomeMapOps[K, V]^, isFlipped: Boolean, p: ((K, V)) => Boolean) extends AbstractMapView[K, V] {
    /** Returns an iterator over the entries of the underlying map that pass the filter, evaluating `p` on each entry as iteration proceeds. */
    def iterator: Iterator[(K, V)]^{this} = underlying.iterator.filterImpl(p, isFlipped)
    /** Returns the value associated with `key` if the resulting entry passes
     *  the filter.
     *
     *  `p` is evaluated on each call, on the entry `(key, v)` where `v` is the
     *  value bound to `key`; it is not evaluated when `key` has no binding.
     *
     *  @param key the key to look up
     *  @return `Some` of the value bound to `key` if the entry passes the
     *          filter, `None` if it does not or if `key` has no binding
     */
    def get(key: K): Option[V] = underlying.get(key) match {
      case s @ Some(v) if p((key, v)) != isFlipped => s
      case _ => None
    }
    /** Returns 0 if the underlying map is known to be empty, otherwise -1, since the number of entries passing the filter is not known without traversal. */
    override def knownSize: Int = if (underlying.knownSize == 0) 0 else super.knownSize
    /** Returns `true` if no entry of the underlying map passes the filter, evaluating `p` on the entries until one passes. */
    override def isEmpty: Boolean = iterator.isEmpty
  }

  /** A map view over the entries of the underlying map that applies the
   *  side-effecting function `f` to each entry as it is accessed.
   *
   *  @tparam K the type of keys
   *  @tparam V the type of values
   *  @tparam U the result type of `f`; the result is discarded
   *  @param underlying the map being viewed
   *  @param f the function applied to each accessed entry
   */
  @SerialVersionUID(3L)
  class TapEach[K, +V, +U](underlying: SomeMapOps[K, V]^, f: ((K, V)) => U) extends AbstractMapView[K, V] {
    /** Returns the value associated with `key`, applying `f` to the entry for
     *  its side effect if a value is found.
     *
     *  `f` is called with `(key, v)` where `v` is the value bound to `key`;
     *  it is not called when `key` has no binding.
     *
     *  @param key the key to look up
     *  @return `Some` of the value bound to `key` in the underlying map, or
     *          `None` if `key` has no binding
     */
    override def get(key: K): Option[V] = {
      underlying.get(key) match {
        case s @ Some(v) =>
          f((key, v))
          s
        case None => None
      }
    }
    /** Returns an iterator over the entries of the underlying map, applying `f` to each entry as it is produced. */
    override def iterator: Iterator[(K, V)]^{this} = underlying.iterator.tapEach(f)
    /** Returns the number of entries in the underlying map, if it can be computed in constant time, otherwise -1. */
    override def knownSize: Int = underlying.knownSize
    /** Returns `true` if the underlying map is empty, without invoking `f`. */
    override def isEmpty: Boolean = underlying.isEmpty
  }

  /** Returns a builder for map views.
   *
   *  The builder is strict: it collects the added entries into a
   *  `mutable.HashMap` and returns a view of that map.
   *
   *  @tparam X the type of keys
   *  @tparam Y the type of values
   */
  override def newBuilder[X, Y]: Builder[(X, Y), MapView[X, Y]] = mutable.HashMap.newBuilder[X, Y].mapResult(_.view)

  /** Returns the empty map view.
   *
   *  Every call returns the same instance, cast to the requested key and
   *  value types.
   *
   *  @tparam K the type of keys
   *  @tparam V the type of values
   */
  override def empty[K, V]: MapView[K, V] = EmptyMapView.asInstanceOf[MapView[K, V]]

  /** Returns a view of the key-value pairs of `it`.
   *
   *  Note that the result is a plain `View` of pairs, not a `MapView`.
   *
   *  @tparam K the type of keys
   *  @tparam V the type of values
   *  @param it the collection of key-value pairs to view
   */
  override def from[K, V](it: IterableOnce[(K, V)]^): View[(K, V)]^{it} = View.from(it)

  /** Returns a map view of `it`.
   *
   *  If `it` is already a `MapView` it is returned itself; otherwise it is
   *  wrapped in a [[MapView.Id]].
   *
   *  @tparam K the type of keys
   *  @tparam V the type of values
   *  @param it the map to view
   */
  override def from[K, V](it: SomeMapOps[K, V]): MapView[K, V] = it match {
    case mv: MapView[K @unchecked, V @unchecked] => mv
    case other => new MapView.Id(other)
  }

  /** Returns a map view of the given key-value pairs.
   *
   *  The pairs are first collected, strictly, into an immutable map, so a
   *  later pair with the same key as an earlier one overrides it; the result
   *  is a view of that map.
   *
   *  @tparam K the type of keys
   *  @tparam V the type of values
   *  @param elems the key-value pairs
   */
  override def apply[K, V](elems: (K, V)*): MapView[K, V] = from(elems.toMap)
}

/** A factory of map views.
 *
 *  Adds to [[scala.collection.MapFactory]] operations that produce a
 *  [[MapView]] rather than a plain view of key-value pairs.
 */
trait MapViewFactory extends collection.MapFactory[({ type l[X, Y] = View[(X, Y)]})#l] {

  /** Returns a builder for map views.
   *
   *  @tparam X the type of keys
   *  @tparam Y the type of values
   *  @return a builder producing a `MapView` of the entries added to it
   */
  def newBuilder[X, Y]: Builder[(X, Y), MapView[X, Y]]

  /** Returns an empty map view.
   *
   *  @tparam X the type of keys
   *  @tparam Y the type of values
   */
  def empty[X, Y]: MapView[X, Y]

  /** Returns a map view of `it`.
   *
   *  @tparam K the type of keys
   *  @tparam V the type of values
   *  @param it the map to view
   */
  def from[K, V](it: SomeMapOps[K, V]): MapView[K, V]

  /** Returns a map view of the given key-value pairs.
   *
   *  The pairs are first collected, strictly, into an immutable map, so a
   *  later pair with the same key as an earlier one overrides it; the result
   *  is a view of that map.
   *
   *  @tparam K the type of keys
   *  @tparam V the type of values
   *  @param elems the key-value pairs
   */
  override def apply[K, V](elems: (K, V)*): MapView[K, V] = from(elems.toMap)
}

/** Explicit instantiation of the `MapView` trait to reduce class file size in subclasses. */
@SerialVersionUID(3L)
abstract class AbstractMapView[K, +V] extends AbstractView[(K, V)] with MapView[K, V]

