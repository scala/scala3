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
package mutable

import scala.language.`2.13`
import language.experimental.captureChecking
import scala.annotation.{tailrec, nowarn}
import scala.collection.generic.DefaultSerializable
import scala.collection.immutable.List

/** A simple mutable map backed by a list, so it preserves insertion order.
  *
  *  @tparam K    the type of the keys contained in this list map.
  *  @tparam V    the type of the values assigned to keys in this list map.
  *
  *  @define Coll `mutable.ListMap`
  *  @define coll mutable list map
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  *  @define orderDependent
  *  @define orderDependentFold
  */
@deprecated("Use an immutable.ListMap assigned to a var instead of mutable.ListMap", "2.13.0")
class ListMap[K, V]
  extends AbstractMap[K, V]
    with MapOps[K, V, ListMap, ListMap[K, V]]
    with StrictOptimizedIterableOps[(K, V), Iterable, ListMap[K, V]]
    with StrictOptimizedMapOps[K, V, ListMap, ListMap[K, V]]
    with MapFactoryDefaults[K, V, ListMap, Iterable]
    with DefaultSerializable {

  /** The factory used to build mutable list maps, the [[ListMap$ `ListMap`]] companion object. */
  override def mapFactory: MapFactory[ListMap] = ListMap

  private var elems: List[(K, V)] = List()
  private var siz: Int = 0

  /** Returns the value associated with a key, wrapped in `Some`, or `None` if
   *  the key is not in this map.
   *
   *  Searches the backing list linearly, taking time proportional to the size
   *  of this map.
   *
   *  @param key the key to look up
   *  @return `Some(value)` if `key` is bound to `value` in this map, `None` otherwise
   */
  def get(key: K): Option[V] = elems find (_._1 == key) map (_._2)
  /** Returns an iterator over the key/value bindings of this map, in the
   *  order of the backing list.
   */
  def iterator: Iterator[(K, V)] = elems.iterator

  /** Adds a new key/value binding to this map, or replaces the value if the
   *  key is already present.
   *
   *  Removes any existing binding for the key and prepends the new binding to
   *  the backing list, taking time proportional to the size of this map. If
   *  the key was already present, the originally stored key instance is kept.
   *
   *  @param kv the key/value pair to add
   *  @return this map
   */
  final override def addOne(kv: (K, V)) = {
    val (e, key0) = remove(kv._1, elems, List())
    elems = (key0, kv._2) :: e
    siz += 1; this
  }

  /** Removes the binding for a key from this map, if present.
   *
   *  Takes time proportional to the size of this map.
   *
   *  @param key the key to remove
   *  @return this map
   */
  final override def subtractOne(key: K) = { elems = remove(key, elems, List())._1; this }

  @tailrec
  private def remove(key: K, elems: List[(K, V)], acc: List[(K, V)]): (List[(K, V)], K) = {
    if (elems.isEmpty) (acc, key)
    else if (elems.head._1 == key) { siz -= 1; (acc ::: elems.tail, elems.head._1) }
    else remove(key, elems.tail, elems.head :: acc)
  }

  /** Removes all bindings from this map. */
  final override def clear(): Unit = { elems = List(); siz = 0 }

  /** The number of key/value bindings in this map. */
  final override def size: Int = siz
  /** The number of key/value bindings in this map; always known, never `-1`. */
  override def knownSize: Int = size
  /** Tests whether this map contains no bindings. */
  override def isEmpty: Boolean = size == 0
  /** The prefix used in the string representation of this map, `"ListMap"`. */
  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected def stringPrefix = "ListMap"
}

/** $factoryInfo
  *  @define Coll `mutable.ListMap`
  *  @define coll mutable list map
  */
@SerialVersionUID(3L)
@deprecated("Use an immutable.ListMap assigned to a var instead of mutable.ListMap", "2.13.0")
object ListMap extends MapFactory[ListMap] {
  /** Creates a new empty mutable list map.
   *
   *  @tparam K the key type of the map
   *  @tparam V the value type of the map
   *  @return a new empty `ListMap[K, V]`
   */
  def empty[K, V]: ListMap[K, V] = new ListMap[K, V]
  /** Creates a new mutable list map containing the key/value pairs of the
   *  given collection.
   *
   *  If `it` contains a key more than once, the last value for that key wins.
   *
   *  @tparam K the key type of the map
   *  @tparam V the value type of the map
   *  @param it the collection of key/value pairs
   *  @return a new list map containing the pairs of `it`
   */
  def from[K, V](it: IterableOnce[(K, V)]^): ListMap[K,V] = Growable.from(empty[K, V], it)
  /** Returns a new builder that accumulates key/value pairs into a mutable list map.
   *
   *  @tparam K the key type of the map
   *  @tparam V the value type of the map
   *  @return a builder for a `ListMap[K, V]`
   */
  def newBuilder[K, V]: Builder[(K, V), ListMap[K,V]] = new GrowableBuilder(empty[K, V])
}
