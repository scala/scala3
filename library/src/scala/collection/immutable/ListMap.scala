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

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable.ReusableBuilder
import scala.collection.generic.DefaultSerializable
import scala.runtime.Statics.releaseFence
import scala.util.hashing.MurmurHash3

/** This class implements immutable maps using a list-based data structure. List map iterators and
 *  traversal methods visit key-value pairs in the order they were first inserted.
 *
 *  Entries are stored internally in reversed insertion order, which means the newest key is at the
 *  head of the list. As such, methods such as `head` and `tail` are O(n), while `last` and `init`
 *  are O(1). Other operations, such as inserting or removing entries, are also O(n), which makes
 *  this collection suitable only for a small number of elements.
 *
 *  Instances of `ListMap` represent empty maps; they can be either created by calling the
 *  constructor directly, or by applying the function `ListMap.empty`.
 *
 *  @tparam K the type of the keys contained in this list map
 *  @tparam V the type of the values associated with the keys
 *
 *  @define Coll ListMap
 *  @define coll list map
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
sealed class ListMap[K, +V]
  extends AbstractMap[K, V]
    with SeqMap[K, V]
    with StrictOptimizedMapOps[K, V, ListMap, ListMap[K, V]]
    with MapFactoryDefaults[K, V, ListMap, Iterable]
    with DefaultSerializable {

  /** The factory used to build list maps, the [[ListMap$ `ListMap`]] companion object. */
  override def mapFactory: MapFactory[ListMap] = ListMap

  /** Returns `0`; an instance of `ListMap` itself, as opposed to one of its nodes, is empty. */
  override def size: Int = 0

  /** Returns `true`; an instance of `ListMap` itself, as opposed to one of its nodes, is empty. */
  override def isEmpty: Boolean = true

  /** Returns `0`; the size of an empty list map is known without traversal. */
  override def knownSize: Int = 0
  /** Returns `None`; this map is empty, so it binds no key.
   *
   *  @param key the key to look for; never used
   */
  def get(key: K): Option[V] = None

  /** Returns a map binding `key` to `value` alone, since this map is empty.
   *
   *  @tparam V1 the type of the value, a supertype of `V`
   *  @param key the key to bind
   *  @param value the value to bind it to
   *  @return a one-entry list map whose remainder is this empty map
   */
  def updated[V1 >: V](key: K, value: V1): ListMap[K, V1] = new ListMap.Node[K, V1](key, value, this)

  /** Returns this map itself; an empty map has no binding to remove.
   *
   *  @param key the key to remove; never used
   */
  def removed(key: K): ListMap[K, V] = this

  /** Returns an iterator over the key-value pairs of this map, in the order in which
   *  their keys were first inserted.
   *
   *  The entries are held in reverse insertion order, so the whole chain is walked and
   *  reversed into a `List` before the iterator is returned; this costs `O(n)`.
   */
  def iterator: Iterator[(K, V)] = {
    var curr: ListMap[K, V] = this
    var res: List[(K, V)] = Nil
    while (curr.nonEmpty) {
      res = (curr.key, curr.value) :: res
      curr = curr.next
    }
    res.iterator
  }

  /** Returns the keys of this map, in the order in which they were first inserted.
   *
   *  Unlike the inherited implementation this returns a strict [[List]] rather than a
   *  view, built by walking and reversing the chain in `O(n)`.
   */
  @nowarn("msg=overriding method keys")
  override def keys: Iterable[K] = {
    var curr: ListMap[K, V] = this
    var res: List[K] = Nil
    while (curr.nonEmpty) {
      res = curr.key :: res
      curr = curr.next
    }
    res
  }

  /** Returns the hash code of this map, computed from its bindings and independent of
   *  their order.
   *
   *  The chain is traversed in its internal, reversed order, which the order-insensitive
   *  hash of a map makes harmless and which avoids reversing the entries first.
   */
  override def hashCode(): Int = {
    if (isEmpty) MurmurHash3.emptyMapHash
    else {
      // Can't efficiently override foreachEntry directly in ListMap because it would need to preserve iteration
      // order be reversing the list first. But mapHash is symmetric so the reversed order is fine here.
      val _reversed = new immutable.AbstractMap[K, V] {
        override def isEmpty: Boolean = ListMap.this.isEmpty
        override def removed(key: K): Map[K, V] = ListMap.this.removed(key)
        override def updated[V1 >: V](key: K, value: V1): Map[K, V1] = ListMap.this.updated(key, value)
        override def get(key: K): Option[V] = ListMap.this.get(key)
        override def iterator: Iterator[(K, V)] = ListMap.this.iterator
        override def foreachEntry[U](f: (K, V) => U): Unit = {
          var curr: ListMap[K, V] = ListMap.this
          while (curr.nonEmpty) {
            f(curr.key, curr.value)
            curr = curr.next
          }
        }
      }
      MurmurHash3.mapHash(_reversed)
    }
  }

  private[immutable] def key: K = throw new NoSuchElementException("key of empty map")
  private[immutable] def value: V = throw new NoSuchElementException("value of empty map")
  private[immutable] def next: ListMap[K, V] = throw new NoSuchElementException("next of empty map")

  /** Applies `op` to the entries of this map and `z`, going right to left in insertion
   *  order.
   *
   *  Since the entries are held newest first, the fold starts at the head of the chain
   *  and runs tail recursively, building neither a reversed copy nor a stack.
   *
   *  @tparam Z the result type of the fold
   *  @param z the start value, combined with the last entry first
   *  @param op the binary operator, applied to an entry and the result accumulated so far
   *  @return the result of inserting `op` between consecutive entries and `z`, or `z`
   *          itself if this map is empty
   */
  override def foldRight[Z](z: Z)(op: ((K, V), Z) => Z): Z = ListMap.foldRightInternal(this, z, op)
  /** The prefix of this map's string representation: `"ListMap"`. */
  override protected def className = "ListMap"

}

/** $factoryInfo
 *
 *  Note that each element insertion takes O(n) time, which means that creating a list map with
 *  n elements will take O(n^2^) time. This makes the builder suitable only for a small number of
 *  elements.
 *
 *  @see ["Scala's Collection Library overview"](https://docs.scala-lang.org/overviews/collections-2.13/concrete-immutable-collection-classes.html#list-maps)
 *  section on `List Maps` for more information.
 *  @define Coll ListMap
 *  @define coll list map
 */
@SerialVersionUID(3L)
object ListMap extends MapFactory[ListMap] {
  /** Represents an entry in the `ListMap`.
   *
   *  @tparam K the type of the keys in this map entry
   *  @tparam V the type of the values in this map entry
   *  @param private[immutable] val key the key for this map entry
   *  @param private[immutable] var _value the value associated with the key
   *  @param private[immutable] var _init the rest of the list map (tail), or `null` during construction
   */
  private[immutable] final class Node[K, V](
    override private[immutable] val key: K,
    private[immutable] var _value: V,
    private[immutable] var _init: ListMap[K, V] | Null
  ) extends ListMap[K, V] {
    releaseFence()

    override private[immutable] def value: V = _value

    /** Returns the number of entries in this map, counted by walking the chain, which costs `O(n)`. */
    override def size: Int = sizeInternal(this, 0)

    @tailrec private def sizeInternal(cur: ListMap[K, V], acc: Int): Int =
      if (cur.isEmpty) acc
      else sizeInternal(cur.next, acc + 1)

    /** Returns `false`; a node always holds at least its own binding. */
    override def isEmpty: Boolean = false

    /** Returns `-1`; the size of a non-empty list map is not known without walking the chain. */
    override def knownSize: Int = -1

    /** Returns the value bound to `k`, searching the chain from the most recently
     *  inserted key towards the oldest, which costs `O(n)`.
     *
     *  @param k the key to look for
     *  @throws NoSuchElementException if no key of this map is `==` to `k`
     */
    @throws[NoSuchElementException]
    override def apply(k: K): V = applyInternal(this, k)

    @tailrec private def applyInternal(cur: ListMap[K, V], k: K): V =
      if (cur.isEmpty) throw new NoSuchElementException("key not found: " + k)
      else if (k == cur.key) cur.value
      else applyInternal(cur.next, k)

    /** Returns the value bound to `k` in a `Some`, or `None` if there is no such
     *  binding; the chain is searched in `O(n)`.
     *
     *  @param k the key to look for
     */
    override def get(k: K): Option[V] = getInternal(this, k)

    @tailrec private def getInternal(cur: ListMap[K, V], k: K): Option[V] =
      if (cur.isEmpty) None
      else if (k == cur.key) Some(cur.value)
      else getInternal(cur.next, k)

    /** Returns `true` if this map binds a key that is `==` to `k`; the chain is searched
     *  in `O(n)`.
     *
     *  @param k the key to look for
     */
    override def contains(k: K): Boolean = containsInternal(this, k)

    @tailrec private def containsInternal(cur: ListMap[K, V], k: K): Boolean =
      if (cur.isEmpty) false
      else if (k == cur.key) true
      else containsInternal(cur.next, k)

    /** Returns a map that binds `k` to `v` and agrees with this map on every other key.
     *
     *  If `k` is already bound, the new binding takes the place of the old one, so `k`
     *  keeps its position in the insertion order; the entries inserted after it are
     *  rebuilt and the ones inserted before it are shared. If `k` is already bound to
     *  the very same value, meaning the same object, this map is returned as is. An
     *  unbound `k` becomes the most recently inserted key.
     *
     *  This costs `O(n)`.
     *
     *  @tparam V1 the type of the new value, a supertype of `V`
     *  @param k the key to bind
     *  @param v the value to bind it to
     *  @return a list map with `k` bound to `v`
     */
    override def updated[V1 >: V](k: K, v: V1): ListMap[K, V1] = {

      var index = -1 // the index (in reverse) where the key to update exists, if it is found
      var found = false // true if the key is found int he map
      var isDifferent = false // true if the key was found and the values are different

      {
        var curr: ListMap[K, V] = this

        while (curr.nonEmpty && !found) {
          if (k == curr.key) {
            found = true
            isDifferent = v.asInstanceOf[AnyRef] ne curr.value.asInstanceOf[AnyRef]
          }
          index += 1
          curr = curr.init
        }
      }

      if (found) {
        if (isDifferent) {
          var newHead: ListMap.Node[K, V1] | Null = null
          var prev: ListMap.Node[K, V1] | Null = null
          var curr: ListMap[K, V1] = this
          var i = 0
          while (i < index) {
            val temp = new ListMap.Node(curr.key, curr.value, null)
            if (prev != null) {
              prev._init = temp
            }
            prev = temp
            curr = curr.init
            if (newHead == null) {
              newHead = prev
            }
            i += 1
          }
          val newNode = new ListMap.Node(curr.key, v, curr.init)
          if (prev != null) {
            prev._init = newNode
          }
          releaseFence()
          if (newHead == null) newNode else newHead
        } else {
          this
        }
      } else {
        new ListMap.Node(k, v, this)
      }
    }

    @tailrec private def removeInternal(k: K, cur: ListMap[K, V], acc: List[ListMap[K, V]]): ListMap[K, V] =
      if (cur.isEmpty) acc.last
      else if (k == cur.key) acc.foldLeft(cur.next) { (t, h) => new Node(h.key, h.value, t) }
      else removeInternal(k, cur.next, cur :: acc)

    /** Returns a map with all bindings of this map except the one for `k`, in their
     *  original insertion order. Returns this map itself if `k` is not bound;
     *  otherwise the entries inserted after `k` are rebuilt and the ones inserted
     *  before it are shared, which costs `O(n)`.
     *
     *  @param k the key to remove
     */
    override def removed(k: K): ListMap[K, V] = removeInternal(k, this, Nil)

    override private[immutable] def next: ListMap[K, V] = _init.nn

    /** Returns the most recently inserted binding of this map, which is this node's own
     *  entry and therefore costs `O(1)`.
     */
    override def last: (K, V) = (key, value)
    /** Returns this map without its most recently inserted binding, which is the chain
     *  below this node and therefore costs `O(1)`.
     */
    override def init: ListMap[K, V] = next

  }

  /** Returns the empty list map.
   *
   *  All calls return the same instance, cast to the requested key and value types.
   *
   *  @tparam K the key type of the map
   *  @tparam V the value type of the map
   */
  def empty[K, V]: ListMap[K, V] = EmptyListMap.asInstanceOf[ListMap[K, V]]

  private object EmptyListMap extends ListMap[Any, Nothing]

  /** Returns a list map containing the bindings of `it`, in the order `it` gives them
   *  out, with a key that occurs more than once kept at the position of its first
   *  occurrence and bound to its last value.
   *
   *  If `it` is already a `ListMap` it is returned unchanged. A `Map`, `MapView` or
   *  `LinkedHashMap` cannot repeat a key, so its entries are chained up directly;
   *  anything else goes through the builder, which costs `O(n^2^)`.
   *
   *  @tparam K the key type
   *  @tparam V the value type
   *  @param it the collection whose bindings are to be contained
   */
  def from[K, V](it: collection.IterableOnce[(K, V)]^): ListMap[K, V] =
    (it: @unchecked) match {
      case lm: ListMap[K, V] => lm
      case lhm: collection.mutable.LinkedHashMap[K, V] =>
        // by directly iterating through LinkedHashMap entries, we save creating intermediate tuples for each
        // key-value pair
        var current: ListMap[K, V] = empty[K, V]
        var firstEntry = lhm._firstEntry
        while (firstEntry ne null) {
          current = new Node(firstEntry.key, firstEntry.value, current)
          firstEntry = firstEntry.later
        }
        current
      case _: collection.Map[K, V] | _: collection.MapView[K, V] =>
        // when creating from a map, we need not handle duplicate keys, so we can just append each key-value to the end
        var current: ListMap[K, V] = empty[K, V]
        val iter = it.iterator
        while (iter.hasNext) {
          val (k, v) = iter.next()
          current = new Node(k, v, current)
        }
        current

      case _ => (newBuilder[K, V] ++= it).result()
    }

  /** Returns a new ListMap builder
   *
   *  The implementation safely handles additions after `result()` without calling `clear()`
   *
   *  @tparam K the map key type
   *  @tparam V the map value type
   *  @return a new `ReusableBuilder` for creating `ListMap` instances
   */
  def newBuilder[K, V]: ReusableBuilder[(K, V), ListMap[K, V]] = new ListMapBuilder[K, V]

  @tailrec private def foldRightInternal[K, V, Z](map: ListMap[K, V], prevValue: Z, op: ((K, V), Z) => Z): Z = {
    if (map.isEmpty) prevValue
    else foldRightInternal(map.init, op(map.last, prevValue), op)
  }
}

/** Builder for ListMap.
 *  $multipleResults
 *
 *  @tparam K the type of the keys in the list map being built
 *  @tparam V the type of the values in the list map being built
 */
private[immutable] final class ListMapBuilder[K, V] extends mutable.ReusableBuilder[(K, V), ListMap[K, V]] {
  private var isAliased: Boolean = false
  private var underlying: ListMap[K, V] = ListMap.empty

  /** Discards everything added so far, so that this builder starts again from the empty
   *  list map; a map already handed out by `result()` is left untouched.
   */
  override def clear(): Unit = {
    underlying = ListMap.empty
    isAliased = false
  }

  /** Returns the list map built so far.
   *
   *  The map is handed out without being copied, so any further addition to this
   *  builder switches to the immutable `updated` and leaves the returned map alone.
   */
  override def result(): ListMap[K, V] = {
    isAliased = true
    releaseFence()
    underlying
  }

  /** Adds the binding `elem` to this builder.
   *
   *  @param elem the key-value pair to add
   *  @return this builder
   */
  override def addOne(elem: (K, V)): this.type = addOne(elem._1, elem._2)

  @tailrec
  private def insertValueAtKeyReturnFound(m: ListMap[K, V], key: K, value: V): Boolean = (m: @unchecked) match {
    case n: ListMap.Node[K, V] =>
      if (n.key == key) {
        n._value = value
        true
      } else {
        insertValueAtKeyReturnFound(n.init, key, value)
      }
    case _ => false
  }

  /** Adds the binding of `key` to `value` to this builder.
   *
   *  If `key` was already added, it keeps the position it had and is rebound to
   *  `value`; otherwise it becomes the most recently inserted key. Either way the
   *  search through the entries added so far costs `O(n)`.
   *
   *  @param key the key to bind
   *  @param value the value to bind it to
   *  @return this builder
   */
  def addOne(key: K, value: V): this.type = {
    if (isAliased) {
      underlying = underlying.updated(key, value)
    } else {
      if (!insertValueAtKeyReturnFound(underlying, key, value)) {
        underlying = new ListMap.Node(key, value, underlying)
      }
    }
    this
  }
  /** Adds all bindings of `xs` to this builder, in the order `xs` gives them out.
   *
   *  A `Map`, `MapView` or `LinkedHashMap` cannot repeat a key, which lets the
   *  duplicate check be skipped or narrowed to the entries already present.
   *
   *  @param xs the bindings to add
   *  @return this builder
   */
  override def addAll(xs: IterableOnce[(K, V)]^): this.type = {
    if (isAliased) {
      super.addAll(xs)
    } else if (underlying.nonEmpty) {
      (xs: @unchecked) match {
        case m: collection.Map[K, V] =>
          // if it is a map, then its keys will not collide with themselves.
          // therefor we only need to check the already-existing elements for collisions.
          // No need to check the entire list

          val iter = m.iterator
          var newUnderlying = underlying
          while (iter.hasNext) {
            val next = iter.next()
            if (!insertValueAtKeyReturnFound(underlying, next._1, next._2)) {
              newUnderlying = new ListMap.Node[K, V](next._1, next._2, newUnderlying)
            }
          }
          underlying = newUnderlying
          this

        case _ =>
          super.addAll(xs)
      }
    } else (xs: @unchecked) match {
      case lhm: collection.mutable.LinkedHashMap[K, V] =>
        // special-casing LinkedHashMap avoids creating of Iterator and tuples for each key-value
        var firstEntry = lhm._firstEntry
        while (firstEntry ne null) {
          underlying = new ListMap.Node(firstEntry.key, firstEntry.value, underlying)
          firstEntry = firstEntry.later
        }
        this

      case _: collection.Map[K, V] | _: collection.MapView[K, V] =>
        val iter = xs.iterator
        while (iter.hasNext) {
          val (k, v) = iter.next()
          underlying = new ListMap.Node(k, v, underlying)
        }

        this
      case _ =>
        super.addAll(xs)
    }
  }
}
