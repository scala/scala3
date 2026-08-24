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
package mutable

import scala.language.`2.13`
import language.experimental.captureChecking
import scala.annotation.{nowarn, tailrec}
import scala.collection.generic.DefaultSerializable
import scala.util.hashing.MurmurHash3


/** This class implements mutable maps using a hashtable.
 *  The iterator and all traversal methods of this class visit elements in the order they were inserted.
 *
 *  @tparam K    the type of the keys contained in this hash map.
 *  @tparam V    the type of the values assigned to keys in this hash map.
 *
 *  @define Coll `LinkedHashMap`
 *  @define coll linked hash map
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 *  @define orderDependent
 *  @define orderDependentFold
 */
@deprecatedInheritance("LinkedHashMap will be made final; use .withDefault for the common use case of computing a default value", "2.13.11")
class LinkedHashMap[K, V]
  extends AbstractMap[K, V]
    with SeqMap[K, V]
    with MapOps[K, V, LinkedHashMap, LinkedHashMap[K, V]]
    with StrictOptimizedIterableOps[(K, V), Iterable, LinkedHashMap[K, V]]
    with StrictOptimizedMapOps[K, V, LinkedHashMap, LinkedHashMap[K, V]]
    with MapFactoryDefaults[K, V, LinkedHashMap, Iterable]
    with DefaultSerializable {

  /** The factory used to build linked hash maps, the [[LinkedHashMap$ `LinkedHashMap`]] companion object. */
  override def mapFactory: MapFactory[LinkedHashMap] = LinkedHashMap

  // stepper / keyStepper / valueStepper are not overridden to use XTableStepper because that stepper
  // would not return the elements in insertion order

  private[collection] type Entry = LinkedHashMap.LinkedEntry[K, V]

  private[collection] def _firstEntry: Entry | Null = firstEntry

  /** The first entry in insertion order, the head of the doubly-linked entry
   *  list, or `null` if this map is empty.
   */
  @annotation.stableNull
  protected var firstEntry: Entry | Null = null

  /** The last entry in insertion order, the tail of the doubly-linked entry
   *  list, or `null` if this map is empty.
   */
  @annotation.stableNull
  protected var lastEntry: Entry | Null = null

  /* Uses the same implementation as mutable.HashMap. The hashtable holds the following invariant:
   * - For each i between 0 and table.length, the bucket at table(i) only contains keys whose hash-index is i.
   * - Every bucket is sorted in ascendant hash order
   * - The sum of the lengths of all buckets is equal to contentSize.
   */
  private var table = new Array[Entry | Null](tableSizeFor(LinkedHashMap.defaultinitialSize))

  private var threshold: Int = newThreshold(table.length)

  private var contentSize = 0

  /** Returns the most recently inserted key/value binding of this map.
   *
   *  @return the last binding in insertion order
   *  @throws NoSuchElementException if this map is empty
   */
  override def last: (K, V) =
    if (size > 0) (lastEntry.nn.key, lastEntry.nn.value)
    else throw new NoSuchElementException("Cannot call .last on empty LinkedHashMap")

  /** Returns the most recently inserted key/value binding of this map wrapped
   *  in `Some`, or `None` if this map is empty.
   */
  override def lastOption: Option[(K, V)] =
    if (size > 0) Some((lastEntry.nn.key, lastEntry.nn.value))
    else None

  /** Returns the first inserted key/value binding of this map.
   *
   *  @return the first binding in insertion order
   *  @throws NoSuchElementException if this map is empty
   */
  override def head: (K, V) =
    if (size > 0) (firstEntry.nn.key, firstEntry.nn.value)
    else throw new NoSuchElementException("Cannot call .head on empty LinkedHashMap")

  /** Returns the first inserted key/value binding of this map wrapped in
   *  `Some`, or `None` if this map is empty.
   */
  override def headOption: Option[(K, V)] =
    if (size > 0) Some((firstEntry.nn.key, firstEntry.nn.value))
    else None

  /** The number of key/value bindings in this map. */
  override def size = contentSize
  /** The number of key/value bindings in this map; always known, never `-1`. */
  override def knownSize: Int = size
  /** Tests whether this map contains no bindings. */
  override def isEmpty: Boolean = size == 0

  /** Returns the value associated with a key, wrapped in `Some`, or `None` if
   *  the key is not in this map.
   *
   *  @param key the key to look up
   *  @return `Some(value)` if `key` is bound to `value` in this map, `None` otherwise
   */
  def get(key: K): Option[V] = {
    val e = findEntry(key)
    if (e == null) None
    else Some(e.value)
  }
  /** Grows the hash table, if needed, so that this map can hold `size` bindings
   *  without further resizing.
   *
   *  The table never shrinks: a hint smaller than the current capacity does nothing.
   *
   *  @param size the expected number of bindings
   */
  override def sizeHint(size: Int): Unit = {
    val target = tableSizeFor(((size + 1).toDouble / LinkedHashMap.defaultLoadFactor).toInt)
    if (target > table.length) growTable(target)
  }

  /** Tests whether this map contains a binding for a key.
   *
   *  @param key the key to look up
   *  @return `true` if this map contains a binding for `key`
   */
  override def contains(key: K): Boolean = {
    if (getClass eq classOf[LinkedHashMap[?, ?]])
      findEntry(key) != null
    else
      super.contains(key) // A subclass might override `get`, use the default implementation `contains`.
  }

  /** Adds a new key/value binding to this map, or updates the value if the key
   *  is already present.
   *
   *  If `key` is already in this map, only its value is replaced: the binding
   *  keeps its original position in the iteration order. A new key is appended
   *  at the end of the iteration order.
   *
   *  @param key the key to bind
   *  @param value the value to associate with `key`
   *  @return `Some(previousValue)` if `key` was already bound, `None` otherwise
   */
  override def put(key: K, value: V): Option[V] = put0(key, value, getOld = true) match {
    case null => None
    case sm => sm
  }

  /** Adds a new key/value binding to this map, or updates the value if the key
   *  is already present.
   *
   *  Like [[put]], but does not report the previous value. An existing key
   *  keeps its position in the iteration order; a new key is appended at the end.
   *
   *  @param key the key to bind
   *  @param value the value to associate with `key`
   */
  override def update(key: K, value: V): Unit = put0(key, value, getOld = false)

  /** Removes the binding for a key from this map, if present.
   *
   *  @param key the key to remove
   *  @return `Some(value)` if `key` was bound to `value`, `None` if `key` was not in this map
   */
  override def remove(key: K): Option[V] = removeEntry0(key) match {
    case null => None
    case nd => Some(nd.value)
  }

  /** Returns the value associated with a key, or a default value if the key is
   *  not in this map.
   *
   *  Overridden to avoid the `Option` allocation of the inherited
   *  implementation when this is a plain `LinkedHashMap` (not a subclass).
   *
   *  @tparam V1 the result type, a supertype of this map's value type
   *  @param key the key to look up
   *  @param default the value to return if `key` is not in this map; evaluated
   *                 only in that case
   *  @return the value bound to `key`, or `default` if `key` is not in this map
   */
  override def getOrElse[V1 >: V](key: K, default: => V1): V1 = {
    if (getClass != classOf[LinkedHashMap[?, ?]]) {
      // subclasses of LinkedHashMap might customise `get` ...
      super.getOrElse(key, default)
    } else {
      // .. but in the common case, we can avoid the Option boxing.
      val nd = findEntry(key)
      if (nd eq null) default else nd.value
    }
  }

  /** Returns the value associated with a key; if the key is not in this map,
   *  binds it to the given default value and returns that value.
   *
   *  A newly inserted binding is appended at the end of the iteration order.
   *  When this is a plain `LinkedHashMap` (not a subclass), the key is hashed
   *  and located only once.
   *
   *  @param key the key to look up
   *  @param defaultValue the value to bind to `key` if it is absent; evaluated
   *                      only in that case
   *  @return the value bound to `key`, either previously or by this call
   */
  override def getOrElseUpdate(key: K, defaultValue: => V): V = {
    if (getClass != classOf[LinkedHashMap[?, ?]]) {
      // subclasses of LinkedHashMap might customise `get` ...
      super.getOrElseUpdate(key, defaultValue)
    } else {
      val hash = computeHash(key)
      val idx = index(hash)
      val nd = table(idx) match {
        case null => null
        case nd => nd.findEntry(key, hash)
      }
      if (nd != null) nd.value
      else {
        val table0 = table
        val default = defaultValue
        if (contentSize + 1 >= threshold) growTable(table.length * 2)
        // Avoid recomputing index if the `defaultValue()` or new element hasn't triggered a table resize.
        val newIdx = if (table0 eq table) idx else index(hash)
        put0(key, default, getOld = false, hash, newIdx)
        default
      }
    }
  }

  private def removeEntry0(elem: K): Entry | Null = removeEntry0(elem, computeHash(elem))

  /** Removes a key from this map if it exists
   *
   *  @param elem the element to remove
   *  @param hash the **improved** hash code of `elem` (see `computeHash`)
   *  @return the entry that contained `elem` if it was present, otherwise `null`
   */
  private def removeEntry0(elem: K, hash: Int): Entry | Null = {
    val idx = index(hash)
    table(idx) match {
      case null => null
      case nd if nd.hash == hash && nd.key == elem =>
        // first element matches
        table(idx) = nd.next
        deleteEntry(nd)
        contentSize -= 1
        nd
      case nd =>
        // find an element that matches
        var prev = nd
        var next = nd.next
        while ((next ne null) && next.hash <= hash) {
          if (next.hash == hash && next.key == elem) {
            prev.next = next.next
            deleteEntry(next)
            contentSize -= 1
            return next
          }
          prev = next
          next = next.next
        }
        null
    }
  }

  /** Computes the improved hash of an original (`any.##`) hash.
   *
   *  @param originalHash the original hash code obtained from `any.##`
   *  @return the improved hash code with better bit distribution
   */
  @`inline` private def improveHash(originalHash: Int): Int = {
    originalHash ^ (originalHash >>> 16)
  }
  @`inline` private[collection] def unimproveHash(improvedHash: Int): Int = improveHash(improvedHash)

  /** Computes the improved hash of this key.
   *
   *  @param o the key whose improved hash to compute
   *  @return the improved hash code of `o`, suitable for indexing into the hash table
   */
  @`inline` private def computeHash(o: K): Int = improveHash(o.##)

  @`inline` private def index(hash: Int) = hash & (table.length - 1)

  @`inline` private def findEntry(key: K): Entry | Null = {
    val hash = computeHash(key)
    table(index(hash)) match {
      case null => null
      case nd => nd.findEntry(key, hash)
    }
  }

  /** Adds a single key/value binding to this map, updating the value if the
   *  key is already present.
   *
   *  As with [[put]], an existing key keeps its position in the iteration
   *  order; a new key is appended at the end.
   *
   *  @param kv the key/value pair to add
   *  @return this map
   */
  def addOne(kv: (K, V)): this.type = {
    put(kv._1, kv._2)
    this
  }

  /** Removes the binding for a key from this map, if present.
   *
   *  @param key the key to remove
   *  @return this map
   */
  def subtractOne(key: K): this.type = {
    remove(key)
    this
  }

  private abstract class LinkedHashMapIterator[T] extends AbstractIterator[T] {
    private var cur: Entry | Null = firstEntry
    /** Returns the iterator's result value for the given entry.
     *
     *  @param nd the entry to extract the value from
     *  @return the value this iterator produces for `nd`
     */
    def extract(nd: Entry): T
    /** Tests whether there are more entries to visit. */
    def hasNext: Boolean = cur ne null
    /** Returns the value for the next entry in insertion order.
     *
     *  @throws NoSuchElementException if no more entries remain
     */
    def next(): T =
      if (hasNext) { val r = extract(cur.nn); cur = cur.nn.later; r }
      else Iterator.empty.next()
  }

  /** Returns an iterator over the key/value bindings of this map, in insertion order. */
  def iterator: Iterator[(K, V)] =
    if (size == 0) Iterator.empty
    else new LinkedHashMapIterator[(K, V)] {
      def extract(nd: Entry): (K, V) = (nd.key, nd.value)
    }

  @deprecated("LinkedKeySet is now strict and no longer used in the implementation of .keySet", "3.8.0")
  /** Note that a LinkedKeySet could be strict. */
  protected class LinkedKeySet extends KeySet {
    /** The factory used to build transformed sets, the [[LinkedHashSet$ `LinkedHashSet`]] companion object. */
    override def iterableFactory: IterableFactory[collection.Set] = LinkedHashSet
  }

  /** Returns a set view of the keys of this map, in insertion order.
   *
   *  The view is backed by this map, so later changes to the map are visible
   *  through it. Transformations of the view build linked hash sets.
   */
  override def keySet: collection.Set[K] = new MapOps.LazyKeySet(this) {
    override def iterableFactory: IterableFactory[collection.Set] = LinkedHashSet
  }

  /** Returns an iterator over the keys of this map, in insertion order. */
  override def keysIterator: Iterator[K] =
    if (size == 0) Iterator.empty
    else new LinkedHashMapIterator[K] {
      def extract(nd: Entry): K = nd.key
    }

  private[collection] def entryIterator: Iterator[Entry] =
    if (size == 0) Iterator.empty
    else new LinkedHashMapIterator[Entry] {
      def extract(nd: Entry): Entry = nd
    }


  // Override updateWith for performance, so we can do the update while hashing
  // the input key only once and performing one lookup into the hash table
  /** Updates, inserts, or removes the binding for a key according to a
   *  remapping function.
   *
   *  The function receives `Some(value)` if `key` is currently bound to
   *  `value`, `None` otherwise. If it returns `Some(newValue)`, the binding is
   *  updated in place (an existing key keeps its position in the iteration
   *  order; a new key is appended at the end); if it returns `None`, any
   *  existing binding is removed. When this is a plain `LinkedHashMap` (not a
   *  subclass), the key is hashed and located only once.
   *
   *  @param key the key whose binding to update
   *  @param remappingFunction the function computing the new binding from the current one
   *  @return the value returned by `remappingFunction`: the new value wrapped
   *          in `Some`, or `None` if the binding was removed or absent
   */
  override def updateWith(key: K)(remappingFunction: Option[V] => Option[V]): Option[V] = {
    if (getClass != classOf[LinkedHashMap[?, ?]]) {
      // subclasses of LinkedHashMap might customise `get` ...
      super.updateWith(key)(remappingFunction)
    } else {
      val hash = computeHash(key)
      val indexedHash = index(hash)

      var foundEntry: Entry | Null = null
      var previousEntry: Entry | Null = null
      table(indexedHash) match {
        case null =>
        case nd =>
          @tailrec
          def findEntry(prev: Entry | Null, nd: Entry, k: K, h: Int): Unit = {
            if (h == nd.hash && k == nd.key) {
              previousEntry = prev
              foundEntry = nd
            }
            else if ((nd.next eq null) || (nd.hash > h)) ()
            else findEntry(nd, nd.next.nn, k, h)
          }

          findEntry(null, nd, key, hash)
      }

      val previousValue = foundEntry match {
        case null => None
        case nd => Some(nd.value)
      }

      val nextValue = remappingFunction(previousValue)

      (previousValue, nextValue) match {
        case (None, None) => // do nothing

        case (Some(_), None) =>
          if (previousEntry != null) previousEntry.nn.next = foundEntry.nn.next
          else table(indexedHash) = foundEntry.nn.next
          deleteEntry(foundEntry.nn)
          contentSize -= 1

        case (None, Some(value)) =>
          val newIndexedHash =
            if (contentSize + 1 >= threshold) {
              growTable(table.length * 2)
              index(hash)
            } else indexedHash
          put0(key, value, getOld = false, hash, newIndexedHash)

        case (Some(_), Some(newValue)) => foundEntry.nn.value = newValue
      }
      nextValue
    }
  }

  /** Returns an iterator over the values of this map, in the insertion order of their keys. */
  override def valuesIterator: Iterator[V] =
    if (size == 0) Iterator.empty
    else new LinkedHashMapIterator[V] {
      def extract(nd: Entry): V = nd.value
    }


  /** Applies a function to each key/value binding of this map, in insertion order.
   *
   *  @tparam U the result type of `f`; the results are discarded
   *  @param f the function applied to each binding, as a pair
   */
  override def foreach[U](f: ((K, V)) => U): Unit = {
    var cur = firstEntry
    while (cur ne null) {
      f((cur.key, cur.value))
      cur = cur.later
    }
  }

  /** Applies a function to each key/value binding of this map, in insertion
   *  order, passing key and value as two arguments rather than as a pair.
   *
   *  @tparam U the result type of `f`; the results are discarded
   *  @param f the function applied to each key and value
   */
  override def foreachEntry[U](f: (K, V) => U): Unit = {
    var cur = firstEntry
    while (cur ne null) {
      f(cur.key, cur.value)
      cur = cur.later
    }
  }

  /** Removes all bindings from this map. The hash table keeps its current capacity. */
  override def clear(): Unit = {
    java.util.Arrays.fill(table.asInstanceOf[Array[AnyRef]], null)
    contentSize = 0
    firstEntry = null
    lastEntry = null
  }

  private def tableSizeFor(capacity: Int) =
    (Integer.highestOneBit((capacity - 1).max(4)) * 2).min(1 << 30)

  private def newThreshold(size: Int) = (size.toDouble * LinkedHashMap.defaultLoadFactor).toInt

  /*create a new entry. If table is empty(firstEntry is null), then the
  * new entry will be the firstEntry. If not, just set the new entry to
  * be the lastEntry.
  * */
  private def createNewEntry(key: K, hash: Int, value: V): Entry = {
    val e = new Entry(key, hash, value)
    if (firstEntry eq null) firstEntry = e
    else {
      lastEntry.nn.later = e
      e.earlier = lastEntry
    }
    lastEntry = e
    e
  }

  /** Deletes the entry from the `LinkedHashMap`, set the `earlier` and `later` pointers correctly.
   *
   *  @param e the entry to remove from the `LinkedHashMap`
   */
  private def deleteEntry(e: Entry): Unit = {
    if (e.earlier eq null) firstEntry = e.later
    else e.earlier.nn.later = e.later
    if (e.later eq null) lastEntry = e.earlier
    else e.later.nn.earlier = e.earlier
    e.earlier = null
    e.later = null
    e.next = null
  }

  private def put0(key: K, value: V, getOld: Boolean): Some[V] | Null = {
    if (contentSize + 1 >= threshold) growTable(table.length * 2)
    val hash = computeHash(key)
    val idx = index(hash)
    put0(key, value, getOld, hash, idx)
  }

  private def put0(key: K, value: V, getOld: Boolean, hash: Int, idx: Int): Some[V] | Null = {
    table(idx) match {
      case null =>
        table(idx) = createNewEntry(key, hash, value)
      case old =>
        var prev: Entry | Null = null
        var n: Entry | Null = old
        while ((n ne null) && n.hash <= hash) {
          if (n.hash == hash && key == n.key) {
            val old = n.value
            n.value = value
            return if (getOld) Some(old) else null
          }
          prev = n
          n = n.next
        }
        val nnode = createNewEntry(key, hash, value)
        if (prev eq null) {
          nnode.next = old
          table(idx) = nnode
        } else {
          nnode.next = prev.next
          prev.next = nnode
        }
    }
    contentSize += 1
    null
  }

  private def growTable(newlen: Int): Unit = {
    if (newlen < 0)
      throw new RuntimeException(s"new hash table size $newlen exceeds maximum")
    var oldlen = table.length
    threshold = newThreshold(newlen)
    if (size == 0) table = new Array(newlen)
    else {
      table = java.util.Arrays.copyOf(table, newlen)
      val preLow = new Entry(null.asInstanceOf[K], 0, null.asInstanceOf[V])
      val preHigh = new Entry(null.asInstanceOf[K], 0, null.asInstanceOf[V])
      // Split buckets until the new length has been reached. This could be done more
      // efficiently when growing an already filled table to more than double the size.
      while (oldlen < newlen) {
        var i = 0
        while (i < oldlen) {
          val old = table(i)
          if (old ne null) {
            preLow.next = null
            preHigh.next = null
            var lastLow = preLow
            var lastHigh = preHigh
            var n: Entry | Null = old
            while (n ne null) {
              val next = n.next
              if ((n.hash & oldlen) == 0) { // keep low
                lastLow.next = n
                lastLow = n
              } else { // move to high
                lastHigh.next = n
                lastHigh = n
              }
              n = next
            }
            lastLow.next = null
            if (old ne preLow.next) table(i) = preLow.next
            if (preHigh.next ne null) {
              table(i + oldlen) = preHigh.next
              lastHigh.next = null
            }
          }
          i += 1
        }
        oldlen *= 2
      }
    }
  }

  /** Returns a hash code for this map, combining the hashes of its bindings
   *  without regard to order, so it is consistent with `equals` across map
   *  implementations.
   */
  override def hashCode(): Int = {
    if (isEmpty) MurmurHash3.emptyMapHash
    else {
      val tupleHashIterator = new LinkedHashMapIterator[Any] {
        var hash: Int = 0
        override def hashCode(): Int = hash
        override def extract(nd: Entry): Any = {
          hash = MurmurHash3.tuple2Hash(unimproveHash(nd.hash), nd.value.##)
          this
        }
      }
      MurmurHash3.unorderedHash(tupleHashIterator, MurmurHash3.mapSeed)
    }
  }
  /** The prefix used in the string representation of this map, `"LinkedHashMap"`. */
  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected def stringPrefix = "LinkedHashMap"
}

/** $factoryInfo
 *  @define Coll `LinkedHashMap`
 *  @define coll linked hash map
 */
@SerialVersionUID(3L)
object LinkedHashMap extends MapFactory[LinkedHashMap] {

  /** Creates a new empty linked hash map.
   *
   *  @tparam K the key type of the map
   *  @tparam V the value type of the map
   */
  def empty[K, V] = new LinkedHashMap[K, V]

  /** Creates a new linked hash map containing the key/value pairs of the given
   *  collection, in its iteration order.
   *
   *  If `it` contains a key more than once, the last value for that key wins,
   *  but the key keeps the position of its first occurrence.
   *
   *  @tparam K the key type of the map
   *  @tparam V the value type of the map
   *  @param it the collection of key/value pairs
   */
  def from[K, V](it: collection.IterableOnce[(K, V)]^) = {
    val newlhm = empty[K, V]
    newlhm.sizeHint(it, delta = 0)
    newlhm.addAll(it)
    newlhm
  }

  /** Returns a new builder that accumulates key/value pairs into a linked hash map.
   *
   *  @tparam K the key type of the map
   *  @tparam V the value type of the map
   *  @return a builder for a `LinkedHashMap[K, V]`
   */
  def newBuilder[K, V]: GrowableBuilder[(K, V), LinkedHashMap[K, V]] = new GrowableBuilder(empty[K, V])

  /** Class for the linked hash map entry, used internally.
   *
   *  @tparam K the type of key stored in this entry
   *  @tparam V the type of value stored in this entry
   *  @param key the key for this map entry
   *  @param hash the improved hash code of `key` (see `improveHash`)
   *  @param value the value associated with `key`
   */
  private[mutable] final class LinkedEntry[K, V](val key: K, val hash: Int, var value: V) {
    /** The previous entry in insertion order, or `null` if this is the first entry. */
    var earlier: LinkedEntry[K, V] | Null = null
    /** The next entry in insertion order, or `null` if this is the last entry. */
    var later: LinkedEntry[K, V] | Null = null
    /** The next entry in the same hash bucket, or `null` if this is the last one. */
    var next: LinkedEntry[K, V] | Null = null

    /** Searches this entry and its bucket successors for a key.
     *
     *  Buckets are sorted by ascending hash, so the search stops early once the
     *  entries' hashes exceed `h`.
     *
     *  @param k the key to find
     *  @param h the improved hash code of `k`
     *  @return the entry with key `k`, or `null` if the bucket contains none
     */
    @tailrec
    final def findEntry(k: K, h: Int): LinkedEntry[K, V] | Null =
      if (h == hash && k == key) this
      else if ((next eq null) || (hash > h)) null
      else next.nn.findEntry(k, h)
  }

  /** The default load factor for the hash table. */
  private[collection] final def defaultLoadFactor: Double = 0.75

  /** The default initial capacity for the hash table. */
  private[collection] final def defaultinitialSize: Int = 16
}
