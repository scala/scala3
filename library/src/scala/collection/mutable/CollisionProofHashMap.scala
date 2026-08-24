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

import scala.{unchecked => uc}
import scala.annotation.{implicitNotFound, tailrec, unused}
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.generic.DefaultSerializationProxy
import scala.runtime.Statics

/** This class implements mutable maps using a hashtable with red-black trees in the buckets for good
 *  worst-case performance on hash collisions. An `Ordering` is required for the element type. Equality
 *  as determined by the `Ordering` has to be consistent with `equals` and `hashCode`. Universal equality
 *  of numeric types is not supported (similar to `AnyRefMap`).
 *
 *  @see ["Scala's Collection Library overview"](https://docs.scala-lang.org/overviews/collections-2.13/concrete-mutable-collection-classes.html#hash-tables)
 *  section on `Hash Tables` for more information.
 *
 *  @define Coll `mutable.CollisionProofHashMap`
 *  @define coll mutable collision-proof hash map
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 *
 *  @tparam K the type of the keys contained in this hash map
 *  @tparam V the type of the values associated with the keys
 *  @param initialCapacity the initial capacity of the internal hash table
 *  @param loadFactor the load factor for the hash table, used to determine when to resize
 *  @param ordering the `Ordering` used to compare keys within a bucket's red-black tree
 */
final class CollisionProofHashMap[K, V](initialCapacity: Int, loadFactor: Double)(implicit ordering: Ordering[K])
  extends AbstractMap[K, V]
    with MapOps[K, V, Map, CollisionProofHashMap[K, V]] //--
    with StrictOptimizedIterableOps[(K, V), Iterable, CollisionProofHashMap[K, V]]
    with StrictOptimizedMapOps[K, V, Map, CollisionProofHashMap[K, V]] { //--

  private final def sortedMapFactory: SortedMapFactory[CollisionProofHashMap] = CollisionProofHashMap

  /** Creates a new, empty hash map with the default initial capacity (16) and the default load factor (0.75).
   *
   *  @param ordering the `Ordering` used to compare keys within a bucket's red-black tree
   */
  def this()(implicit ordering: Ordering[K]) = this(CollisionProofHashMap.defaultInitialCapacity, CollisionProofHashMap.defaultLoadFactor)(using ordering)

  import CollisionProofHashMap.Node
  private type RBNode = CollisionProofHashMap.RBNode[K, V]
  private type LLNode = CollisionProofHashMap.LLNode[K, V]

  /** The actual hash table. */
  private var table: Array[Node | Null] = new Array[Node | Null](tableSizeFor(initialCapacity))

  /** The next size value at which to resize (capacity * load factor). */
  private var threshold: Int = newThreshold(table.length)

  private var contentSize = 0

  /** Returns the number of key-value pairs in this map. */
  override def size: Int = contentSize

  @inline private final def computeHash(o: K): Int = {
    // Objects.hashCode is consistent with the requirements, namely:
    // > Universal equality of numeric types is not supported (similar to `AnyRefMap`).
    val h = java.util.Objects.hashCode(o)
    h ^ (h >>> 16)
  }

  @inline private final def index(hash: Int) = hash & (table.length - 1)

  /** Creates a new `CollisionProofHashMap` containing the key-value pairs of `coll`, using
   *  the same key ordering as this map.
   *
   *  @param coll the collection whose key-value pairs are added to the new map
   *  @return a new `CollisionProofHashMap` containing the key-value pairs of `coll`
   */
  override protected def fromSpecific(coll: (IterableOnce[(K, V)] @uncheckedVariance)^): CollisionProofHashMap[K, V] @uncheckedVariance = CollisionProofHashMap.from(coll)
  /** Returns a new builder for a `CollisionProofHashMap` with the same key ordering as this map. */
  override protected def newSpecificBuilder: Builder[(K, V), CollisionProofHashMap[K, V]] @uncheckedVariance = CollisionProofHashMap.newBuilder[K, V]

  /** Returns a new, empty `CollisionProofHashMap` with the same key ordering as this map and
   *  the default initial capacity and load factor.
   */
  override def empty: CollisionProofHashMap[K, V] = new CollisionProofHashMap[K, V]

  /** Tests whether this map contains a binding for a key.
   *
   *  @param key the key to test for membership
   *  @return `true` if this map contains a binding for `key`, `false` otherwise
   */
  override def contains(key: K): Boolean = findNode(key) ne null

  /** Returns the value associated with `key` in this map, wrapped in `Some`, or `None` if
   *  `key` is not present.
   *
   *  @param key the key to look up
   */
  def get(key: K): Option[V] = findNode(key) match {
    case null => None
    case nd => Some(nd match {
      case nd: LLNode @uc => nd.value
      case nd: RBNode @uc => nd.value
    })
  }

  /** Returns the value associated with `key`.
   *
   *  @param key the key to look up
   *  @return the value bound to `key`
   *  @throws NoSuchElementException if `key` is not in this map
   */
  @throws[NoSuchElementException]
  override def apply(key: K): V = findNode(key) match {
    case null => default(key)
    case nd => nd match {
      case nd: LLNode @uc => nd.value
      case nd: RBNode @uc => nd.value
    }
  }

  /** Returns the value associated with `key`, or `default` if `key` is not in this map.
   *  The lookup avoids allocating an `Option`.
   *
   *  @tparam V1 the result type, a supertype of this map's value type
   *  @param key the key to look up
   *  @param default the value to return when `key` is absent; evaluated only in that case
   *  @return the value bound to `key`, or `default` if there is no such binding
   */
  override def getOrElse[V1 >: V](key: K, default: => V1): V1 = {
    val nd = findNode(key)
    if (nd eq null) default else nd match {
      case nd: LLNode @uc => nd.value
      case n => n.asInstanceOf[RBNode].value
    }
  }

  @`inline` private def findNode(elem: K): Node | Null = {
    val hash = computeHash(elem)
    table(index(hash)) match {
      case null => null
      case n: LLNode @uc => n.getNode(elem, hash)
      case n => n.asInstanceOf[RBNode].getNode(elem, hash)
    }
  }

  /** Grows the internal table, if necessary, so that `size` entries can be stored without
   *  triggering a resize. Never shrinks the table.
   *
   *  @param size the expected number of entries
   *  @note NEEDS-HUMAN: `if(size == 0) reallocTable(target)` tests the `size` parameter, which
   *        shadows the `size` method; the analogous check in `growTable` of `HashMap`/`HashSet`
   *        tests the current content size. `reallocTable` discards the table contents without
   *        resetting `contentSize`, so `sizeHint(0)` on a non-empty map whose load factor is
   *        small enough for `target` to exceed the table length would silently drop all entries.
   */
  override def sizeHint(size: Int): Unit = {
    val target = tableSizeFor(((size + 1).toDouble / loadFactor).toInt)
    if(target > table.length) {
      if(size == 0) reallocTable(target)
      else growTable(target)
    }
  }

  /** Adds a new key/value pair to this map. If the map already contains a binding for `key`,
   *  its value is replaced by `value`.
   *
   *  @param key the key to add
   *  @param value the value to bind to `key`
   */
  override def update(key: K, value: V): Unit = put0(key, value, getOld = false)

  /** Adds a new key/value pair to this map. If the map already contains a binding for `key`,
   *  its value is replaced by `value`.
   *
   *  @param key the key to add
   *  @param value the value to bind to `key`
   *  @note NEEDS-HUMAN: the returned option is unreliable. `put0` ends in
   *        `if(res) Some(null.asInstanceOf[V]) else null //TODO`, so this method returns
   *        `Some(null)` instead of `None` when `key` was not present, and `None` instead of
   *        `Some(previousValue)` when the binding was replaced inside a tree bucket (`insert`
   *        discards the old value). Only replacement inside a list bucket returns the previous
   *        value as `MapOps.put` documents.
   */
  override def put(key: K, value: V): Option[V] = put0(key, value, getOld = true) match {
    case null => None
    case sm => sm
  }

  /** Adds a single key-value pair to this map, replacing the value of an existing binding for
   *  the same key.
   *
   *  @param elem the key-value pair to add
   *  @return this map
   */
  def addOne(elem: (K, V)): this.type = { put0(elem._1, elem._2, getOld = false); this }

  @`inline` private def put0(key: K, value: V, getOld: Boolean): Some[V] | Null = {
    if(contentSize + 1 >= threshold) growTable(table.length * 2)
    val hash = computeHash(key)
    val idx = index(hash)
    put0(key, value, getOld, hash, idx)
  }

  private def put0(key: K, value: V, getOld: Boolean, hash: Int, idx: Int): Some[V] | Null = {
    val res = table(idx) match {
      case n: RBNode @uc =>
        insert(n, idx, key, hash, value)
      case _old =>
        val old: LLNode | Null = _old.asInstanceOf[LLNode | Null]
        if(old eq null) {
          table(idx) = new LLNode(key, hash, value, null)
        } else {
          var remaining = CollisionProofHashMap.treeifyThreshold
          var prev: LLNode | Null = null
          var n: LLNode | Null = old
          while((n ne null) && n.hash <= hash && remaining > 0) {
            if(n.hash == hash && key == n.key) {
              val old = n.value
              n.value = value
              return (if(getOld) Some(old) else null)
            }
            prev = n
            n = n.next
            remaining -= 1
          }
          if(remaining == 0) {
            treeify(old, idx)
            return put0(key, value, getOld, hash, idx)
          }
          if(prev eq null) table(idx) = new LLNode(key, hash, value, old)
          else prev.next = new LLNode(key, hash, value, prev.next)
        }
        true
    }
    if(res) contentSize += 1
    if(res) Some(null.asInstanceOf[V]) else null //TODO
  }

  private def treeify(old: LLNode, idx: Int): Unit = {
    table(idx) = CollisionProofHashMap.leaf(old.key, old.hash, old.value, red = false, null)
    var n: LLNode | Null = old.next
    while(n ne null) {
      val root = table(idx).asInstanceOf[RBNode]
      insertIntoExisting(root, idx, n.key, n.hash, n.value, root)
      n = n.next
    }
  }

  /** Adds all key-value pairs produced by `xs` to this map, replacing the values of keys that
   *  are already present. When the size of `xs` is known, the table is grown beforehand to
   *  make room for the current entries plus that many more.
   *
   *  @param xs the key-value pairs to add
   *  @return this map
   */
  override def addAll(xs: IterableOnce[(K, V)]^): this.type = {
    sizeHint(xs, delta = contentSize)
    super.addAll(xs)
  }

  // returns the old value or Statics.pfMarker if not found
  private def remove0(elem: K) : Any = {
    val hash = computeHash(elem)
    val idx = index(hash)
    table(idx) match {
      case null => Statics.pfMarker
      case t: RBNode @uc =>
        val v = delete(t, idx, elem, hash)
        if(v.asInstanceOf[AnyRef] ne Statics.pfMarker) contentSize -= 1
        v
      case nd: LLNode @uc if nd.hash == hash && nd.key == elem =>
        // first element matches
        table(idx) = nd.next
        contentSize -= 1
        nd.value
      case nd: LLNode @uc =>
        // find an element that matches
        var prev = nd
        var next = nd.next
        while((next ne null) && next.hash <= hash) {
          if(next.hash == hash && next.key == elem) {
            prev.next = next.next
            contentSize -= 1
            return next.value
          }
          prev = next
          next = next.next
        }
        Statics.pfMarker
    }
  }

  private abstract class MapIterator[R] extends AbstractIterator[R] {
    /** Extracts the value this iterator yields for the given linked-list node.
     *
     *  @param node the list node currently visited
     *  @return the value derived from `node` (for instance its key)
     */
    protected def extract(node: LLNode): R
    /** Extracts the value this iterator yields for the given red-black tree node.
     *
     *  @param node the tree node currently visited
     *  @return the value derived from `node` (for instance its key)
     */
    protected def extract(node: RBNode): R

    private var i = 0
    private var node: Node | Null = null
    private val len = table.length

    /** Returns `true` if there are more nodes to visit, advancing to the next non-empty
     *  bucket if the current bucket is exhausted. On entering a tree bucket, positions the
     *  iterator at the node with the minimal key.
     */
    def hasNext: Boolean = {
      if(node ne null) true
      else {
        while(i < len) {
          val n = table(i)
          i += 1
          n match {
            case null =>
            case n: RBNode @uc =>
              node = CollisionProofHashMap.minNodeNonNull(n)
              return true
            case n: LLNode @uc =>
              node = n
              return true
          }
        }
        false
      }
    }

    /** Returns the value extracted from the next node, advancing to the next node in the
     *  chain within a list bucket and to the in-order successor within a tree bucket.
     *
     *  @throws NoSuchElementException if no more nodes remain
     */
    def next(): R =
      if(!hasNext) Iterator.empty.next()
      else node.nn match {
        case n: RBNode @uc =>
          val r = extract(n)
          node = CollisionProofHashMap.successor(n )
          r
        case n: LLNode @uc =>
          val r = extract(n)
          node = n.next
          r
      }
  }

  /** Returns an iterator over the keys of this map. The iteration order is not specified
   *  and may change when the map is modified.
   */
  override def keysIterator: Iterator[K] = {
    if (isEmpty) Iterator.empty
    else new MapIterator[K] {
      protected def extract(node: LLNode) = node.key
      protected def extract(node: RBNode) = node.key
    }
  }

  /** Returns an iterator over the key-value pairs of this map. The iteration order is not
   *  specified and may change when the map is modified.
   */
  override def iterator: Iterator[(K, V)] = {
    if (isEmpty) Iterator.empty
    else new MapIterator[(K, V)] {
      protected def extract(node: LLNode) = (node.key, node.value)
      protected def extract(node: RBNode) = (node.key, node.value)
    }
  }

  private def growTable(newlen: Int) = {
    var oldlen = table.length
    table = java.util.Arrays.copyOf(table, newlen)
    threshold = newThreshold(table.length)
    while(oldlen < newlen) {
      var i = 0
      while (i < oldlen) {
        val old = table(i)
        if(old ne null) splitBucket(old, i, i + oldlen, oldlen)
        i += 1
      }
      oldlen *= 2
    }
  }

  @`inline` private def reallocTable(newlen: Int) = {
    table = new Array[Node | Null](newlen)
    threshold = newThreshold(table.length)
  }

  @`inline` private def splitBucket(tree: Node | Null, lowBucket: Int, highBucket: Int, mask: Int): Unit = tree match {
    case t: LLNode @uc => splitBucket(t, lowBucket, highBucket, mask)
    case t: RBNode @uc => splitBucket(t, lowBucket, highBucket, mask)
    case null =>
  }

  private def splitBucket(list: LLNode, lowBucket: Int, highBucket: Int, mask: Int): Unit = {
    val preLow: LLNode = new LLNode(null.asInstanceOf[K], 0, null.asInstanceOf[V], null)
    val preHigh: LLNode = new LLNode(null.asInstanceOf[K], 0, null.asInstanceOf[V], null)
    //preLow.next = null
    //preHigh.next = null
    var lastLow: LLNode = preLow
    var lastHigh: LLNode = preHigh
    var n: LLNode | Null = list
    while(n ne null) {
      val next = n.next
      if((n.hash & mask) == 0) { // keep low
        lastLow.next = n
        lastLow = n
      } else { // move to high
        lastHigh.next = n
        lastHigh = n
      }
      n = next
    }
    lastLow.next = null
    if(list ne preLow.next) table(lowBucket) = preLow.next
    if(preHigh.next ne null) {
      table(highBucket) = preHigh.next
      lastHigh.next = null
    }
  }

  private def splitBucket(tree: RBNode, lowBucket: Int, highBucket: Int, mask: Int): Unit = {
    var lowCount, highCount = 0
    tree.foreachNode((n: RBNode) => if((n.hash & mask) != 0) highCount += 1 else lowCount += 1)
    if(highCount != 0) {
      if(lowCount == 0) {
        table(lowBucket) = null
        table(highBucket) = tree
      } else {
        table(lowBucket) = fromNodes(new CollisionProofHashMap.RBNodesIterator(tree).filter(n => (n.hash & mask) == 0), lowCount)
        table(highBucket) = fromNodes(new CollisionProofHashMap.RBNodesIterator(tree).filter(n => (n.hash & mask) != 0), highCount)
      }
    }
  }

  private def tableSizeFor(capacity: Int) =
    (Integer.highestOneBit((capacity-1).max(4))*2).min(1 << 30)

  private def newThreshold(size: Int) = (size.toDouble * loadFactor).toInt

  /** Removes all key-value pairs from this map. The internal table keeps its current capacity. */
  override def clear(): Unit = {
    java.util.Arrays.fill(table.asInstanceOf[Array[AnyRef]], null)
    contentSize = 0
  }

  /** Removes the binding for `key` from this map, if present.
   *
   *  @param key the key whose binding is removed
   *  @return `Some(value)` if `key` was bound to `value` before the removal, `None` if `key`
   *          was not in the map
   */
  override def remove(key: K): Option[V] = {
    val v = remove0(key)
    if(v.asInstanceOf[AnyRef] eq Statics.pfMarker) None else Some(v.asInstanceOf[V])
  }

  /** Removes the binding for `elem` from this map, if present.
   *
   *  @param elem the key whose binding is removed
   *  @return this map
   */
  def subtractOne(elem: K): this.type = { remove0(elem); this }

  /** Returns the number of key-value pairs in this map; never `-1`, since the size of a hash map is always known. */
  override def knownSize: Int = size

  /** Returns `true` if this map contains no key-value pairs. */
  override def isEmpty: Boolean = size == 0

  /** Applies a function `f` to each key-value pair of this map, passed as a tuple. The order
   *  of traversal is not specified and may change when the map is modified.
   *
   *  @tparam U the result type of `f`, which is discarded
   *  @param f the function to apply to each key-value pair
   */
  override def foreach[U](f: ((K, V)) => U): Unit = {
    val len = table.length
    var i = 0
    while(i < len) {
      val n = table(i)
      if(n ne null) n match {
        case n: LLNode @uc => n.foreach(f)
        case n: RBNode @uc => n.foreach(f)
      }
      i += 1
    }
  }

  /** Applies a function `f` to each key-value pair of this map, passing the key and value as
   *  two separate arguments, without allocating tuples. The order of traversal is not
   *  specified and may change when the map is modified.
   *
   *  @tparam U the result type of `f`, which is discarded
   *  @param f the function to apply to each key and value
   */
  override def foreachEntry[U](f: (K, V) => U): Unit = {
    val len = table.length
    var i = 0
    while(i < len) {
      val n = table(i)
      if(n ne null) n match {
        case n: LLNode @uc => n.foreachEntry(f)
        case n: RBNode @uc => n.foreachEntry(f)
      }
      i += 1
    }
  }

  /** Replaces this map with a serialization proxy during Java serialization. The proxy records
   *  the current table length, load factor and key ordering so deserialization can restore an
   *  equivalent map.
   */
  protected def writeReplace(): AnyRef = new DefaultSerializationProxy(new CollisionProofHashMap.DeserializationFactory[K, V](table.length, loadFactor, ordering), this)

  /** The name of this collection class, `"CollisionProofHashMap"`, used as the prefix in `toString`. */
  override protected def className = "CollisionProofHashMap"

  /** Returns the value associated with `key`; if `key` is not in this map, evaluates
   *  `defaultValue`, adds a binding from `key` to the result, and returns it.
   *
   *  `defaultValue` is evaluated at most once, and only when `key` is absent. The key is
   *  hashed and located only once for both the lookup and the insertion; if evaluating
   *  `defaultValue` resizes this map's table, the new binding is still inserted at the
   *  correct position.
   *
   *  @param key the key to look up
   *  @param defaultValue the value to bind to `key` if it is absent; evaluated only in that case
   *  @return the value now associated with `key`
   */
  override def getOrElseUpdate(key: K, defaultValue: => V): V = {
    val hash = computeHash(key)
    val idx = index(hash)
    table(idx) match {
      case null => ()
      case n: LLNode @uc =>
        val nd = n.getNode(key, hash)
        if(nd != null) return nd.value
      case n =>
        val nd = n.asInstanceOf[RBNode].getNode(key, hash)
        if(nd != null) return nd.value
    }
    val table0 = table
    val default = defaultValue
    if(contentSize + 1 >= threshold) growTable(table.length * 2)
    // Avoid recomputing index if the `defaultValue()` or new element hasn't triggered a table resize.
    val newIdx = if (table0 eq table) idx else index(hash)
    put0(key, default, getOld = false, hash, newIdx)
    default
  }

  ///////////////////// Overrides code from SortedMapOps

  /** Builds a new `CollisionProofHashMap` by applying a function to all elements of this $coll.
   *
   *  @tparam K2 the key type of the returned collection
   *  @tparam V2 the value type of the returned collection
   *  @param f      the function to apply to each element.
   *  @return       a new $coll resulting from applying the given function
   *                `f` to each element of this $coll and collecting the results.
   */
  def map[K2, V2](f: ((K, V)) => (K2, V2))
      (implicit @implicitNotFound(CollisionProofHashMap.ordMsg) ordering: Ordering[K2]): CollisionProofHashMap[K2, V2] =
    sortedMapFactory.from(new View.Map[(K, V), (K2, V2)](this, f))

  /** Builds a new `CollisionProofHashMap` by applying a function to all elements of this $coll
   *  and using the elements of the resulting collections.
   *
   *  @tparam K2 the key type of the returned collection
   *  @tparam V2 the value type of the returned collection
   *  @param f      the function to apply to each element.
   *  @return       a new $coll resulting from applying the given collection-valued function
   *                `f` to each element of this $coll and concatenating the results.
   */
  def flatMap[K2, V2](f: ((K, V)) => IterableOnce[(K2, V2)]^)
      (implicit @implicitNotFound(CollisionProofHashMap.ordMsg) ordering: Ordering[K2]): CollisionProofHashMap[K2, V2] =
    sortedMapFactory.from(new View.FlatMap(this, f))

  /** Builds a new sorted map by applying a partial function to all elements of this $coll
   *  on which the function is defined.
   *
   *  @tparam K2 the key type of the returned collection
   *  @tparam V2 the value type of the returned collection
   *  @param pf     the partial function which filters and maps the $coll.
   *  @return       a new $coll resulting from applying the given partial function
   *                `pf` to each element on which it is defined and collecting the results.
   *                The order of the elements is preserved.
   */
  def collect[K2, V2](pf: PartialFunction[(K, V), (K2, V2)])
      (implicit @implicitNotFound(CollisionProofHashMap.ordMsg) ordering: Ordering[K2]): CollisionProofHashMap[K2, V2] =
    sortedMapFactory.from(new View.Collect(this, pf))

  /** Returns a new `CollisionProofHashMap`, with the same key ordering as this map, containing
   *  the key-value pairs of this map followed by those of `suffix`. When a key occurs in both,
   *  the value from `suffix` ends up in the result.
   *
   *  @tparam V2 the value type of the returned map, a supertype of this map's value type
   *  @param suffix the key-value pairs to add to those of this map
   *  @return a new `CollisionProofHashMap` combining this map and `suffix`
   */
  override def concat[V2 >: V](suffix: IterableOnce[(K, V2)]^): CollisionProofHashMap[K, V2] = sortedMapFactory.from(suffix match {
    case it: Iterable[(K, V2) @unchecked] => new View.Concat(this, it)
    case _ => iterator.concat(suffix.iterator)
  })

  /** Alias for `concat`.
   *
   *  @tparam V2 the value type of the returned collection
   *  @param xs the key-value pairs to append to this collection
   */
  @`inline` override final def ++ [V2 >: V](xs: IterableOnce[(K, V2)]^): CollisionProofHashMap[K, V2] = concat(xs)

  /** Returns a new `CollisionProofHashMap` containing the key-value pairs of this map and the
   *  pair `kv`. When `kv._1` is already a key of this map, its value in the result is `kv._2`.
   *
   *  @tparam V1 the value type of the returned map, a supertype of this map's value type
   *  @param kv the key-value pair added to those of this map
   *  @return a new `CollisionProofHashMap` combining this map and `kv`
   */
  @deprecated("Consider requiring an immutable Map or fall back to Map.concat", "2.13.0")
  override def + [V1 >: V](kv: (K, V1)): CollisionProofHashMap[K, V1] =
     sortedMapFactory.from(new View.Appended(this, kv))

  /** Returns a new `CollisionProofHashMap` containing the key-value pairs of this map and the
   *  given pairs. When a key occurs more than once, the value of its last occurrence ends up
   *  in the result.
   *
   *  @tparam V1 the value type of the returned map, a supertype of this map's value type
   *  @param elem1 the first key-value pair added to those of this map
   *  @param elem2 the second key-value pair added to those of this map
   *  @param elems the remaining key-value pairs added to those of this map
   *  @return a new `CollisionProofHashMap` combining this map and the given pairs
   */
  @deprecated("Use ++ with an explicit collection argument instead of + with varargs", "2.13.0")
  override def + [V1 >: V](elem1: (K, V1), elem2: (K, V1), elems: (K, V1)*): CollisionProofHashMap[K, V1] =
     sortedMapFactory.from(new View.Concat(new View.Appended(new View.Appended(this, elem1), elem2), elems))

  ///////////////////// RedBlackTree code derived from mutable.RedBlackTree:

  @`inline` private def isRed(node: RBNode | Null) = (node ne null) && node.red
  @`inline` private def isBlack(node: RBNode | Null) = (node eq null) || !node.red

  @unused @`inline` private def compare(key: K, hash: Int, node: LLNode): Int = {
    val i = hash - node.hash
    if(i != 0) i else ordering.compare(key, node.key)
  }

  @`inline` private def compare(key: K, hash: Int, node: RBNode): Int = {
    /*val i = hash - node.hash
    if(i != 0) i else*/ ordering.compare(key, node.key)
  }

  // ---- insertion ----

  @tailrec private final def insertIntoExisting(_root: RBNode, bucket: Int, key: K, hash: Int, value: V, x: RBNode): Boolean = {
    val cmp = compare(key, hash, x)
    if(cmp == 0) {
      x.value = value
      false
    } else {
      val next = if(cmp < 0) x.left else x.right
      if(next eq null) {
        val z = CollisionProofHashMap.leaf(key, hash, value, red = true, x)
        if (cmp < 0) x.left = z else x.right = z
        table(bucket) = fixAfterInsert(_root, z)
        return true
      }
      else insertIntoExisting(_root, bucket, key, hash, value, next)
    }
  }

  private final def insert(tree: RBNode | Null, bucket: Int, key: K, hash: Int, value: V): Boolean = {
    if(tree eq null) {
      table(bucket) = CollisionProofHashMap.leaf(key, hash, value, red = false, null)
      true
    } else insertIntoExisting(tree, bucket, key, hash, value, tree)
  }

  private def fixAfterInsert(_root: RBNode, node: RBNode): RBNode = {
    var root = _root
    var z = node
    while (isRed(z.parent)) {
      if (z.parent eq z.parent.nn.parent.nn.left) {
        val y = z.parent.nn.parent.nn.right
        if (isRed(y)) {
          z.parent.nn.red = false
          y.nn.red = false
          z.parent.nn.parent.nn.red = true
          z = z.parent.nn.parent.nn
        } else {
          if (z eq z.parent.nn.right) {
            z = z.parent.nn
            root = rotateLeft(root, z)
          }
          z.parent.nn.red = false
          z.parent.nn.parent.nn.red = true
          root = rotateRight(root, z.parent.nn.parent.nn)
        }
      } else { // symmetric cases
        val y = z.parent.nn.parent.nn.left
        if (isRed(y)) {
          z.parent.nn.red = false
          y.nn.red = false
          z.parent.nn.parent.nn.red = true
          z = z.parent.nn.parent.nn
        } else {
          if (z eq z.parent.nn.left) {
            z = z.parent.nn
            root = rotateRight(root, z)
          }
          z.parent.nn.red = false
          z.parent.nn.parent.nn.red = true
          root = rotateLeft(root, z.parent.nn.parent.nn)
        }
      }
    }
    root.red = false
    root
  }

  // ---- deletion ----

  // returns the old value or Statics.pfMarker if not found
  private def delete(_root: RBNode, bucket: Int, key: K, hash: Int): Any = {
    var root = _root
    val z = root.getNode(key, hash: Int)
    if (z ne null) {
      val oldValue = z.value
      var y = z
      var yIsRed = y.red
      var x: RBNode | Null = null
      var xParent: RBNode | Null = null

      if (z.left eq null) {
        x = z.right
        root = transplant(root, z, z.right.nn)
        xParent = z.parent
      }
      else if (z.right eq null) {
        x = z.left
        root = transplant(root, z, z.left)
        xParent = z.parent
      }
      else {
        y = CollisionProofHashMap.minNodeNonNull(z.right)
        yIsRed = y.red
        x = y.right

        if (y.parent eq z) xParent = y
        else {
          xParent = y.parent
          root = transplant(root, y, y.right.nn)
          y.right = z.right
          y.right.nn.parent = y
        }
        root = transplant(root, z, y)
        y.left = z.left
        y.left.nn.parent = y
        y.red = z.red
      }

      if (!yIsRed) root = fixAfterDelete(root, x, xParent)
      if(root ne _root) table(bucket) = root
      oldValue
    } else Statics.pfMarker
  }

  private def fixAfterDelete(_root: RBNode, node: RBNode | Null, parent: RBNode | Null): RBNode = {
    var root = _root
    var x = node
    var xParent = parent
    while ((x ne root) && isBlack(x)) {
      if (x eq xParent.nn.left) {
        var w = xParent.nn.right
        // assert(w ne null)

        if (w.nn.red) {
          w.nn.red = false
          xParent.nn.red = true
          root = rotateLeft(root, xParent.nn)
          w = xParent.nn.right
        }
        if (isBlack(w.nn.left) && isBlack(w.nn.right)) {
          w.nn.red = true
          x = xParent
        } else {
          if (isBlack(w.nn.right)) {
            w.nn.left.nn.red = false
            w.nn.red = true
            root = rotateRight(root, w.nn)
            w = xParent.nn.right
          }
          w.nn.red = xParent.nn.red
          xParent.nn.red = false
          w.nn.right.nn.red = false
          root = rotateLeft(root, xParent.nn)
          x = root
        }
      } else { // symmetric cases
        var w = xParent.nn.left
        // assert(w ne null)

        if (w.nn.red) {
          w.nn.red = false
          xParent.nn.red = true
          root = rotateRight(root, xParent.nn)
          w = xParent.nn.left
        }
        if (isBlack(w.nn.right) && isBlack(w.nn.left)) {
          w.nn.red = true
          x = xParent
        } else {
          if (isBlack(w.nn.left)) {
            w.nn.right.nn.red = false
            w.nn.red = true
            root = rotateLeft(root, w.nn)
            w = xParent.nn.left
          }
          w.nn.red = xParent.nn.red
          xParent.nn.red = false
          w.nn.left.nn.red = false
          root = rotateRight(root, xParent.nn)
          x = root
        }
      }
      xParent = x.nn.parent
    }
    if (x ne null) x.red = false
    root
  }

  // ---- helpers ----

  @`inline` private def rotateLeft(_root: RBNode, x: RBNode): RBNode = {
    var root = _root
    val y = x.right.nn
    x.right = y.left

    val xp = x.parent
    if (y.left ne null) y.left.parent = x
    y.parent = xp

    if (xp eq null) root = y
    else if (x eq xp.left) xp.left = y
    else xp.right = y

    y.left = x
    x.parent = y
    root
  }

  @`inline` private def rotateRight(_root: RBNode, x: RBNode): RBNode = {
    var root = _root
    val y = x.left.nn
    x.left = y.right

    val xp = x.parent
    if (y.right ne null) y.right.parent = x
    y.parent = xp

    if (xp eq null) root = y
    else if (x eq xp.right) xp.right = y
    else xp.left = y

    y.right = x
    x.parent = y
    root
  }

  /** Transplant the node `from` to the place of node `to`. This is done by setting `from` as a child of `to`'s previous
   *  parent and setting `from`'s parent to the `to`'s previous parent. The children of `from` are left unchanged.
   *
   *  @param _root the root of the red-black tree
   *  @param to the node to be replaced in the tree
   *  @param from the node that replaces `to`
   *  @return the (possibly updated) root of the tree, which differs from `_root` only when `to` was the root
   */
  private def transplant(_root: RBNode, to: RBNode, from: RBNode): RBNode = {
    var root = _root
    if (to.parent eq null) root = from
    else if (to eq to.parent.left) to.parent.left = from
    else to.parent.right = from
    if (from ne null) from.parent = to.parent
    root
  }

  // building

  /** Builds a red-black tree from the next `size` nodes produced by `xs`, which must arrive
   *  in ascending key order, copying each node's key, hash and value. The tree is built
   *  perfectly balanced, with only the nodes at the maximum depth colored red. Used to rebuild
   *  the tree buckets when the table grows.
   *
   *  @param xs the nodes whose keys, hashes and values are copied into the new tree
   *  @param size the number of nodes to consume from `xs`
   *  @return the root of the new tree, or `null` if `size` is `0`
   */
  def fromNodes(xs: Iterator[Node], size: Int): RBNode | Null = {
    val maxUsedDepth = 32 - Integer.numberOfLeadingZeros(size) // maximum depth of non-leaf nodes
    def f(level: Int, size: Int): RBNode | Null = size match {
      case 0 => null
      case 1 =>
        val nn = xs.next()
        val (key, hash, value) = nn match {
          case nn: LLNode @uc => (nn.key, nn.hash, nn.value)
          case nn: RBNode @uc => (nn.key, nn.hash, nn.value)
        }
        new RBNode(key, hash, value, level == maxUsedDepth && level != 1, null, null, null)
      case n =>
        val leftSize = (size-1)/2
        val left = f(level+1, leftSize)
        val nn = xs.next()
        val right = f(level+1, size-1-leftSize)
        val (key, hash, value) = nn match {
          case nn: LLNode @uc => (nn.key, nn.hash, nn.value)
          case nn: RBNode @uc => (nn.key, nn.hash, nn.value)
        }
        val n = new RBNode(key, hash, value, red = false, left, right, null)
        if(left ne null) left.parent = n
        if(right ne null) right.parent = n
        n
    }
    f(1, size)
  }
}

/** $factoryInfo
 *  @define Coll `mutable.CollisionProofHashMap`
 *  @define coll mutable collision-proof hash map
 */
@SerialVersionUID(3L)
object CollisionProofHashMap extends SortedMapFactory[CollisionProofHashMap] {
  private[collection] final val ordMsg = "No implicit Ordering[${K2}] found to build a CollisionProofHashMap[${K2}, ${V2}]. You may want to upcast to a Map[${K}, ${V}] first by calling `unsorted`."

  /** Creates a new collision-proof hash map containing the key-value pairs of the given
   *  collection. When the size of `it` is known, the initial capacity is chosen so that all
   *  pairs can be added without resizing the table.
   *
   *  @tparam K the key type of the new map, which must have an implicit `Ordering`
   *  @tparam V the value type of the new map
   *  @param it the collection whose key-value pairs are added to the new map
   *  @return a new `CollisionProofHashMap` containing the key-value pairs of `it`
   */
  def from[K : Ordering, V](it: scala.collection.IterableOnce[(K, V)]^): CollisionProofHashMap[K, V] = {
    val k = it.knownSize
    val cap = if(k > 0) ((k + 1).toDouble / defaultLoadFactor).toInt else defaultInitialCapacity
    new CollisionProofHashMap[K, V](cap, defaultLoadFactor) ++= it
  }

  /** Creates a new, empty collision-proof hash map with the default initial capacity (16) and
   *  load factor (0.75).
   *
   *  @tparam K the key type of the new map, which must have an implicit `Ordering`
   *  @tparam V the value type of the new map
   *  @return a new, empty `CollisionProofHashMap`
   */
  def empty[K : Ordering, V]: CollisionProofHashMap[K, V] = new CollisionProofHashMap[K, V]

  /** Creates a new builder for a `CollisionProofHashMap` with the default initial capacity and
   *  load factor.
   *
   *  @tparam K the key type of the map to build, which must have an implicit `Ordering`
   *  @tparam V the value type of the map to build
   *  @return a new builder producing a `CollisionProofHashMap`
   */
  def newBuilder[K : Ordering, V]: Builder[(K, V), CollisionProofHashMap[K, V]] = newBuilder(defaultInitialCapacity, defaultLoadFactor)

  /** Creates a new builder for a `CollisionProofHashMap` with the given initial capacity and
   *  load factor. Size hints given to the builder are forwarded to the underlying map's
   *  `sizeHint`.
   *
   *  @tparam K the key type of the map to build, which must have an implicit `Ordering`
   *  @tparam V the value type of the map to build
   *  @param initialCapacity the initial capacity of the map's hash table
   *  @param loadFactor the load factor of the map's hash table
   *  @return a new builder producing a `CollisionProofHashMap`
   */
  def newBuilder[K : Ordering, V](initialCapacity: Int, loadFactor: Double): Builder[(K, V), CollisionProofHashMap[K, V]] =
    new GrowableBuilder[(K, V), CollisionProofHashMap[K, V]](new CollisionProofHashMap[K, V](initialCapacity, loadFactor)) {
      override def sizeHint(size: Int) = elems.sizeHint(size)
    }

  /** The default load factor for the hash table. */
  final def defaultLoadFactor: Double = 0.75

  /** The default initial capacity for the hash table. */
  final def defaultInitialCapacity: Int = 16

  @SerialVersionUID(3L)
  private final class DeserializationFactory[K, V](val tableLength: Int, val loadFactor: Double, val ordering: Ordering[K]) extends Factory[(K, V), CollisionProofHashMap[K, V]], Serializable {
    /** Creates a new `CollisionProofHashMap` with the recorded table length, load factor and
     *  key ordering, containing the key-value pairs of `it`.
     *
     *  @param it the deserialized key-value pairs to add
     *  @return a new `CollisionProofHashMap` containing the key-value pairs of `it`
     */
    def fromSpecific(it: IterableOnce[(K, V)]^): CollisionProofHashMap[K, V] = new CollisionProofHashMap[K, V](tableLength, loadFactor)(using ordering) ++= it
    /** Returns a new builder for a `CollisionProofHashMap` with the recorded table length, load factor and key ordering. */
    def newBuilder: Builder[(K, V), CollisionProofHashMap[K, V]] = CollisionProofHashMap.newBuilder(tableLength, loadFactor)(using ordering)
  }

  /** Compares `key` and `hash` against a list node, by the difference of the improved hashes
   *  first and by `ord` on ties. Never called (list nodes are searched via `LLNode.getNode`).
   */
  @unused @`inline` private def compare[K, V](key: K, hash: Int, node: LLNode[K, V])(implicit ord: Ordering[K]): Int = {
    val i = hash - node.hash
    if(i != 0) i else ord.compare(key, node.key)
  }

  @`inline` private def compare[K, V](key: K, hash: Int, node: RBNode[K, V])(implicit ord: Ordering[K]): Int = {
    /*val i = hash - node.hash
    if(i != 0) i else*/ ord.compare(key, node.key)
  }

  private final val treeifyThreshold = 8

  // Superclass for RBNode and LLNode to help the JIT with optimizing instance checks, but no shared common fields.
  // Keeping calls monomorphic where possible and dispatching manually where needed is faster.
  /** The common supertype of the two bucket node types, `RBNode` and `LLNode`. It has no
   *  members of its own; a bucket stores either a list of `LLNode`s or a tree of `RBNode`s,
   *  and callers dispatch on the concrete node type.
   */
  sealed abstract class Node

  /////////////////////////// Red-Black Tree Node

  /** A node of a red-black tree bucket. The tree is ordered by the key `Ordering` alone; the
   *  improved hash of the key is cached only so it can be redistributed when the table grows.
   *
   *  @tparam K the key type
   *  @tparam V the value type
   *  @param key the key stored in this node
   *  @param hash the improved hash of `key`, cached when the entry was created
   *  @param value the value bound to `key`
   *  @param red `true` if this node is red, `false` if it is black
   *  @param left the left child, or `null` if there is none
   *  @param right the right child, or `null` if there is none
   *  @param parent the parent node, or `null` if this node is the root of its tree
   */
  final class RBNode[K, V](
      /** The key, its cached improved hash, the value bound to the key, and this node's color (`true` for red, `false` for black). */
      var key: K, var hash: Int, var value: V, var red: Boolean,
      /** The left child, or `null` if there is none. */
      @annotation.stableNull
      var left: RBNode[K, V] | Null,
      /** The right child, or `null` if there is none. */
      @annotation.stableNull
      var right: RBNode[K, V] | Null,
      /** The parent node, or `null` if this node is the root of its tree. */
      @annotation.stableNull
      var parent: RBNode[K, V] | Null
    ) extends Node {
    /** Returns a string rendering this node's key, hash, value, color and both subtrees. */
    override def toString(): String = "RBNode(" + key + ", " + hash + ", " + value + ", " + red + ", " + left + ", " + right + ")"

    /** Finds the node for key `k` in the tree rooted at this node, descending left or right
     *  according to the ordering.
     *
     *  @param k the key to look for
     *  @param h the improved hash of `k`; never used, nodes are compared by the ordering alone
     *  @param ord the ordering used to compare keys
     *  @return the node whose key compares equal to `k`, or `null` if the tree contains no such node
     */
    @tailrec def getNode(k: K, h: Int)(implicit ord: Ordering[K]): RBNode[K, V] | Null = {
      val cmp = compare(k, h, this)
      if (cmp < 0) {
        if(left ne null) left.getNode(k, h) else null
      } else if (cmp > 0) {
        if(right ne null) right.getNode(k, h) else null
      } else this
    }

    /** Applies `f` to the key-value pair of every node in the tree rooted at this node,
     *  passed as a tuple, in ascending key order (in-order traversal).
     *
     *  @tparam U the result type of `f`, which is discarded
     *  @param f the function to apply to each key-value pair
     */
    def foreach[U](f: ((K, V)) => U): Unit = {
      if(left ne null) left.foreach(f)
      f((key, value))
      if(right ne null) right.foreach(f)
    }

    /** Applies `f` to the key and value of every node in the tree rooted at this node,
     *  passed as two separate arguments, in ascending key order (in-order traversal).
     *
     *  @tparam U the result type of `f`, which is discarded
     *  @param f the function to apply to each key and value
     */
    def foreachEntry[U](f: (K, V) => U): Unit = {
      if(left ne null) left.foreachEntry(f)
      f(key, value)
      if(right ne null) right.foreachEntry(f)
    }

    /** Applies `f` to every node in the tree rooted at this node, in ascending key order
     *  (in-order traversal).
     *
     *  @tparam U the result type of `f`, which is discarded
     *  @param f the function to apply to each node
     */
    def foreachNode[U](f: RBNode[K, V] => U): Unit = {
      if(left ne null) left.foreachNode(f)
      f(this)
      if(right ne null) right.foreachNode(f)
    }
  }

  @`inline` private def leaf[A, B](key: A, hash: Int, value: B, red: Boolean, parent: RBNode[A, B] | Null): RBNode[A, B] =
    new RBNode(key, hash, value, red, null, null, parent)

  @tailrec private def minNodeNonNull[A, B](node: RBNode[A, B]): RBNode[A, B] =
    if (node.left eq null) node else minNodeNonNull(node.left)

  /** Returns the node that follows `node` in an in-order tree traversal. If `node` has the maximum key (and is,
   *  therefore, the last node), this method returns `null`.
   *
   *  @tparam A the key type of the tree nodes
   *  @tparam B the value type of the tree nodes
   *  @param node the node whose successor is to be found
   *  @return the in-order successor of `node`, or `null` if `node` is the last node in the traversal
   */
  private def successor[A, B](node: RBNode[A, B]): RBNode[A, B] | Null = {
    if (node.right ne null) minNodeNonNull(node.right)
    else {
      var x = node
      var y = x.parent
      while ((y ne null) && (x eq y.right)) {
        x = y
        y = y.parent
      }
      y
    }
  }

  private final class RBNodesIterator[A, B](tree: RBNode[A, B] | Null)(implicit @unused ord: Ordering[A]) extends AbstractIterator[RBNode[A, B]] {
    private var nextNode: RBNode[A, B] | Null = if(tree eq null) null else minNodeNonNull(tree)

    /** Returns `true` if more tree nodes remain. */
    def hasNext: Boolean = nextNode ne null

    /** Returns the next node of the tree in ascending key order (in-order traversal).
     *
     *  @throws NoSuchElementException if no more nodes remain
     */
    @throws[NoSuchElementException]
    def next(): RBNode[A, B] = nextNode match {
      case null => Iterator.empty.next()
      case node =>
        nextNode = successor(node)
        node
    }
  }

  /////////////////////////// Linked List Node

  private final class LLNode[K, V](var key: K, var hash: Int, var value: V, @annotation.stableNull var next: LLNode[K, V] | Null) extends Node {
    /** Returns a string rendering this node's key, value and hash followed by the rest of the chain. */
    override def toString() = s"LLNode($key, $value, $hash) -> $next"

    private def eq(a: Any, b: Any): Boolean =
      if(a.asInstanceOf[AnyRef] eq null) b.asInstanceOf[AnyRef] eq null else a.asInstanceOf[AnyRef].equals(b)

    /** Finds the node for key `k` in the chain starting at this node. Keys are compared with
     *  null-safe `equals`, and because chains are sorted in ascending hash order, the search
     *  stops as soon as a node with a larger hash is seen.
     *
     *  @param k the key to look for
     *  @param h the improved hash of `k`
     *  @param ord the ordering for keys; never used
     *  @return the node whose key equals `k`, or `null` if the chain contains no such node
     */
    @tailrec def getNode(k: K, h: Int)(implicit ord: Ordering[K]): LLNode[K, V] | Null = {
      if(h == hash && eq(k, key) /*ord.compare(k, key) == 0*/) this
      else if((next eq null) || (hash > h)) null
      else next.getNode(k, h)
    }

    /** Applies `f` to the key-value pair of this node and of every following node in the
     *  chain, passed as a tuple.
     *
     *  @tparam U the result type of `f`, which is discarded
     *  @param f the function to apply to each key-value pair
     */
    @tailrec def foreach[U](f: ((K, V)) => U): Unit = {
      f((key, value))
      if(next ne null) next.foreach(f)
    }

    /** Applies `f` to the key and value of this node and of every following node in the
     *  chain, passed as two separate arguments.
     *
     *  @tparam U the result type of `f`, which is discarded
     *  @param f the function to apply to each key and value
     */
    @tailrec def foreachEntry[U](f: (K, V) => U): Unit = {
      f(key, value)
      if(next ne null) next.foreachEntry(f)
    }

    /** Applies `f` to this node and to every following node in the chain.
     *
     *  @tparam U the result type of `f`, which is discarded
     *  @param f the function to apply to each node
     */
    @tailrec def foreachNode[U](f: LLNode[K, V] => U): Unit = {
      f(this)
      if(next ne null) next.foreachNode(f)
    }
  }
}
