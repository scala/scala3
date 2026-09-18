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

import scala.annotation.{nowarn, tailrec}
import scala.collection.Stepper.EfficientSplit
import scala.collection.generic.DefaultSerializationProxy
import scala.util.hashing.MurmurHash3

/** This class implements mutable maps using a hashtable.
 *
 *  @see ["Scala's Collection Library overview"](https://docs.scala-lang.org/overviews/collections-2.13/concrete-mutable-collection-classes.html#hash-tables)
 *  section on `Hash Tables` for more information.
 *
 *  @tparam K    the type of the keys contained in this hash map.
 *  @tparam V    the type of the values assigned to keys in this hash map.
 *
 *  @define Coll `mutable.HashMap`
 *  @define coll mutable hash map
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
@deprecatedInheritance("HashMap will be made final; use .withDefault for the common use case of computing a default value", "2.13.0")
class HashMap[K, V](initialCapacity: Int, loadFactor: Double)
  extends AbstractMap[K, V]
    with MapOps[K, V, HashMap, HashMap[K, V]]
    with StrictOptimizedIterableOps[(K, V), Iterable, HashMap[K, V]]
    with StrictOptimizedMapOps[K, V, HashMap, HashMap[K, V]]
    with MapFactoryDefaults[K, V, HashMap, Iterable]
    with Serializable {

  /* The `HashMap` class holds the following invariant:
   * - For each i between  0 and table.length, the bucket at table(i) only contains keys whose hash-index is i.
   * - Every bucket is sorted in ascendent hash order
   * - The sum of the lengths of all buckets is equal to contentSize.
   */
  /** Creates a new, empty hash map with the default initial capacity (16) and the default load factor (0.75). */
  def this() = this(HashMap.defaultInitialCapacity, HashMap.defaultLoadFactor)

  import HashMap.Node

  /** The actual hash table. */
  private var table = new Array[Node[K, V] | Null](tableSizeFor(initialCapacity))

  /** The next size value at which to resize (capacity * load factor). */
  private var threshold: Int = newThreshold(table.length)

  private var contentSize = 0

  /** Returns the number of key-value pairs in this map. */
  override def size: Int = contentSize

  /** Performs the inverse operation of improveHash. In this case, it happens to be identical to improveHash.
   *
   *  @param improvedHash the improved hash value to convert back to the original `any.##` hash
   *  @return the original `any.##` hash code corresponding to `improvedHash`
   */
  @`inline` private[collection] def unimproveHash(improvedHash: Int): Int = improveHash(improvedHash)

  /** Computes the improved hash of an original (`any.##`) hash.
   *
   *  @param originalHash the original hash code from `any.##`
   *  @return the improved hash with the high 16 bits XORed into the low 16 bits
   */
  @`inline` private def improveHash(originalHash: Int): Int = {
    // Improve the hash by XORing the high 16 bits into the low 16 bits just in case entropy is skewed towards the
    // high-value bits. We only use the lowest bits to determine the hash bucket. This is the same improvement
    // algorithm as in java.util.HashMap.
    //
    // This function is also its own inverse. That is, for all ints i, improveHash(improveHash(i)) = i
    // this allows us to retrieve the original hash when we need it, for instance when appending to an immutable.HashMap
    // and that is why unimproveHash simply forwards to this method
    originalHash ^ (originalHash >>> 16)
  }

  /** Computes the improved hash of this key.
   *
   *  @param o the key for which to compute the improved hash
   *  @return the improved hash code of `o` (i.e., `improveHash(o.##)`)
   */
  @`inline` private def computeHash(o: K): Int = improveHash(o.##)

  @`inline` private def index(hash: Int) = hash & (table.length - 1)

  /** Tests whether this map contains a binding for a key.
   *
   *  @param key the key to test for membership
   *  @return `true` if this map contains a binding for `key`, `false` otherwise
   */
  override def contains(key: K): Boolean = findNode(key) ne null

  @`inline` private def findNode(key: K): Node[K, V] | Null = {
    val hash = computeHash(key)
    table(index(hash)) match {
      case null => null
      case nd => nd.findNode(key, hash)
    }
  }

  /** Grows the internal table, if necessary, so that `size` entries can be stored without
   *  triggering a resize. Never shrinks the table.
   *
   *  @param size the expected number of entries
   */
  override def sizeHint(size: Int): Unit = {
    val target = tableSizeFor(((size + 1).toDouble / loadFactor).toInt)
    if(target > table.length) growTable(target)
  }

  /** Adds all key-value pairs produced by `xs` to this map, replacing the values of keys that
   *  are already present. When `xs` is an `immutable.HashMap`, a `mutable.HashMap`, or a
   *  `LinkedHashMap`, the hash codes cached in `xs` are reused instead of being recomputed.
   *
   *  @param xs the key-value pairs to add
   *  @return this map
   */
  override def addAll(xs: IterableOnce[(K, V)]^): this.type = {
    sizeHint(xs)

    (xs: @unchecked) match {
      case hm: immutable.HashMap[K, V] =>
        hm.foreachWithHash((k, v, h) => put0(k, v, improveHash(h), getOld = false))
        this
      case hm: mutable.HashMap[K, V] =>
        val iter = hm.nodeIterator
        while (iter.hasNext) {
          val next = iter.next()
          put0(next.key, next.value, next.hash, getOld = false)
        }
        this
      case lhm: mutable.LinkedHashMap[K, V] =>
        val iter = lhm.entryIterator
        while (iter.hasNext) {
          val entry = iter.next()
          put0(entry.key, entry.value, entry.hash, getOld = false)
        }
        this
      case thatMap: Map[K, V] =>
        thatMap.foreachEntry { (key: K, value: V) =>
          put0(key, value, improveHash(key.##), getOld = false)
        }
        this
      case _ =>
        super.addAll(xs)
    }
  }

  // Override updateWith for performance, so we can do the update while hashing
  // the input key only once and performing one lookup into the hash table
  /** Updates the binding for `key` based on its current, optional value: applies
   *  `remappingFunction` to `Some(value)` if `key` is bound to `value` and to `None` otherwise,
   *  then updates the map to agree with the result. If the function returns `Some(v)`, `key` is
   *  bound to `v`; if it returns `None`, any binding for `key` is removed. This override hashes
   *  `key` and searches the table only once for both the lookup and the update; the function is
   *  applied exactly once.
   *
   *  @param key the key whose binding is updated
   *  @param remappingFunction the function that maps the current, optional value to the new, optional value
   *  @return the value now bound to `key` wrapped in `Some`, or `None` if `key` is now unbound
   *  @throws Throwable whatever `remappingFunction` throws, in which case this map is
   *          left unchanged
   */
  override def updateWith(key: K)(remappingFunction: Option[V] => Option[V]): Option[V] = {
    if (getClass != classOf[HashMap[?, ?]]) {
      // subclasses of HashMap might customise `get` ...
      super.updateWith(key)(remappingFunction)
    } else {
      val hash = computeHash(key)
      val indexedHash = index(hash)

      var foundNode: Node[K, V] | Null = null
      var previousNode: Node[K, V] | Null = null
      table(indexedHash) match {
        case null =>
        case nd =>
          @tailrec
          def findNode(prev: Node[K, V] | Null, nd: Node[K, V], k: K, h: Int): Unit = {
            if (h == nd.hash && k == nd.key) {
              previousNode = prev
              foundNode = nd
            }
            else {
              val ndNext = nd.next
              if ((ndNext eq null) || (nd.hash > h)) ()
              else findNode(nd, ndNext, k, h)
            }
          }

          findNode(null, nd, key, hash)
      }

      val previousValue = foundNode match {
        case null => None
        case nd => Some(nd.value)
      }

      val nextValue = remappingFunction(previousValue)

      (previousValue, nextValue) match {
        case (None, None) => // do nothing

        case (Some(_), None) =>
          if (previousNode != null) previousNode.nn.next = foundNode.nn.next
          else table(indexedHash) = foundNode.nn.next
          contentSize -= 1

        case (None, Some(value)) =>
          val newIndexedHash =
            if (contentSize + 1 >= threshold) {
              growTable(table.length * 2)
              index(hash)
            } else indexedHash
          put0(key, value, getOld = false, hash, newIndexedHash)

        case (Some(_), Some(newValue)) => foundNode.nn.value = newValue
      }
      nextValue
    }
  }

  /** Removes the bindings for all keys produced by `xs` from this map, stopping early once
   *  this map is empty. When `xs` is an `immutable.HashSet`, a `mutable.HashSet`, or a
   *  `LinkedHashSet`, the hash codes cached in `xs` are reused instead of being recomputed.
   *
   *  @param xs the keys whose bindings are removed
   *  @return this map
   */
  override def subtractAll(xs: IterableOnce[K]^): this.type = {
    if (size == 0) {
      return this
    }

    (xs: @unchecked) match {
      case hs: immutable.HashSet[K] =>
        hs.foreachWithHashWhile { (k, h) =>
          remove0(k, improveHash(h))
          size > 0
        }
        this
      case hs: mutable.HashSet[K] =>
        val iter = hs.nodeIterator
        while (iter.hasNext) {
          val next = iter.next()
          remove0(next.key, next.hash)
          if (size == 0) return this
        }
        this
      case lhs: mutable.LinkedHashSet[K] =>
        val iter = lhs.entryIterator
        while (iter.hasNext) {
          val next = iter.next()
          remove0(next.key, next.hash)
          if (size == 0) return this
        }
        this
      case _ => super.subtractAll(xs)
    }
  }

  /** Adds a key-value pair to this map
   *
   *  @param key the key to add
   *  @param value the value to add
   *  @param hash the **improved** hashcode of `key` (see computeHash)
   *  @param getOld if true, then the previous value for `key` will be returned, otherwise, false
   *  @return `Some` wrapping the previous value if `getOld` is true and `key` was already present; `null` otherwise (i.e., when `getOld` is false or `key` was not in the map)
   */
  private def put0(key: K, value: V, hash: Int, getOld: Boolean): Some[V] | Null = {
    if(contentSize + 1 >= threshold) growTable(table.length * 2)
    val idx = index(hash)
    put0(key, value, getOld, hash, idx)
  }

  private def put0(key: K, value: V, getOld: Boolean): Some[V] | Null = {
    if(contentSize + 1 >= threshold) growTable(table.length * 2)
    val hash = computeHash(key)
    val idx = index(hash)
    put0(key, value, getOld, hash, idx)
  }


  private def put0(key: K, value: V, getOld: Boolean, hash: Int, idx: Int): Some[V] | Null = {
    table(idx) match {
      case null =>
        table(idx) = new Node[K, V](key, hash, value, null)
      case old =>
        var prev: Node[K, V] | Null = null
        var n: Node[K, V] | Null = old
        while((n ne null) && n.hash <= hash) {
          if(n.hash == hash && key == n.key) {
            val old = n.value
            n.value = value
            return if(getOld) Some(old) else null
          }
          prev = n
          n = n.next
        }
        if(prev eq null) table(idx) = new Node(key, hash, value, old)
        else prev.next = new Node(key, hash, value, prev.next)
    }
    contentSize += 1
    null
  }

  private def remove0(elem: K) : Node[K, V] | Null = remove0(elem, computeHash(elem))

  /** Removes a key from this map if it exists
   *
   *  @param elem the element to remove
   *  @param hash the **improved** hashcode of `element` (see computeHash)
   *  @return the node that contained element if it was present, otherwise null
   */
  private def remove0(elem: K, hash: Int) : Node[K, V] | Null = {
    val idx = index(hash)
    table(idx) match {
      case null => null
      case nd if nd.hash == hash && nd.key == elem =>
        // first element matches
        table(idx) = nd.next
        contentSize -= 1
        nd
      case nd =>
        // find an element that matches
        var prev = nd
        var next = nd.next
        while((next ne null) && next.hash <= hash) {
          if(next.hash == hash && next.key == elem) {
            prev.next = next.next
            contentSize -= 1
            return next
          }
          prev = next
          next = next.next
        }
        null
    }
  }

  private abstract class HashMapIterator[A] extends AbstractIterator[A] {
    private var i = 0
    private var node: Node[K, V] | Null = null
    private val len = table.length

    protected def extract(nd: Node[K, V]): A

    def hasNext: Boolean = {
      if(node ne null) true
      else {
        while(i < len) {
          val n = table(i)
          i += 1
          if(n ne null) { node = n; return true }
        }
        false
      }
    }

    def next(): A =
      if(!hasNext) Iterator.empty.next()
      else {
        val r = extract(node.nn)
        node = node.nn.next
        r
      }
  }

  /** Returns an iterator over the key-value pairs of this map. The iteration order is not
   *  specified and may change when the map is modified.
   */
  override def iterator: Iterator[(K, V)] =
    if(size == 0) Iterator.empty
    else new HashMapIterator[(K, V)] {
      protected def extract(nd: Node[K, V]) = (nd.key, nd.value)
    }

  /** Returns an iterator over the keys of this map. The iteration order is not specified
   *  and may change when the map is modified.
   */
  override def keysIterator: Iterator[K] =
    if(size == 0) Iterator.empty
    else new HashMapIterator[K] {
      protected def extract(nd: Node[K, V]) = nd.key
    }

  /** Returns an iterator over the values of this map. The iteration order is not specified
   *  and may change when the map is modified.
   */
  override def valuesIterator: Iterator[V] =
    if(size == 0) Iterator.empty
    else new HashMapIterator[V] {
      protected def extract(nd: Node[K, V]) = nd.value
    }


  /** Returns an iterator over the nodes stored in this HashMap. */
  private[collection] def nodeIterator: Iterator[Node[K, V]] =
    if(size == 0) Iterator.empty
    else new HashMapIterator[Node[K, V]] {
      protected def extract(nd: Node[K, V]) = nd
    }

  /** Returns a [[Stepper]] for the key-value pairs of this map, stepping through the hash
   *  table directly.
   *
   *  @tparam S the type of `Stepper` to use, determined by the implicit `StepperShape`
   *  @param shape the implicit `StepperShape` that selects the `Stepper` type for `(K, V)` pairs
   *  @return a `Stepper` over the key-value pairs of this map, supporting efficient splitting
   */
  override def stepper[S <: Stepper[?]](implicit shape: StepperShape[(K, V), S]): S & EfficientSplit =
    shape.
      parUnbox(new convert.impl.AnyTableStepper[(K, V), Node[K, V]](size, table, _.next, node => (node.key, node.value), 0, table.length)).
      asInstanceOf[S & EfficientSplit]

  /** Returns a [[Stepper]] for the keys of this map, stepping through the hash table directly.
   *
   *  @tparam S the type of `Stepper` to use, determined by the implicit `StepperShape`
   *  @param shape the implicit `StepperShape` that selects the appropriate primitive or boxed `Stepper` for `K`
   *  @return a `Stepper` over the keys of this map, specialized for primitives when the
   *          resolved `StepperShape` corresponds to `Int`, `Long`, or `Double`, and supporting
   *          efficient splitting
   */
  override def keyStepper[S <: Stepper[?]](implicit shape: StepperShape[K, S]): S & EfficientSplit = {
    import convert.impl._
    val s = shape.shape match {
      case StepperShape.IntShape    => new IntTableStepper[Node[K, V]]   (size, table, _.next, _.key.asInstanceOf[Int],    0, table.length)
      case StepperShape.LongShape   => new LongTableStepper[Node[K, V]]  (size, table, _.next, _.key.asInstanceOf[Long],   0, table.length)
      case StepperShape.DoubleShape => new DoubleTableStepper[Node[K, V]](size, table, _.next, _.key.asInstanceOf[Double], 0, table.length)
      case _         => shape.parUnbox(new AnyTableStepper[K, Node[K, V]](size, table, _.next, _.key,                      0, table.length))
    }
    s.asInstanceOf[S & EfficientSplit]
  }

  /** Returns a [[Stepper]] for the values of this map, stepping through the hash table directly.
   *
   *  @tparam S the type of `Stepper` to use, determined by the implicit `StepperShape`
   *  @param shape the implicit `StepperShape` that selects the appropriate primitive or boxed `Stepper` for `V`
   *  @return a `Stepper` over the values of this map, specialized for primitives when the
   *          resolved `StepperShape` corresponds to `Int`, `Long`, or `Double`, and supporting
   *          efficient splitting
   */
  override def valueStepper[S <: Stepper[?]](implicit shape: StepperShape[V, S]): S & EfficientSplit = {
    import convert.impl._
    val s = shape.shape match {
      case StepperShape.IntShape    => new IntTableStepper[Node[K, V]]   (size, table, _.next, _.value.asInstanceOf[Int],    0, table.length)
      case StepperShape.LongShape   => new LongTableStepper[Node[K, V]]  (size, table, _.next, _.value.asInstanceOf[Long],   0, table.length)
      case StepperShape.DoubleShape => new DoubleTableStepper[Node[K, V]](size, table, _.next, _.value.asInstanceOf[Double], 0, table.length)
      case _         => shape.parUnbox(new AnyTableStepper[V, Node[K, V]](size, table, _.next, _.value,                      0, table.length))
    }
    s.asInstanceOf[S & EfficientSplit]
  }

  private def growTable(newlen: Int) = {
    if (newlen < 0)
      throw new RuntimeException(s"new HashMap table size $newlen exceeds maximum")
    var oldlen = table.length
    threshold = newThreshold(newlen)
    if(size == 0) table = new Array[Node[K, V] | Null](newlen)
    else {
      table = java.util.Arrays.copyOf(table, newlen)
      val preLow: Node[K, V] = new Node(null.asInstanceOf[K], 0, null.asInstanceOf[V], null)
      val preHigh: Node[K, V] = new Node(null.asInstanceOf[K], 0, null.asInstanceOf[V], null)
      // Split buckets until the new length has been reached. This could be done more
      // efficiently when growing an already filled table to more than double the size.
      while(oldlen < newlen) {
        var i = 0
        while (i < oldlen) {
          val old = table(i)
          if(old ne null) {
            preLow.next = null
            preHigh.next = null
            var lastLow: Node[K, V] = preLow
            var lastHigh: Node[K, V] = preHigh
            var n: Node[K, V] | Null = old
            while(n ne null) {
              val next = n.next
              if((n.hash & oldlen) == 0) { // keep low
                lastLow.next = n
                lastLow = n
              } else { // move to high
                lastHigh.next = n
                lastHigh = n
              }
              n = next
            }
            lastLow.next = null
            if(old ne preLow.next) table(i) = preLow.next
            if(preHigh.next ne null) {
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

  private def tableSizeFor(capacity: Int) =
    (Integer.highestOneBit((capacity-1).max(4))*2).min(1 << 30)

  private def newThreshold(size: Int) = (size.toDouble * loadFactor).toInt

  /** Removes all key-value pairs from this map. The internal table keeps its current capacity. */
  override def clear(): Unit = {
    java.util.Arrays.fill(table.asInstanceOf[Array[AnyRef]], null)
    contentSize = 0
  }

  /** Returns the value associated with `key` in this map, wrapped in `Some`, or `None` if
   *  `key` is not present.
   *
   *  @param key the key to look up
   */
  def get(key: K): Option[V] = findNode(key) match {
    case null => None
    case nd => Some(nd.value)
  }

  /** Returns the value associated with `key`, or the result of `default(key)` if `key` is not
   *  in this map.
   *
   *  @param key the key to look up
   *  @return the value bound to `key`, or `default(key)` otherwise
   *  @throws NoSuchElementException if `key` is not in this map and the `default` method is
   *          not overridden to return a value instead of throwing
   */
  @throws[NoSuchElementException]
  override def apply(key: K): V = findNode(key) match {
    case null => default(key)
    case nd => nd.value
  }

  /** Returns the value associated with `key`, or `default` if `key` is not in this map.
   *  For a plain `HashMap` the lookup avoids allocating an `Option`; subclasses that might
   *  customise `get` go through the generic implementation.
   *
   *  @tparam V1 the result type, a supertype of this map's value type
   *  @param key the key to look up
   *  @param default the value to return when `key` is absent; evaluated only in that case
   *  @return the value bound to `key`, or `default` if there is no such binding
   */
  override def getOrElse[V1 >: V](key: K, default: => V1): V1 = {
    if (getClass != classOf[HashMap[?, ?]]) {
      // subclasses of HashMap might customise `get` ...
      super.getOrElse(key, default)
    } else {
      // .. but in the common case, we can avoid the Option boxing.
      val nd = findNode(key)
      if (nd eq null) default else nd.value
    }
  }

  /** Returns the value associated with `key`; if `key` is not in this map, evaluates
   *  `defaultValue`, adds a binding from `key` to the result, and returns it.
   *
   *  `defaultValue` is evaluated at most once, and only when `key` is absent. For a plain
   *  `HashMap` the key is hashed and located only once for both the lookup and the insertion,
   *  and if evaluating `defaultValue` resizes this map's table, the new binding is still
   *  inserted at the correct position; subclasses that might customise `get` go through the
   *  generic implementation.
   *
   *  @param key the key to look up
   *  @param defaultValue the value to bind to `key` if it is absent; evaluated only in that case
   *  @return the value now associated with `key`
   */
  override def getOrElseUpdate(key: K, defaultValue: => V): V = {
    if (getClass != classOf[HashMap[?, ?]]) {
      // subclasses of HashMap might customise `get` ...
      super.getOrElseUpdate(key, defaultValue)
    } else {
      val hash = computeHash(key)
      val idx = index(hash)
      val nd = table(idx) match {
        case null => null
        case nd => nd.findNode(key, hash)
      }
      if(nd != null) nd.value
      else {
        val table0 = table
        val default = defaultValue
        if(contentSize + 1 >= threshold) growTable(table.length * 2)
        // Avoid recomputing index if the `defaultValue()` or new element hasn't triggered a table resize.
        val newIdx = if (table0 eq table) idx else index(hash)
        put0(key, default, getOld = false, hash, newIdx)
        default
      }
    }
  }

  /** Adds a new key/value pair to this map, returning the previously bound value as an option.
   *  If the map already contains a binding for `key`, its value is replaced by `value`.
   *
   *  @param key the key to add
   *  @param value the value to bind to `key`
   *  @return `Some(previousValue)` if `key` was already bound to `previousValue`, `None` if
   *          `key` was not in the map before
   */
  override def put(key: K, value: V): Option[V] = put0(key, value, getOld = true) match {
    case null => None
    case sm => sm
  }

  /** Removes the binding for `key` from this map, if present.
   *
   *  @param key the key whose binding is removed
   *  @return `Some(value)` if `key` was bound to `value` before the removal, `None` if `key`
   *          was not in the map
   */
  override def remove(key: K): Option[V] = remove0(key) match {
    case null => None
    case nd => Some(nd.value)
  }

  /** Adds a new key/value pair to this map. If the map already contains a binding for `key`,
   *  its value is replaced by `value`.
   *
   *  @param key the key to add
   *  @param value the value to bind to `key`
   */
  override def update(key: K, value: V): Unit = put0(key, value, getOld = false)

  /** Adds a single key-value pair to this map, replacing the value of an existing binding for
   *  the same key.
   *
   *  @param elem the key-value pair to add
   *  @return this map
   */
  def addOne(elem: (K, V)): this.type = { put0(elem._1, elem._2, getOld = false); this }

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
      if(n ne null) n.foreach(f)
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
      if(n ne null) n.foreachEntry(f)
      i += 1
    }
  }

  /** Replaces this map with a serialization proxy during Java serialization. The proxy records
   *  the current table length and load factor so deserialization can restore an equivalent map.
   */
  protected def writeReplace(): AnyRef = new DefaultSerializationProxy(new mutable.HashMap.DeserializationFactory[K, V](table.length, loadFactor), this)

  /** Removes all key-value pairs from this map for which the predicate returns `false`.
   *
   *  @param p the predicate used to test key-value pairs; entries for which it returns `false` are removed
   *  @return this map
   */
  override def filterInPlace(p: (K, V) => Boolean): this.type = {
    if (nonEmpty) {
      var bucket = 0

      while (bucket < table.length) {
        var head = table(bucket)

        while ((head ne null) && !p(head.key, head.value)) {
          head = head.next
          contentSize -= 1
        }

        if (head ne null) {
          var prev = head
          var next = head.next

          while (next ne null) {
            if (p(next.key, next.value)) {
              prev = next
            } else {
              prev.next = next.next
              contentSize -= 1
            }
            next = next.next
          }
        }

        table(bucket) = head
        bucket += 1
      }
    }
    this
  }

  // TODO: rename to `mapValuesInPlace` and override the base version (not binary compatible)
  private[mutable] def mapValuesInPlaceImpl(f: (K, V) => V): this.type = {
    val len = table.length
    var i = 0
    while (i < len) {
      var n = table(i)
      while (n ne null) {
        n.value = f(n.key, n.value)
        n = n.next
      }
      i += 1
    }
    this
  }

  /** Returns the companion object [[HashMap]], used to build hash maps of the same kind. */
  override def mapFactory: MapFactory[HashMap] = HashMap

  /** The name of this collection class, `"HashMap"`, used as the prefix in `toString`. */
  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected def stringPrefix = "HashMap"

  /** Returns the hash code of this map, computed as an unordered [[scala.util.hashing.MurmurHash3]]
   *  hash of the key-value pair hashes (or `MurmurHash3.emptyMapHash` when the map is empty).
   *  Each pair hash combines the key's original hash, recovered from the value cached in the
   *  node (via `unimproveHash`) rather than recomputed, with the value's hash code, so the
   *  result is the same as for other maps with equal contents.
   */
  override def hashCode(): Int = {
    if (isEmpty) MurmurHash3.emptyMapHash
    else {
      val tupleHashIterator = new HashMapIterator[Any] {
        var hash: Int = 0
        override def hashCode(): Int = hash
        override protected def extract(nd: Node[K, V]): Any = {
          hash = MurmurHash3.tuple2Hash(unimproveHash(nd.hash), nd.value.##)
          this
        }
      }
      MurmurHash3.unorderedHash(tupleHashIterator, MurmurHash3.mapSeed)
    }
  }
}

/** $factoryInfo
 *  @define Coll `mutable.HashMap`
 *  @define coll mutable hash map
 */
@SerialVersionUID(3L)
object HashMap extends MapFactory[HashMap] {

  /** Creates a new, empty hash map with the default initial capacity (16) and load factor (0.75).
   *
   *  @tparam K the key type of the new map
   *  @tparam V the value type of the new map
   *  @return a new, empty `HashMap`
   */
  def empty[K, V]: HashMap[K, V] = new HashMap[K, V]

  /** Creates a new hash map containing the key-value pairs of the given collection. When the
   *  size of `it` is known, the initial capacity is chosen so that all pairs can be added
   *  without resizing the table.
   *
   *  @tparam K the key type of the new map
   *  @tparam V the value type of the new map
   *  @param it the collection whose key-value pairs are added to the new map
   *  @return a new `HashMap` containing the key-value pairs of `it`
   */
  def from[K, V](it: collection.IterableOnce[(K, V)]^): HashMap[K, V] = {
    val k = it.knownSize
    val cap = if(k > 0) ((k + 1).toDouble / defaultLoadFactor).toInt else defaultInitialCapacity
    new HashMap[K, V](cap, defaultLoadFactor).addAll(it)
  }

  /** Creates a new builder for a `HashMap` with the default initial capacity and load factor.
   *
   *  @tparam K the key type of the map to build
   *  @tparam V the value type of the map to build
   *  @return a new builder producing a `HashMap`
   */
  def newBuilder[K, V]: Builder[(K, V), HashMap[K, V]] = newBuilder(defaultInitialCapacity, defaultLoadFactor)

  /** Creates a new builder for a `HashMap` with the given initial capacity and load factor.
   *  Size hints given to the builder are forwarded to the underlying map's `sizeHint`.
   *
   *  @tparam K the key type of the map to build
   *  @tparam V the value type of the map to build
   *  @param initialCapacity the initial capacity of the map's hash table
   *  @param loadFactor the load factor of the map's hash table
   *  @return a new builder producing a `HashMap`
   */
  def newBuilder[K, V](initialCapacity: Int, loadFactor: Double): Builder[(K, V), HashMap[K, V]] =
    new GrowableBuilder[(K, V), HashMap[K, V]](new HashMap[K, V](initialCapacity, loadFactor)) {
      override def sizeHint(size: Int) = elems.sizeHint(size)
    }

  /** The default load factor for the hash table. */
  final def defaultLoadFactor: Double = 0.75

  /** The default initial capacity for the hash table. */
  final def defaultInitialCapacity: Int = 16

  @SerialVersionUID(3L)
  private final class DeserializationFactory[K, V](val tableLength: Int, val loadFactor: Double) extends Factory[(K, V), HashMap[K, V]], Serializable {
    def fromSpecific(it: IterableOnce[(K, V)]^): HashMap[K, V] = new HashMap[K, V](tableLength, loadFactor).addAll(it)
    def newBuilder: Builder[(K, V), HashMap[K, V]] = HashMap.newBuilder(tableLength, loadFactor)
  }

  private[collection] final class Node[K, V](_key: K, _hash: Int, private var _value: V, @annotation.stableNull private var _next: Node[K, V] | Null) {
    def key: K = _key
    def hash: Int = _hash
    def value: V = _value
    def value_= (v: V): Unit = _value = v
    def next: Node[K, V] | Null = _next
    def next_= (n: Node[K, V] | Null): Unit = _next = n

    @tailrec
    def findNode(k: K, h: Int): Node[K, V] | Null =
      if(h == _hash && k == _key) this
      else if((_next eq null) || (_hash > h)) null
      else _next.findNode(k, h)

    @tailrec
    def foreach[U](f: ((K, V)) => U): Unit = {
      f((_key, _value))
      if(_next ne null) _next.foreach(f)
    }

    @tailrec
    def foreachEntry[U](f: (K, V) => U): Unit = {
      f(_key, _value)
      if(_next ne null) _next.foreachEntry(f)
    }

    override def toString() = s"Node($key, $value, $hash) -> $next"
  }
}

