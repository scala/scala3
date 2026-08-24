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
package collection.immutable

import scala.language.`2.13`
import language.experimental.captureChecking

import java.lang.Integer.bitCount
import java.lang.System.arraycopy

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.collection.Hashing.improve
import scala.collection.Stepper.EfficientSplit
import scala.collection.generic.DefaultSerializable
import scala.collection.mutable, mutable.ReusableBuilder
import scala.collection.{Iterator, MapFactory, MapFactoryDefaults, Stepper, StepperShape, mutable}
import scala.runtime.AbstractFunction2
import scala.runtime.Statics.releaseFence
import scala.util.hashing.MurmurHash3

/** This class implements immutable maps using a Compressed Hash-Array Mapped Prefix-tree.
 *  See paper https://michael.steindorfer.name/publications/oopsla15.pdf for more details.
 *
 *  @tparam K      the type of the keys contained in this hash set.
 *  @tparam V      the type of the values associated with the keys in this hash map.
 *
 *  @define Coll `immutable.HashMap`
 *  @define coll immutable champ hash map
 */

final class HashMap[K, +V] private[immutable] (private[immutable] val rootNode: BitmapIndexedMapNode[K, V])
  extends AbstractMap[K, V]
    with StrictOptimizedMapOps[K, V, HashMap, HashMap[K, V]]
    with MapFactoryDefaults[K, V, HashMap, Iterable]
    with DefaultSerializable {

  /** Creates an empty map, backed by the shared empty root node. */
  def this() = this(MapNode.empty)

  // This release fence is present because rootNode may have previously been mutated during construction.
  releaseFence()

  /** Returns the [[HashMap$ HashMap]] companion object, the factory used to build new hash maps. */
  override def mapFactory: MapFactory[HashMap] = HashMap

  /** Returns the number of key-value pairs in this map. The size is cached in the root node,
   *  so this is always known and never `-1`.
   */
  override def knownSize: Int = rootNode.size

  /** Returns the number of key-value pairs in this map, read from the cached count in the root node. */
  override def size: Int = rootNode.size

  /** Returns `true` if this map contains no key-value pairs, `false` otherwise. */
  override def isEmpty: Boolean = rootNode.size == 0

  /** Returns the set of all keys in this map. For a non-empty map the result shares this map's
   *  trie structure rather than copying the keys; for an empty map it is `Set.empty`.
   */
  override def keySet: Set[K] = if (size == 0) Set.empty else new HashKeySet


  private[immutable] final class HashKeySet extends LazyImmutableKeySet {

    private def newKeySetOrThis(newHashMap: HashMap[K, ?]): Set[K] =
      if (newHashMap eq HashMap.this) this else newHashMap.keySet
    private def newKeySetOrThis(newRootNode: BitmapIndexedMapNode[K, ?]): Set[K] =
      if (newRootNode eq rootNode) this else new HashMap(newRootNode).keySet

    /** Returns a set containing `elem` and all keys of the underlying map. If `elem` is already a
     *  key, returns this set. The element is inserted into the underlying trie with a `null` value
     *  and `replaceValue = false`, so an existing entry for `elem` is left untouched.
     *
     *  @param elem the element to add
     *  @return a key set containing `elem` and all existing keys, or this set if `elem` is
     *          already present
     */
    override def incl(elem: K): Set[K] = {
      val originalHash = elem.##
      val improvedHash = improve(originalHash)
      val newNode = rootNode.updated(elem, null.asInstanceOf[V], originalHash, improvedHash, 0, replaceValue = false)
      newKeySetOrThis(newNode)
    }
    /** Returns a set containing all keys of the underlying map except `elem`, or this set if
     *  `elem` is not a key.
     *
     *  @param elem the element to remove
     *  @return a key set without `elem`, or this set if `elem` is not present
     */
    override def excl(elem: K): Set[K] = newKeySetOrThis(HashMap.this - elem)
    /** Returns a set of all keys that satisfy the predicate, by filtering the underlying map on
     *  its keys.
     *
     *  @param pred the predicate used to test keys
     *  @return a key set of all keys satisfying `pred`, or this set if all keys satisfy it
     */
    override def filter(pred: K => Boolean): Set[K] = newKeySetOrThis(HashMap.this.filter(kv => pred(kv._1)))
    /** Returns a set of all keys that do not satisfy the predicate, by filtering the underlying
     *  map on its keys.
     *
     *  @param pred the predicate used to test keys
     *  @return a key set of all keys not satisfying `pred`, or this set if no key satisfies it
     */
    override def filterNot(pred: K => Boolean): Set[K] = newKeySetOrThis(HashMap.this.filterNot(kv => pred(kv._1)))
  }

  /** Returns an iterator over the key-value pairs of this map, in the depth-first order of the
   *  underlying trie (payload entries of a node before its sub-nodes).
   */
  def iterator: Iterator[(K, V)] = {
    if (isEmpty) Iterator.empty
    else new MapKeyValueTupleIterator[K, V](rootNode)
  }

  /** Returns an iterator over the keys of this map, in the same order as `iterator`, without
   *  allocating a tuple per entry.
   */
  override def keysIterator: Iterator[K] = {
    if (isEmpty) Iterator.empty
    else new MapKeyIterator[K, V](rootNode)
  }
  /** Returns an iterator over the values of this map, in the same order as `iterator`, without
   *  allocating a tuple per entry.
   */
  override def valuesIterator: Iterator[V] = {
    if (isEmpty) Iterator.empty
    else new MapValueIterator[K, V](rootNode)
  }

  /** Returns an iterator over the key-value pairs of this map in the exact reverse order of
   *  `iterator`. Used to implement `last` and `init`.
   */
  protected[immutable] def reverseIterator: Iterator[(K, V)] = {
    if (isEmpty) Iterator.empty
    else new MapKeyValueTupleReverseIterator[K, V](rootNode)
  }

  /** Returns a stepper over the key-value pairs of this map that supports efficient splitting,
   *  for use with Java streams and parallel processing. The stepper walks the trie nodes
   *  directly rather than going through an iterator.
   *
   *  @tparam S the type of the stepper, determined by `shape`
   *  @param shape the implicit evidence selecting the stepper implementation for `(K, V)` tuples
   *  @return an efficiently splittable stepper over the key-value pairs
   */
  override def stepper[S <: Stepper[?]](implicit shape: StepperShape[(K, V), S]): S & EfficientSplit =
    shape.
      parUnbox(collection.convert.impl.AnyChampStepper.from[(K, V), MapNode[K, V]](size, rootNode, (node, i) => node.getPayload(i)))

  /** Returns a stepper over the keys of this map that supports efficient splitting, for use with
   *  Java streams and parallel processing. When the key type is `Int`, `Long`, or `Double`, the
   *  returned stepper is a primitive one that avoids boxing.
   *
   *  @tparam S the type of the stepper, determined by `shape`
   *  @param shape the implicit evidence selecting the stepper implementation for the key type
   *  @return an efficiently splittable stepper over the keys
   */
  override def keyStepper[S <: Stepper[?]](implicit shape: StepperShape[K, S]): S & EfficientSplit = {
    import collection.convert.impl._
    val s = shape.shape match {
      case StepperShape.IntShape    => IntChampStepper.from[   MapNode[K, V]](size, rootNode, (node, i) => node.getKey(i).asInstanceOf[Int])
      case StepperShape.LongShape   => LongChampStepper.from[  MapNode[K, V]](size, rootNode, (node, i) => node.getKey(i).asInstanceOf[Long])
      case StepperShape.DoubleShape => DoubleChampStepper.from[MapNode[K, V]](size, rootNode, (node, i) => node.getKey(i).asInstanceOf[Double])
      case _         => shape.parUnbox(AnyChampStepper.from[K, MapNode[K, V]](size, rootNode, (node, i) => node.getKey(i)))
    }
    s.asInstanceOf[S & EfficientSplit]
  }

  /** Returns a stepper over the values of this map that supports efficient splitting, for use
   *  with Java streams and parallel processing. When the value type is `Int`, `Long`, or
   *  `Double`, the returned stepper is a primitive one that avoids boxing.
   *
   *  @tparam S the type of the stepper, determined by `shape`
   *  @param shape the implicit evidence selecting the stepper implementation for the value type
   *  @return an efficiently splittable stepper over the values
   */
  override def valueStepper[S <: Stepper[?]](implicit shape: StepperShape[V, S]): S & EfficientSplit = {
    import collection.convert.impl._
    val s = shape.shape match {
      case StepperShape.IntShape    => IntChampStepper.from[   MapNode[K, V]](size, rootNode, (node, i) => node.getValue(i).asInstanceOf[Int])
      case StepperShape.LongShape   => LongChampStepper.from[  MapNode[K, V]](size, rootNode, (node, i) => node.getValue(i).asInstanceOf[Long])
      case StepperShape.DoubleShape => DoubleChampStepper.from[MapNode[K, V]](size, rootNode, (node, i) => node.getValue(i).asInstanceOf[Double])
      case _         => shape.parUnbox(AnyChampStepper.from[V, MapNode[K, V]](size, rootNode, (node, i) => node.getValue(i)))
    }
    s.asInstanceOf[S & EfficientSplit]
  }

  /** Tests whether this map contains a binding for a key.
   *
   *  @param key the key to look up
   *  @return `true` if this map contains a binding for `key`, `false` otherwise
   */
  override final def contains(key: K): Boolean = {
    val keyUnimprovedHash = key.##
    val keyHash = improve(keyUnimprovedHash)
    rootNode.containsKey(key, keyUnimprovedHash, keyHash, 0)
  }

  /** Returns the value associated with a key.
   *
   *  @param key the key to look up
   *  @return the value associated with `key`
   *  @throws NoSuchElementException if this map contains no binding for `key`
   */
  override def apply(key: K): V = {
    val keyUnimprovedHash = key.##
    val keyHash = improve(keyUnimprovedHash)
    rootNode.apply(key, keyUnimprovedHash, keyHash, 0)
  }

  /** Optionally returns the value associated with a key.
   *
   *  @param key the key to look up
   *  @return `Some(value)` if `key` is bound to `value` in this map, `None` otherwise
   */
  def get(key: K): Option[V] = {
    val keyUnimprovedHash = key.##
    val keyHash = improve(keyUnimprovedHash)
    rootNode.get(key, keyUnimprovedHash, keyHash, 0)
  }

  /** Returns the value associated with a key, or a default value if the key is not contained in
   *  this map.
   *
   *  @tparam V1 the result type, a supertype of `V`
   *  @param key the key to look up
   *  @param default a computation that yields the default value; only evaluated if `key` is not
   *                bound in this map
   *  @return the value bound to `key`, or `default` if `key` is not bound
   */
  override def getOrElse[V1 >: V](key: K, default: => V1): V1 = {
    val keyUnimprovedHash = key.##
    val keyHash = improve(keyUnimprovedHash)
    rootNode.getOrElse(key, keyUnimprovedHash, keyHash, 0, default)
  }

  @inline private def newHashMapOrThis[V1 >: V](newRootNode: BitmapIndexedMapNode[K, V1]): HashMap[K, V1] =
    if (newRootNode eq rootNode) this else new HashMap(newRootNode)

  /** Returns a map containing all key-value pairs of this map, with `key` bound to `value`,
   *  replacing any existing binding for `key`. If `key` is already bound to a value that is
   *  reference-equal to `value`, returns this map unchanged.
   *
   *  @tparam V1 the value type of the returned map, a supertype of `V`
   *  @param key the key to add or update
   *  @param value the value to associate with `key`
   *  @return a map with `key` bound to `value`, or this map if the binding is already present
   */
  def updated[V1 >: V](key: K, value: V1): HashMap[K, V1] = {
    val keyUnimprovedHash = key.##
    newHashMapOrThis(rootNode.updated(key, value, keyUnimprovedHash, improve(keyUnimprovedHash), 0, replaceValue = true))
  }

  // preemptively overridden in anticipation of performance optimizations
  /** Updates the binding for a key based on its current, optional value. The remapping function
   *  is applied to `Some(currentValue)` if `key` is bound, or to `None` otherwise; the binding
   *  is then set to the value in the function's result, or removed if the result is `None`.
   *  Delegates to the inherited implementation; this override exists only so an optimized
   *  version can be added without breaking binary compatibility.
   *
   *  @tparam V1 the value type of the returned map, a supertype of `V`
   *  @param key the key whose binding is updated
   *  @param remappingFunction the function mapping the current optional value to the new
   *                          optional value
   *  @return a map with the binding for `key` updated, added, or removed according to the
   *          result of `remappingFunction`
   */
  override def updatedWith[V1 >: V](key: K)(remappingFunction: Option[V] => Option[V1]): HashMap[K, V1] =
    super.updatedWith[V1](key)(remappingFunction)

  /** Returns a map containing all key-value pairs of this map except any binding for `key`. If
   *  `key` is not bound, returns this map unchanged.
   *
   *  @param key the key to remove
   *  @return a map without a binding for `key`, or this map if none was present
   */
  def removed(key: K): HashMap[K, V] = {
    val keyUnimprovedHash = key.##
    newHashMapOrThis(rootNode.removed(key, keyUnimprovedHash, improve(keyUnimprovedHash), 0))
  }

  /** Returns a map containing all key-value pairs of this map and of `that`. For keys present in
   *  both, the binding from `that` wins. When `that` is another `HashMap`, the two tries are
   *  merged node by node, sharing unchanged subtrees with the inputs; for other collection types
   *  the entries of `that` are added one at a time, mutating freshly created private nodes in
   *  place. Where possible, one of the two original maps is returned unchanged.
   *
   *  @tparam V1 the value type of the returned map, a supertype of `V`
   *  @param that the collection of key-value pairs to add
   *  @return a map containing all pairs of this map and `that`, with `that` taking precedence
   *          on duplicate keys
   */
  override def concat[V1 >: V](that: scala.IterableOnce[(K, V1)]^): HashMap[K, V1] = (that: @unchecked) match {
    case hm: HashMap[K, V1] =>
      if (isEmpty) hm
      else {
        val newNode = rootNode.concat(hm.rootNode, 0)
        if (newNode eq hm.rootNode) hm
        else newHashMapOrThis(newNode)
      }
    case hm: mutable.HashMap[K @unchecked, V @unchecked] =>
      val iter = hm.nodeIterator
      var current = rootNode
      while (iter.hasNext) {
        val next = iter.next()
        val originalHash = hm.unimproveHash(next.hash)
        val improved = improve(originalHash)
        current = current.updated(next.key, next.value, originalHash, improved, 0, replaceValue = true)

        if (current ne rootNode) {
          var shallowlyMutableNodeMap = Node.bitposFrom(Node.maskFrom(improved, 0))

          while (iter.hasNext) {
            val next = iter.next()
            val originalHash = hm.unimproveHash(next.hash)
            shallowlyMutableNodeMap = current.updateWithShallowMutations(next.key, next.value, originalHash, improve(originalHash), 0, shallowlyMutableNodeMap)
          }
          return new HashMap(current)
        }
      }
      this
    case lhm: mutable.LinkedHashMap[K @unchecked, V @unchecked] =>
      val iter = lhm.entryIterator
      var current = rootNode
      while (iter.hasNext) {
        val next = iter.next()
        val originalHash = lhm.unimproveHash(next.hash)
        val improved = improve(originalHash)
        current = current.updated(next.key, next.value, originalHash, improved, 0, replaceValue = true)

        if (current ne rootNode) {
          var shallowlyMutableNodeMap = Node.bitposFrom(Node.maskFrom(improved, 0))

          while (iter.hasNext) {
            val next = iter.next()
            val originalHash = lhm.unimproveHash(next.hash)
            shallowlyMutableNodeMap = current.updateWithShallowMutations(next.key, next.value, originalHash, improve(originalHash), 0, shallowlyMutableNodeMap)
          }
          return new HashMap(current)
        }
      }
      this
    case _ =>
      class accum extends AbstractFunction2[K, V1, Unit] with Function1[(K, V1), Unit] {
        /** Whether any addition so far has produced a root node different from `rootNode`. Once
         *  true, `current` is a private copy that may be mutated shallowly.
         */
        var changed = false
        /** Bitmap of child node positions of `current` that were freshly created by this
         *  accumulator and may therefore be mutated in place.
         */
        var shallowlyMutableNodeMap: Int = 0
        /** The root node of the map being accumulated; starts as the original `rootNode`. */
        var current: BitmapIndexedMapNode[K, V1] = rootNode
        /** Adds a key-value pair given as a tuple.
         *
         *  @param kv the key-value pair to add
         */
        def apply(kv: (K, V1)) = apply(kv._1, kv._2)
        /** Adds a key-value pair, replacing any existing binding for `key`. The first addition
         *  that changes the root switches to shallowly mutating freshly created nodes for all
         *  subsequent additions.
         *
         *  @param key the key to add or update
         *  @param value the value to associate with `key`
         */
        def apply(key: K, value: V1): Unit = {
          val originalHash = key.##
          val improved = improve(originalHash)
          if (!changed) {
            current = current.updated(key, value, originalHash, improved, 0, replaceValue = true)
            if (current ne rootNode) {
              // Note: We could have started with shallowlyMutableNodeMap = 0, however this way, in the case that
              // the first changed key ended up in a subnode beneath root, we mark that root right away as being
              // shallowly mutable.
              //
              // since key->value has just been inserted, and certainly caused a new root node to be created, we can say with
              // certainty that it either caused a new subnode to be created underneath `current`, in which case we should
              // carry on mutating that subnode, or it ended up as a child data pair of the root, in which case, no harm is
              // done by including its bit position in the shallowlyMutableNodeMap anyways.
              changed = true
              shallowlyMutableNodeMap = Node.bitposFrom(Node.maskFrom(improved, 0))
            }
          } else {
            shallowlyMutableNodeMap = current.updateWithShallowMutations(key, value, originalHash, improved, 0, shallowlyMutableNodeMap)
          }
        }
      }
      (that: @unchecked) match {
        case thatMap: Map[K, V1] =>
          if (thatMap.isEmpty) this
          else {
            val accum = new accum
            thatMap.foreachEntry(accum)
            newHashMapOrThis(accum.current)
          }
        case _ =>
          val it = that.iterator
          if (it.isEmpty) this
          else {
            val accum = new accum
            it.foreach(accum)
            newHashMapOrThis(accum.current)
          }
      }
  }

  /** Returns a map containing all key-value pairs of this map except `head`. As maps are
   *  unordered, which pair is removed is not defined.
   *
   *  @throws NoSuchElementException if this map is empty
   */
  override def tail: HashMap[K, V] = this - head._1

  /** Returns a map containing all key-value pairs of this map except `last`. As maps are
   *  unordered, which pair is removed is not defined.
   *
   *  @throws NoSuchElementException if this map is empty
   */
  override def init: HashMap[K, V] = this - last._1

  /** Returns the first key-value pair in iteration order. As maps are unordered, which pair is
   *  first is not defined.
   *
   *  @throws NoSuchElementException if this map is empty
   */
  override def head: (K, V) = iterator.next()

  /** Returns the last key-value pair in iteration order. As maps are unordered, which pair is
   *  last is not defined.
   *
   *  @throws NoSuchElementException if this map is empty
   */
  override def last: (K, V) = reverseIterator.next()

  /** Applies a function to each key-value pair of this map, walking the trie directly rather
   *  than going through an iterator.
   *
   *  @tparam U the result type of `f`; the results are discarded
   *  @param f the function applied to each key-value pair
   */
  override def foreach[U](f: ((K, V)) => U): Unit = rootNode.foreach(f)

  /** Applies a two-argument function to each key and value of this map, without allocating a
   *  tuple per entry.
   *
   *  @tparam U the result type of `f`; the results are discarded
   *  @param f the function applied to each key and its associated value
   */
  override def foreachEntry[U](f: (K, V) => U): Unit = rootNode.foreachEntry(f)

  /** Applies a function to each key, value, and **original** hash value in this Map.
   *
   *  @param f the function to apply to each key, value, and original hash triple
   */
  @inline private[collection] def foreachWithHash(f: (K, V, Int) => Unit): Unit = rootNode.foreachWithHash(f)

  /** Tests whether this map is equal to another object. Two `HashMap`s are compared by their
   *  root nodes, exploiting the canonical trie structure to compare bitmaps and cached hashes
   *  before contents; any other object is compared with the generic `Map` equality.
   *
   *  @param that the object to compare with
   *  @return `true` if `that` is a map with the same key-value pairs as this map
   */
  override def equals(that: Any): Boolean =
    that match {
      case map: HashMap[?, ?] => (this eq map) || (this.rootNode == map.rootNode)
      case _ => super.equals(that)
    }

  /** Returns a hash code compatible with the universal `Map` hash: the unordered MurmurHash3 of
   *  the key-value pairs. Key hash codes are read from the caches in the trie nodes instead of
   *  being recomputed.
   */
  override def hashCode(): Int = {
    if (isEmpty) MurmurHash3.emptyMapHash
    else {
      // Optimized to avoid recomputation of key hashcodes as these are cached in the nodes and can be assumed to be
      // immutable.
      val hashIterator = new MapKeyValueTupleHashIterator(rootNode)
      val hash = MurmurHash3.unorderedHash(hashIterator, MurmurHash3.mapSeed)
      // assert(hash == super.hashCode())
      hash
    }
  }

  /** The name of this collection class, used as the prefix in its `toString` representation. */
  override protected def className = "HashMap"

  /** Merges this HashMap with an other HashMap by combining all key-value pairs of both maps, and delegating to a merge
   *  function to resolve any key collisions between the two HashMaps.
   *
   *  @example ```scala sc:compile
   *   val left = HashMap(1 -> 1, 2 -> 1)
   *   val right = HashMap(2 -> 2, 3 -> 2)
   *
   *   val merged = left.merged(right){ case ((k0, v0), (k1, v1)) => (k0 + k1) -> (v0 + v1) }
   *     // HashMap(1 -> 1, 3 -> 2, 4 -> 3)
   *
   *  ```
   *
   *  @param that the HashMap to merge this HashMap with
   *  @param mergef the merge function which resolves collisions between the two HashMaps. If `mergef` is null, then
   *               keys from `this` will overwrite keys from `that`, making the behaviour equivalent to
   *               `that.concat(this)`
   *
   *  @note In cases where `mergef` returns keys which themselves collide with other keys returned by `merge`, or
   *       found in `this` or `that`, it is not defined which value will be chosen. For example:
   *
   *       Colliding multiple results of merging:
   *       ```scala sc:compile
   *         // key `3` collides between a result of merging keys `1` and `2`
   *         val left = HashMap(1 -> 1, 2 -> 2)
   *         val right = HashMap(1 -> 1, 2 -> 2)
   *
   *         val merged = left.merged(right){ case (_, (_, v1)) => 3 -> v1 }
   *           // HashMap(3 -> 2) is returned, but it could also have returned HashMap(3 -> 1)
   *       ```
   *       Colliding results of merging with other keys:
   *       ```scala sc:compile
   *         // key `2` collides between a result of merging `1`, and existing key `2`
   *         val left = HashMap(1 -> 1, 2 -> 1)
   *         val right = HashMap(1 -> 2)
   *
   *         val merged = left.merged(right)((_,_) => 2 -> 3)
   *           // HashMap(2 -> 1) is returned, but it could also have returned HashMap(2 -> 3)
   *       ```
   *
   *
   *  @tparam V1 the value type of the resulting HashMap, a supertype of `V`
   *
   *  @return a new `HashMap` containing all key-value pairs from both maps, with `mergef` applied to resolve collisions when non-null
   */
  def merged[V1 >: V](that: HashMap[K, V1])(mergef: ((K, V), (K, V1)) => (K, V1)): HashMap[K, V1] =
    if (mergef == null) {
      that ++ this
    } else {
      if (isEmpty) that
      else if (that.isEmpty) this
      else if (size == 1) {
        val payload@(k, v) = rootNode.getPayload(0)
        val originalHash = rootNode.getHash(0)
        val improved = improve(originalHash)

        if (that.rootNode.containsKey(k, originalHash, improved, 0)) {
          val thatPayload = that.rootNode.getTuple(k, originalHash, improved, 0)
          val (mergedK, mergedV) = mergef(payload, thatPayload)
          val mergedOriginalHash = mergedK.##
          val mergedImprovedHash = improve(mergedOriginalHash)
          new HashMap(that.rootNode.removed(thatPayload._1, originalHash, improved, 0).updated(mergedK, mergedV, mergedOriginalHash, mergedImprovedHash, 0, replaceValue = true))
        } else {
          new HashMap(that.rootNode.updated(k, v, originalHash, improved, 0, replaceValue = true))
        }
      } else if (that.size == 0) {
        val thatPayload@(k, v) = rootNode.getPayload(0)
        val thatOriginalHash = rootNode.getHash(0)
        val thatImproved = improve(thatOriginalHash)

        if (rootNode.containsKey(k, thatOriginalHash, thatImproved, 0)) {
          val payload = rootNode.getTuple(k, thatOriginalHash, thatImproved, 0)
          val (mergedK, mergedV) = mergef(payload, thatPayload)
          val mergedOriginalHash = mergedK.##
          val mergedImprovedHash = improve(mergedOriginalHash)
          new HashMap(rootNode.updated(mergedK, mergedV, mergedOriginalHash, mergedImprovedHash, 0, replaceValue = true))
        } else {
          new HashMap(rootNode.updated(k, v, thatOriginalHash, thatImproved, 0, replaceValue = true))
        }
      } else {
        val builder = new HashMapBuilder[K, V1]
        rootNode.mergeInto(that.rootNode, builder, 0)(mergef)
        builder.result()
      }
    }

  /** Returns a map with the same keys as this map, where each value is the result of applying
   *  `f` to the key and its current value. The trie structure is reused: subtrees whose values
   *  are all returned unchanged (by reference) are shared with this map, and if no value
   *  changes, this map itself is returned.
   *
   *  @tparam W the value type of the returned map
   *  @param f the function applied to each key and its associated value
   *  @return a map with each value replaced by the result of `f`
   */
  override def transform[W](f: (K, V) => W): HashMap[K, W] =
    newHashMapOrThis(rootNode.transform[Any](f)).asInstanceOf[HashMap[K, W]]

  /** Returns a map containing the key-value pairs selected by the predicate; implements both
   *  `filter` (`isFlipped = false`) and `filterNot` (`isFlipped = true`). The trie is filtered
   *  node by node, sharing unchanged subtrees; if nothing is removed, this map is returned.
   *
   *  @param pred the predicate used to test key-value pairs
   *  @param isFlipped if `false`, keeps the pairs satisfying `pred`; if `true`, keeps the pairs
   *                  not satisfying it
   *  @return a map containing the selected key-value pairs
   */
  override protected[collection] def filterImpl(pred: ((K, V)) => Boolean, isFlipped: Boolean): HashMap[K, V] = {
    val newRootNode = rootNode.filterImpl(pred, isFlipped)
    if (newRootNode eq rootNode) this
    else if (newRootNode.size == 0) HashMap.empty
    else new HashMap(newRootNode)
  }

  /** Returns a map containing all key-value pairs of this map except those whose keys occur in
   *  `keys`. Hash sets are handled specially so their cached key hashes are reused instead of
   *  recomputed. Returns this map if no key is removed, and stops early with the empty map once
   *  all entries have been removed.
   *
   *  @param keys the keys to remove
   *  @return a map without bindings for any key in `keys`
   */
  override def removedAll(keys: IterableOnce[K]^): HashMap[K, V] = {
    if (isEmpty) {
      this
    } else {
      (keys: @unchecked) match {
        case hashSet: HashSet[K] =>
          if (hashSet.isEmpty) {
            this
          } else {
            // TODO: Remove all keys from the hashSet in a sub-linear fashion by only visiting the nodes in the tree
            // This can be a direct port of the implementation of `SetNode[A]#diff(SetNode[A])`
            val newRootNode = new MapNodeRemoveAllSetNodeIterator(hashSet.rootNode).removeAll(rootNode)
            if (newRootNode eq rootNode) this
            else if (newRootNode.size <= 0) HashMap.empty
            else new HashMap(newRootNode)
          }
        case hashSet: collection.mutable.HashSet[K] =>
          if (hashSet.isEmpty) {
            this
          } else {
            val iter = hashSet.nodeIterator
            var curr = rootNode

            while (iter.hasNext) {
              val next = iter.next()
              val originalHash = hashSet.unimproveHash(next.hash)
              val improved = improve(originalHash)
              curr = curr.removed(next.key, originalHash, improved, 0)
              if (curr.size == 0) {
                return HashMap.empty
              }
            }
            newHashMapOrThis(curr)
          }
        case lhashSet: collection.mutable.LinkedHashSet[K] =>
          if (lhashSet.isEmpty) {
            this
          } else {
            val iter = lhashSet.entryIterator
            var curr = rootNode

            while (iter.hasNext) {
              val next = iter.next()
              val originalHash = lhashSet.unimproveHash(next.hash)
              val improved = improve(originalHash)
              curr = curr.removed(next.key, originalHash, improved, 0)
              if (curr.size == 0) {
                return HashMap.empty
              }
            }
            newHashMapOrThis(curr)
          }
        case _ =>
          val iter = keys.iterator
          var curr = rootNode
          while (iter.hasNext) {
            val next = iter.next()
            val originalHash = next.##
            val improved = improve(originalHash)
            curr = curr.removed(next, originalHash, improved, 0)
            if (curr.size == 0) {
              return HashMap.empty
            }
          }
          newHashMapOrThis(curr)
      }
    }
  }

  /** Splits this map into a pair of maps: the key-value pairs that satisfy the predicate, and
   *  those that do not. Delegates to the inherited implementation; this override exists only so
   *  an optimized version can be added without breaking binary compatibility.
   *
   *  @param p the predicate used to test key-value pairs
   *  @return a pair of maps: the pairs satisfying `p`, and the pairs not satisfying it
   */
  override def partition(p: ((K, V)) => Boolean): (HashMap[K, V], HashMap[K, V]) = {
    // This method has been preemptively overridden in order to ensure that an optimizing implementation may be included
    // in a minor release without breaking binary compatibility.
    //
    // In particular, `partition` could be optimized to traverse the trie node-by-node, splitting each node into two,
    // based on the result of applying `p` to its elements and subnodes.
    super.partition(p)
  }

  /** Returns a map containing the first `n` key-value pairs in iteration order, or this map if
   *  it has at most `n` entries. As maps are unordered, which pairs are taken is not defined.
   *  Delegates to the inherited implementation; this override exists only so an optimized
   *  version can be added without breaking binary compatibility.
   *
   *  @param n the number of key-value pairs to take
   *  @return a map of the first `n` pairs, the empty map if `n` is non-positive, or this map if
   *          it has at most `n` entries
   */
  override def take(n: Int): HashMap[K, V] = {
    // This method has been preemptively overridden in order to ensure that an optimizing implementation may be included
    // in a minor release without breaking binary compatibility.
    //
    // In particular, `take` could be optimized to construct a new trie structure by visiting each node, and including
    // those nodes in the resulting trie, until `n` total elements have been included.
    super.take(n)
  }

  /** Returns a map containing the last `n` key-value pairs in iteration order, or this map if
   *  it has at most `n` entries. As maps are unordered, which pairs are taken is not defined.
   *  Delegates to the inherited implementation; this override exists only so an optimized
   *  version can be added without breaking binary compatibility.
   *
   *  @param n the number of key-value pairs to take
   *  @return a map of the last `n` pairs, the empty map if `n` is non-positive, or this map if
   *          it has at most `n` entries
   */
  override def takeRight(n: Int): HashMap[K, V] = {
    // This method has been preemptively overridden in order to ensure that an optimizing implementation may be included
    // in a minor release without breaking binary compatibility.
    //
    // In particular, `take` could be optimized to construct a new trie structure by visiting each node in reverse, and
    // and including those nodes in the resulting trie, until `n` total elements have been included.
    super.takeRight(n)
  }

  /** Returns a map containing the longest prefix of key-value pairs, in iteration order, that
   *  all satisfy the predicate. As maps are unordered, the result is not defined beyond that.
   *  Delegates to the inherited implementation; this override exists only so an optimized
   *  version can be added without breaking binary compatibility.
   *
   *  @param p the predicate used to test key-value pairs
   *  @return a map of the longest prefix of pairs satisfying `p`
   */
  override def takeWhile(p: ((K, V)) => Boolean): HashMap[K, V] = {
    // This method has been preemptively overridden in order to ensure that an optimizing implementation may be included
    // in a minor release without breaking binary compatibility.
    //
    // In particular, `takeWhile` could be optimized to construct a new trie structure by visiting each node, and
    // including those nodes in the resulting trie, until `p` returns `false`
    super.takeWhile(p)
  }

  /** Returns a map containing the key-value pairs remaining after dropping the longest prefix,
   *  in iteration order, that all satisfy the predicate. As maps are unordered, the result is
   *  not defined beyond that. Delegates to the inherited implementation; this override exists
   *  only so an optimized version can be added without breaking binary compatibility.
   *
   *  @param p the predicate used to test key-value pairs
   *  @return a map of the pairs remaining after the longest prefix satisfying `p` is dropped
   */
  override def dropWhile(p: ((K, V)) => Boolean): HashMap[K, V] = {
    // This method has been preemptively overridden in order to ensure that an optimizing implementation may be included
    // in a minor release without breaking binary compatibility.
    //
    // In particular, `dropWhile` could be optimized to construct a new trie structure by visiting each node, and
    // dropping those nodes in the resulting trie, until `p` returns `true`
    super.dropWhile(p)
  }

  /** Returns a map containing all key-value pairs except the last `n` in iteration order. As
   *  maps are unordered, which pairs are dropped is not defined. Delegates to the inherited
   *  implementation; this override exists only so an optimized version can be added without
   *  breaking binary compatibility.
   *
   *  @param n the number of key-value pairs to drop
   *  @return a map without the last `n` pairs, or this map if `n` is non-positive
   */
  override def dropRight(n: Int): HashMap[K, V] = {
    // This method has been preemptively overridden in order to ensure that an optimizing implementation may be included
    // in a minor release without breaking binary compatibility.
    //
    // In particular, `dropRight` could be optimized to construct a new trie structure by visiting each node, in reverse
    // order, and dropping all nodes until `n` elements have been dropped
    super.dropRight(n)
  }

  /** Returns a map containing all key-value pairs except the first `n` in iteration order. As
   *  maps are unordered, which pairs are dropped is not defined. Delegates to the inherited
   *  implementation; this override exists only so an optimized version can be added without
   *  breaking binary compatibility.
   *
   *  @param n the number of key-value pairs to drop
   *  @return a map without the first `n` pairs, or this map if `n` is non-positive
   */
  override def drop(n: Int): HashMap[K, V] = {
    // This method has been preemptively overridden in order to ensure that an optimizing implementation may be included
    // in a minor release without breaking binary compatibility.
    //
    // In particular, `dropRight` could be optimized to construct a new trie structure by visiting each node, and
    // dropping all nodes until `n` elements have been dropped
    super.drop(n)
  }

  /** Splits this map into the longest prefix of key-value pairs, in iteration order, that all
   *  satisfy the predicate, and the remainder. As maps are unordered, the split point is not
   *  defined beyond that. Delegates to the inherited implementation; this override exists only
   *  so an optimized version can be added without breaking binary compatibility.
   *
   *  @param p the predicate used to test key-value pairs
   *  @return a pair of maps: the longest prefix satisfying `p`, and the rest
   */
  override def span(p: ((K, V)) => Boolean): (HashMap[K, V], HashMap[K, V]) = {
    // This method has been preemptively overridden in order to ensure that an optimizing implementation may be included
    // in a minor release without breaking binary compatibility.
    //
    // In particular, `scan` could be optimized to construct a new trie structure by visiting each node, and
    // keeping each node and element until `p` returns false, then including the remaining nodes in the second result.
    // This would avoid having to rebuild most of the trie, and would eliminate the need to perform hashing and equality
    // checks.
    super.span(p)
  }

}

private[immutable] object MapNode {

  private final val EmptyMapNode = new BitmapIndexedMapNode(0, 0, Array.empty, Array.empty, 0, 0)

  /** Returns the empty map node: a single shared, immutable instance with empty bitmaps and
   *  arrays, cast to the requested key and value types.
   *
   *  @tparam K the key type of the returned node
   *  @tparam V the value type of the returned node
   *  @return the shared empty `BitmapIndexedMapNode`
   */
  def empty[K, V]: BitmapIndexedMapNode[K, V] = EmptyMapNode.asInstanceOf[BitmapIndexedMapNode[K, V]]

  /** The number of `content` array slots taken by one payload entry: one for the key, one for
   *  the value.
   */
  final val TupleLength = 2

}


private[immutable] sealed abstract class MapNode[K, +V] extends Node[MapNode[K, V @uV]] {
  /** Returns the value associated with `key` in the subtree rooted at this node.
   *
   *  @param key the key to look up
   *  @param originalHash the original hash code of `key` (via `key.##`)
   *  @param hash the improved hash of `key`
   *  @param shift the bit-level offset into the hash code, equal to `depth * BitPartitionSize`
   *  @return the value bound to `key`
   *  @throws NoSuchElementException if `key` is not bound in this subtree
   */
  def apply(key: K, originalHash: Int, hash: Int, shift: Int): V

  /** Optionally returns the value associated with `key` in the subtree rooted at this node.
   *
   *  @param key the key to look up
   *  @param originalHash the original hash code of `key` (via `key.##`)
   *  @param hash the improved hash of `key`
   *  @param shift the bit-level offset into the hash code, equal to `depth * BitPartitionSize`
   *  @return `Some(value)` if `key` is bound in this subtree, `None` otherwise
   */
  def get(key: K, originalHash: Int, hash: Int, shift: Int): Option[V]

  /** Returns the value associated with `key` in the subtree rooted at this node, or the default
   *  value if `key` is not bound.
   *
   *  @tparam V1 the result type, a supertype of `V`
   *  @param key the key to look up
   *  @param originalHash the original hash code of `key` (via `key.##`)
   *  @param hash the improved hash of `key`
   *  @param shift the bit-level offset into the hash code, equal to `depth * BitPartitionSize`
   *  @param f a computation that yields the default value; only evaluated if `key` is not bound
   *  @return the value bound to `key`, or `f` if `key` is not bound
   */
  def getOrElse[V1 >: V](key: K, originalHash: Int, hash: Int, shift: Int, f: => V1): V1

  /** Tests whether `key` is bound in the subtree rooted at this node.
   *
   *  @param key the key to look up
   *  @param originalHash the original hash code of `key` (via `key.##`)
   *  @param hash the improved hash of `key`
   *  @param shift the bit-level offset into the hash code, equal to `depth * BitPartitionSize`
   *  @return `true` if `key` is bound in this subtree, `false` otherwise
   */
  def containsKey(key: K, originalHash: Int, hash: Int, shift: Int): Boolean

  /** Returns a MapNode with the passed key-value assignment added
   *
   *  @tparam V1 the value type of the returned node, a supertype of `V`
   *  @param key the key to add to the MapNode
   *  @param value the value to associate with `key`
   *  @param originalHash the original hash code of `key` (via `key.##`)
   *  @param hash the improved hash of `key`
   *  @param shift the shift of the node (distanceFromRoot * BitPartitionSize)
   *  @param replaceValue if true, then the value currently associated to `key` will be replaced with the passed value
   *                     argument.
   *                     if false, then the key will be inserted if not already present, however if the key is present
   *                     then the passed value will not replace the current value. That is, if `false`, then this
   *                     method has `update if not exists` semantics.
   *  @return a new `MapNode` containing the updated key-value mapping
   */
  def updated[V1 >: V](key: K, value: V1, originalHash: Int, hash: Int, shift: Int, replaceValue: Boolean): MapNode[K, V1]

  /** Returns a node with the binding for `key` removed, or this node unchanged if `key` is not
   *  bound. The result is in canonical form: a node left with a single entry is collapsed so
   *  the entry can be inlined as a payload of an ancestor.
   *
   *  @tparam V1 the value type of the returned node, a supertype of `V`
   *  @param key the key to remove
   *  @param originalHash the original hash code of `key` (via `key.##`)
   *  @param hash the improved hash of `key`
   *  @param shift the bit-level offset into the hash code, equal to `depth * BitPartitionSize`
   *  @return a node without a binding for `key`, or this node if none was present
   */
  def removed[V1 >: V](key: K, originalHash: Int, hash: Int, shift: Int): MapNode[K, V1]

  /** Tests whether this node has at least one sub-node child. */
  def hasNodes: Boolean

  /** Returns the number of sub-node children of this node. */
  def nodeArity: Int

  /** Returns the sub-node child at the given position.
   *
   *  @param index the position among this node's sub-nodes, from `0` until `nodeArity`
   *  @return the sub-node at `index`
   */
  def getNode(index: Int): MapNode[K, V]

  /** Tests whether this node stores at least one payload entry (an inline key-value pair). */
  def hasPayload: Boolean

  /** Returns the number of payload entries (inline key-value pairs) stored directly in this
   *  node, not counting entries in sub-nodes.
   */
  def payloadArity: Int

  /** Returns the key of the payload entry at the given position.
   *
   *  @param index the position among this node's payload entries, from `0` until `payloadArity`
   *  @return the key of the payload entry at `index`
   */
  def getKey(index: Int): K

  /** Returns the value of the payload entry at the given position.
   *
   *  @param index the position among this node's payload entries, from `0` until `payloadArity`
   *  @return the value of the payload entry at `index`
   */
  def getValue(index: Int): V

  /** Returns the payload entry at the given position as a key-value tuple.
   *
   *  @param index the position among this node's payload entries, from `0` until `payloadArity`
   *  @return the `(key, value)` pair at `index`
   */
  def getPayload(index: Int): (K, V)

  /** Returns the total number of key-value pairs in the subtree rooted at this node. */
  def size: Int

  /** Applies a function to each key-value pair in the subtree rooted at this node: payload
   *  entries of this node first, then each sub-node recursively.
   *
   *  @tparam U the result type of `f`; the results are discarded
   *  @param f the function applied to each key-value pair
   */
  def foreach[U](f: ((K, V)) => U): Unit

  /** Applies a two-argument function to each key and value in the subtree rooted at this node,
   *  without allocating a tuple per entry.
   *
   *  @tparam U the result type of `f`; the results are discarded
   *  @param f the function applied to each key and its associated value
   */
  def foreachEntry[U](f: (K, V) => U): Unit

  /** Applies a function to each key, value, and original (unimproved) key hash in the subtree
   *  rooted at this node.
   *
   *  @param f the function applied to each key, value, and original hash triple
   */
  def foreachWithHash(f: (K, V, Int) => Unit): Unit

  /** Returns a node with the same keys and structure, where each value is the result of
   *  applying `f` to its key and current value. Returns this node if every result is
   *  reference-equal to the value it replaces.
   *
   *  @tparam W the value type of the returned node
   *  @param f the function applied to each key and its associated value
   *  @return a node with each value replaced by the result of `f`, or this node if nothing
   *          changed
   */
  def transform[W](f: (K, V) => W): MapNode[K, W]

  /** Returns a deep copy of this node's subtree, for use by `HashMapBuilder` when unaliasing
   *  its internal structure before further in-place mutation.
   */
  def copy(): MapNode[K, V]

  /** Returns a node containing all key-value pairs of this node and `that`, which must sit at
   *  the same position in its trie as this node does. For keys present in both, the binding
   *  from `that` wins. Unchanged subtrees are shared with the operands.
   *
   *  @tparam V1 the value type of the returned node, a supertype of `V`
   *  @param that the node to concatenate with; the "right" operand, whose bindings take
   *             precedence
   *  @param shift the bit-level offset into the hash code, equal to `depth * BitPartitionSize`
   *  @return a node containing the union of both nodes' key-value pairs
   */
  def concat[V1 >: V](that: MapNode[K, V1], shift: Int): MapNode[K, V1]

  /** Returns a node containing the key-value pairs of this subtree selected by the predicate;
   *  implements both `filter` (`isFlipped = false`) and `filterNot` (`isFlipped = true`).
   *  Returns this node if nothing is removed, and the canonical empty node if nothing remains.
   *
   *  @param pred the predicate used to test key-value pairs
   *  @param isFlipped if `false`, keeps the pairs satisfying `pred`; if `true`, keeps the pairs
   *                  not satisfying it
   *  @return a node containing the selected key-value pairs
   */
  def filterImpl(pred: ((K, V)) => Boolean, isFlipped: Boolean): MapNode[K, V]

  /** Merges this node with that node, adding each resulting tuple to `builder`
   *
   *  `this` should be a node from `left` hashmap in `left.merged(right)(mergef)`
   *
   *  @tparam V1 the value type of the merged result, a supertype of `V`
   *  @param that node from the "right" HashMap. Must also be at the same "path" or "position" within the right tree,
   *             as `this` is, within the left tree
   *  @param builder the builder used to accumulate the merged key-value pairs
   *  @param shift the bit-level offset into the hash code, equal to `depth * BitPartitionSize`
   *  @param mergef the function used to resolve collisions between keys present in both nodes
   */
  def mergeInto[V1 >: V](that: MapNode[K, V1], builder: HashMapBuilder[K, V1], shift: Int)(mergef: ((K, V), (K, V1)) => (K, V1)): Unit

  /** Returns the exact (equal by reference) key, and value, associated to a given key.
   *  If the key is not bound to a value, then an exception is thrown
   *
   *  @param key the key to look up
   *  @param originalHash the original hash code of `key` (via `key.##`)
   *  @param hash the improved hash of `key`
   *  @param shift the bit-level offset into the hash code, equal to `depth * BitPartitionSize`
   *  @return the `(key, value)` tuple bound to `key` in this node
   */
  def getTuple(key: K, originalHash: Int, hash: Int, shift: Int): (K, V)

  /** Adds all key-value pairs to a builder.
   *
   *  @tparam V1 the value type of the target builder, a supertype of `V`
   *  @param builder the builder to add the key-value pairs to
   */
  def buildTo[V1 >: V](builder: HashMapBuilder[K, V1]): Unit
}

private final class BitmapIndexedMapNode[K, +V](
  /** Bitmap with one bit set per 5-bit hash segment for which this node stores a payload entry
   *  (an inline key-value pair). Disjoint from `nodeMap`.
   */
  var dataMap: Int,
  /** Bitmap with one bit set per 5-bit hash segment for which this node stores a sub-node.
   *  Disjoint from `dataMap`.
   */
  var nodeMap: Int,
  /** The compressed storage of this node: payload entries first, as key-value slot pairs in
   *  ascending bit position order, then sub-nodes from the end of the array backwards (the
   *  sub-node at bit position index `i` is at `content.length - 1 - i`).
   */
  var content: Array[Any],
  /** The original (unimproved) `key.##` of each payload entry, in the same order as the
   *  key-value pairs in `content`.
   */
  var originalHashes: Array[Int],
  /** The total number of key-value pairs in the subtree rooted at this node. */
  var size: Int,
  /** The sum of the improved hashes of all keys in the subtree rooted at this node. Used to
   *  speed up node equality checks and to update ancestors' caches incrementally.
   */
  var cachedJavaKeySetHashCode: Int) extends MapNode[K, V] {

  releaseFence()

  import MapNode._
  import Node._

  /*
  assert(checkInvariantContentIsWellTyped())
  assert(checkInvariantSubNodesAreCompacted())

  private final def checkInvariantSubNodesAreCompacted(): Boolean =
    new MapKeyValueTupleIterator[K, V](this).size - payloadArity >= 2 * nodeArity

  private final def checkInvariantContentIsWellTyped(): Boolean = {
    val predicate1 = TupleLength * payloadArity + nodeArity == content.length

    val predicate2 = Range(0, TupleLength * payloadArity)
      .forall(i => content(i).isInstanceOf[MapNode[_, _]] == false)

    val predicate3 = Range(TupleLength * payloadArity, content.length)
      .forall(i => content(i).isInstanceOf[MapNode[_, _]] == true)

    predicate1 && predicate2 && predicate3
  }
  */

  /** Returns the key of the payload entry at the given position, read from the key slot in
   *  `content`.
   *
   *  @param index the position among this node's payload entries, from `0` until `payloadArity`
   *  @return the key of the payload entry at `index`
   */
  def getKey(index: Int): K = content(TupleLength * index).asInstanceOf[K]
  /** Returns the value of the payload entry at the given position, read from the value slot in
   *  `content`.
   *
   *  @param index the position among this node's payload entries, from `0` until `payloadArity`
   *  @return the value of the payload entry at `index`
   */
  def getValue(index: Int): V = content(TupleLength * index + 1).asInstanceOf[V]

  /** Returns the payload entry at the given position as a freshly allocated key-value tuple.
   *
   *  @param index the position among this node's payload entries, from `0` until `payloadArity`
   */
  def getPayload(index: Int) = Tuple2(
    content(TupleLength * index).asInstanceOf[K],
    content(TupleLength * index + 1).asInstanceOf[V])

  /** Returns the original (unimproved) hash code of the key of the payload entry at the given
   *  position.
   *
   *  @param index the position among this node's payload entries, from `0` until `payloadArity`
   *  @return the cached `key.##` of the payload entry at `index`
   */
  override def getHash(index: Int): Int = originalHashes(index)

  /** Returns the sub-node at the given position, read from the back of the `content` array.
   *
   *  @param index the position among this node's sub-nodes, from `0` until `nodeArity`
   *  @return the sub-node at `index`
   */
  def getNode(index: Int): MapNode[K, V] =
    content(content.length - 1 - index).asInstanceOf[MapNode[K, V]]

  /** Returns the value associated with `key` in the subtree rooted at this node, by inspecting
   *  the 5-bit segment of the improved hash at `shift`: a matching `dataMap` bit selects a
   *  payload entry to compare against, a matching `nodeMap` bit recurses into the sub-node.
   *
   *  @param key the key to look up
   *  @param originalHash the original hash code of `key` (via `key.##`); never used, as the
   *                     payload comparison relies on key equality alone
   *  @param keyHash the improved hash of `key`, used to navigate the trie
   *  @param shift the bit-level offset into the hash code, equal to `depth * BitPartitionSize`
   *  @return the value bound to `key`
   *  @throws NoSuchElementException if `key` is not bound in this subtree
   */
  def apply(key: K, originalHash: Int, keyHash: Int, shift: Int): V = {
    val mask = maskFrom(keyHash, shift)
    val bitpos = bitposFrom(mask)

    if ((dataMap & bitpos) != 0) {
      val index = indexFrom(dataMap, mask, bitpos)
      if (key == getKey(index)) getValue(index) else throw new NoSuchElementException(s"key not found: $key")
    } else if ((nodeMap & bitpos) != 0) {
      getNode(indexFrom(nodeMap, mask, bitpos)).apply(key, originalHash, keyHash, shift + BitPartitionSize)
    } else {
      throw new NoSuchElementException(s"key not found: $key")
    }
  }

  /** Optionally returns the value associated with `key` in the subtree rooted at this node,
   *  navigating by the 5-bit segment of the improved hash at `shift`.
   *
   *  @param key the key to look up
   *  @param originalHash the original hash code of `key` (via `key.##`); never used, as the
   *                     payload comparison relies on key equality alone
   *  @param keyHash the improved hash of `key`, used to navigate the trie
   *  @param shift the bit-level offset into the hash code, equal to `depth * BitPartitionSize`
   *  @return `Some(value)` if `key` is bound in this subtree, `None` otherwise
   */
  def get(key: K, originalHash: Int, keyHash: Int, shift: Int): Option[V] = {
    val mask = maskFrom(keyHash, shift)
    val bitpos = bitposFrom(mask)

    if ((dataMap & bitpos) != 0) {
      val index = indexFrom(dataMap, mask, bitpos)
      val key0 = this.getKey(index)
      if (key == key0) Some(this.getValue(index)) else None
    } else if ((nodeMap & bitpos) != 0) {
      val index = indexFrom(nodeMap, mask, bitpos)
      this.getNode(index).get(key, originalHash, keyHash, shift + BitPartitionSize)
    } else {
      None
    }
  }

  /** Returns the stored `(key, value)` tuple for `key`, containing the exact (reference-equal)
   *  key instance held in the trie.
   *
   *  @param key the key to look up
   *  @param originalHash the original hash code of `key` (via `key.##`); never used, as the
   *                     payload comparison relies on key equality alone
   *  @param hash the improved hash of `key`, used to navigate the trie
   *  @param shift the bit-level offset into the hash code, equal to `depth * BitPartitionSize`
   *  @return the `(key, value)` tuple bound to `key` in this subtree
   *  @throws NoSuchElementException if `key` is not bound in this subtree
   */
  override def getTuple(key: K, originalHash: Int, hash: Int, shift: Int): (K, V) = {
    val mask = maskFrom(hash, shift)
    val bitpos = bitposFrom(mask)

    if ((dataMap & bitpos) != 0) {
      val index = indexFrom(dataMap, mask, bitpos)
      val payload = getPayload(index)
      if (key == payload._1) payload else Iterator.empty.next()
    } else if ((nodeMap & bitpos) != 0) {
      val index = indexFrom(nodeMap, mask, bitpos)
      getNode(index).getTuple(key, originalHash, hash, shift + BitPartitionSize)
    } else {
      Iterator.empty.next()
    }
  }

  /** Returns the value associated with `key` in the subtree rooted at this node, or the default
   *  value if `key` is not bound.
   *
   *  @tparam V1 the result type, a supertype of `V`
   *  @param key the key to look up
   *  @param originalHash the original hash code of `key` (via `key.##`); never used, as the
   *                     payload comparison relies on key equality alone
   *  @param keyHash the improved hash of `key`, used to navigate the trie
   *  @param shift the bit-level offset into the hash code, equal to `depth * BitPartitionSize`
   *  @param f a computation that yields the default value; only evaluated if `key` is not bound
   *  @return the value bound to `key`, or `f` if `key` is not bound
   */
  def getOrElse[V1 >: V](key: K, originalHash: Int, keyHash: Int, shift: Int, f: => V1): V1 = {
    val mask = maskFrom(keyHash, shift)
    val bitpos = bitposFrom(mask)

    if ((dataMap & bitpos) != 0) {
      val index = indexFrom(dataMap, mask, bitpos)
      val key0 = this.getKey(index)
      if (key == key0) getValue(index) else f
    } else if ((nodeMap & bitpos) != 0) {
      val index = indexFrom(nodeMap, mask, bitpos)
      this.getNode(index).getOrElse(key, originalHash, keyHash, shift + BitPartitionSize, f)
    } else {
      f
    }
  }

  /** Tests whether `key` is bound in the subtree rooted at this node. A payload candidate is
   *  compared first by its cached original hash and only then by key equality, so a mismatch is
   *  usually detected without an equality check.
   *
   *  @param key the key to look up
   *  @param originalHash the original hash code of `key` (via `key.##`), compared against the
   *                     cached hashes of payload entries
   *  @param keyHash the improved hash of `key`, used to navigate the trie
   *  @param shift the bit-level offset into the hash code, equal to `depth * BitPartitionSize`
   *  @return `true` if `key` is bound in this subtree, `false` otherwise
   */
  override def containsKey(key: K, originalHash: Int, keyHash: Int, shift: Int): Boolean = {
    val mask = maskFrom(keyHash, shift)
    val bitpos = bitposFrom(mask)

    if ((dataMap & bitpos) != 0) {
      val index = indexFrom(dataMap, mask, bitpos)
      // assert(hashes(index) == computeHash(this.getKey(index)), (hashes.toSeq, content.toSeq, index, key, keyHash, shift))
      (originalHashes(index) == originalHash) && key == getKey(index)
    } else if ((nodeMap & bitpos) != 0) {
      getNode(indexFrom(nodeMap, mask, bitpos)).containsKey(key, originalHash, keyHash, shift + BitPartitionSize)
    } else {
      false
    }
  }


  /** Returns a node with `key` bound to `value` in the subtree rooted at this node. If the
   *  targeted payload slot holds a different key, both pairs are pushed down into a new
   *  sub-node (`mergeTwoKeyValPairs`); if the slot is empty, the pair is inserted inline.
   *  Returns this node unchanged if `key` is already bound to a reference-equal key and value,
   *  or if `key` is present and `replaceValue` is `false`.
   *
   *  @tparam V1 the value type of the returned node, a supertype of `V`
   *  @param key the key to add or update
   *  @param value the value to associate with `key`
   *  @param originalHash the original hash code of `key` (via `key.##`)
   *  @param keyHash the improved hash of `key`, used to navigate the trie
   *  @param shift the bit-level offset into the hash code, equal to `depth * BitPartitionSize`
   *  @param replaceValue if `true`, an existing binding for `key` is replaced; if `false`, the
   *                     pair is only inserted when `key` is absent
   *  @return a node containing the binding, or this node if nothing changed
   */
  def updated[V1 >: V](key: K, value: V1, originalHash: Int, keyHash: Int, shift: Int, replaceValue: Boolean): BitmapIndexedMapNode[K, V1] = {
    val mask = maskFrom(keyHash, shift)
    val bitpos = bitposFrom(mask)

    if ((dataMap & bitpos) != 0) {
      val index = indexFrom(dataMap, mask, bitpos)
      val key0 = getKey(index)
      val key0UnimprovedHash = getHash(index)
      if (key0UnimprovedHash == originalHash && key0 == key) {
        if (replaceValue) {
          val value0 = this.getValue(index)
          if ((key0.asInstanceOf[AnyRef] eq key.asInstanceOf[AnyRef]) && (value0.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]))
            this
          else copyAndSetValue(bitpos, key, value)
        } else this
      } else {
        val value0 = this.getValue(index)
        val key0Hash = improve(key0UnimprovedHash)
        val subNodeNew = mergeTwoKeyValPairs(key0, value0, key0UnimprovedHash, key0Hash, key, value, originalHash, keyHash, shift + BitPartitionSize)

        copyAndMigrateFromInlineToNode(bitpos, key0Hash, subNodeNew)
      }
    } else if ((nodeMap & bitpos) != 0) {
      val index = indexFrom(nodeMap, mask, bitpos)
      val subNode = this.getNode(index)
      val subNodeNew = subNode.updated(key, value, originalHash, keyHash, shift + BitPartitionSize, replaceValue)

      if (subNodeNew eq subNode) this else copyAndSetNode(bitpos, subNode, subNodeNew)
    } else copyAndInsertValue(bitpos, key, originalHash, keyHash, value)
  }

  /** A variant of `updated` which performs shallow mutations on the root (`this`), and if possible, on immediately
   *  descendant child nodes (only one level beneath `this`)
   *
   *  The caller should pass a bitmap of child nodes of this node, which this method may mutate.
   *  If this method may mutate a child node, then if the updated key-value belongs in that child node, it will
   *  be shallowly mutated (its children will not be mutated).
   *
   *  If instead this method may not mutate the child node in which the to-be-updated key-value pair belongs, then
   *  that child will be updated immutably, but the result will be mutably re-inserted as a child of this node.
   *
   *  @tparam V1 the value type of the updated node, a supertype of `V`
   *  @param key the key to update
   *  @param value the value to set `key` to
   *  @param originalHash key.##
   *  @param keyHash the improved hash
   *
   *  @param shallowlyMutableNodeMap bitmap of child nodes of this node, which can be shallowly mutated
   *                                during the call to this method
   *  @param shift the bit-level offset into the hash code, equal to `depth * BitPartitionSize`
   *  @return Int which is the bitwise OR of shallowlyMutableNodeMap and any freshly created nodes, which will be
   *         available for mutations in subsequent calls.
   */
  def updateWithShallowMutations[V1 >: V](key: K, value: V1, originalHash: Int, keyHash: Int, shift: Int, shallowlyMutableNodeMap: Int): Int = {
    val mask = maskFrom(keyHash, shift)
    val bitpos = bitposFrom(mask)

    if ((dataMap & bitpos) != 0) {
      val index = indexFrom(dataMap, mask, bitpos)
      val key0 = getKey(index)
      val key0UnimprovedHash = getHash(index)
      if (key0UnimprovedHash == originalHash && key0 == key) {
        val value0 = this.getValue(index)
        if (!((key0.asInstanceOf[AnyRef] eq key.asInstanceOf[AnyRef]) && (value0.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]))) {
          val dataIx = dataIndex(bitpos)
          val idx = TupleLength * dataIx
          content(idx + 1) = value
        }
        shallowlyMutableNodeMap
      } else {
        val value0 = this.getValue(index)
        val key0Hash = improve(key0UnimprovedHash)

        val subNodeNew = mergeTwoKeyValPairs(key0, value0, key0UnimprovedHash, key0Hash, key, value, originalHash, keyHash, shift + BitPartitionSize)
        migrateFromInlineToNodeInPlace(bitpos, key0Hash, subNodeNew)
        shallowlyMutableNodeMap | bitpos
      }
    } else if ((nodeMap & bitpos) != 0) {
      val index = indexFrom(nodeMap, mask, bitpos)
      val subNode = this.getNode(index)
      val subNodeSize = subNode.size
      val subNodeHashCode = subNode.cachedJavaKeySetHashCode

      var returnMutableNodeMap = shallowlyMutableNodeMap

      val subNodeNew: MapNode[K, V1] = subNode match {
        case subNodeBm: BitmapIndexedMapNode[K, V] if (bitpos & shallowlyMutableNodeMap) != 0 =>
          subNodeBm.updateWithShallowMutations(key, value, originalHash, keyHash, shift + BitPartitionSize, 0)
          subNodeBm
        case _ =>
          val result = subNode.updated(key, value, originalHash, keyHash, shift + BitPartitionSize, replaceValue = true)
          if (result ne subNode) {
            returnMutableNodeMap |= bitpos
          }
          result
      }

      this.content(this.content.length - 1 - this.nodeIndex(bitpos)) = subNodeNew
      this.size = this.size - subNodeSize + subNodeNew.size
      this.cachedJavaKeySetHashCode = this.cachedJavaKeySetHashCode - subNodeHashCode + subNodeNew.cachedJavaKeySetHashCode
      returnMutableNodeMap
    } else {
      val dataIx = dataIndex(bitpos)
      val idx = TupleLength * dataIx

      val src = this.content
      val dst = new Array[Any](src.length + TupleLength)

      // copy 'src' and insert 2 element(s) at position 'idx'
      arraycopy(src, 0, dst, 0, idx)
      dst(idx) = key
      dst(idx + 1) = value
      arraycopy(src, idx, dst, idx + TupleLength, src.length - idx)

      this.dataMap |= bitpos
      this.content = dst
      this.originalHashes = insertElement(originalHashes, dataIx, originalHash)
      this.size += 1
      this.cachedJavaKeySetHashCode += keyHash
      shallowlyMutableNodeMap
    }
  }

  /** Returns a node with the binding for `key` removed, or this node unchanged if `key` is not
   *  bound. Maintains the canonical form: a non-root node that would be left with exactly one
   *  remaining payload entry is rebuilt so it can be inlined into its parent, and a sub-node
   *  reduced to a single entry is either returned directly (when it was the only child) or
   *  migrated back to an inline payload entry of this node.
   *
   *  @tparam V1 the value type of the returned node, a supertype of `V`
   *  @param key the key to remove
   *  @param originalHash the original hash code of `key` (via `key.##`)
   *  @param keyHash the improved hash of `key`, used to navigate the trie
   *  @param shift the bit-level offset into the hash code, equal to `depth * BitPartitionSize`
   *  @return a node without a binding for `key`, or this node if none was present
   */
  def removed[V1 >: V](key: K, originalHash: Int, keyHash: Int, shift: Int): BitmapIndexedMapNode[K, V1] = {
    val mask = maskFrom(keyHash, shift)
    val bitpos = bitposFrom(mask)

    if ((dataMap & bitpos) != 0) {
      val index = indexFrom(dataMap, mask, bitpos)
      val key0 = this.getKey(index)

      if (key0 == key) {
        if (this.payloadArity == 2 && this.nodeArity == 0) {
          /*
           * Creates new node with remaining pair. The new node will a) either become the new root
           * returned, or b) unwrapped and inlined during returning.
           */
          val newDataMap = if (shift == 0) (dataMap ^ bitpos) else bitposFrom(maskFrom(keyHash, 0))
          if (index == 0)
            new BitmapIndexedMapNode[K, V1](newDataMap, 0, Array(getKey(1), getValue(1)), Array(originalHashes(1)), 1, improve(getHash(1)))
          else
            new BitmapIndexedMapNode[K, V1](newDataMap, 0, Array(getKey(0), getValue(0)), Array(originalHashes(0)), 1, improve(getHash(0)))
        } else copyAndRemoveValue(bitpos, keyHash)
      } else this
    } else if ((nodeMap & bitpos) != 0) {
      val index = indexFrom(nodeMap, mask, bitpos)
      val subNode = this.getNode(index)

      val subNodeNew = subNode.removed(key, originalHash, keyHash, shift + BitPartitionSize)
      // assert(subNodeNew.size != 0, "Sub-node must have at least one element.")

      if (subNodeNew eq subNode) return this

      // cache just in case subNodeNew is a hashCollision node, in which in which case a little arithmetic is avoided
      // in Vector#length
      val subNodeNewSize = subNodeNew.size

      if (subNodeNewSize == 1) {
        if (this.size == subNode.size) {
          // subNode is the only child (no other data or node children of `this` exist)
          // escalate (singleton or empty) result
          subNodeNew.asInstanceOf[BitmapIndexedMapNode[K, V]]
        } else {
          // inline value (move to front)
          copyAndMigrateFromNodeToInline(bitpos, subNode, subNodeNew)
        }
      } else if (subNodeNewSize > 1) {
        // modify current node (set replacement node)
        copyAndSetNode(bitpos, subNode, subNodeNew)
      } else this
    } else this
  }

  /** Returns a node holding two distinct keys whose improved hashes collide in every 5-bit
   *  segment above `shift`. If the hashes differ in the segment at `shift`, both pairs become
   *  payload entries of one new node; if they still collide, a single-child node chain is built
   *  recursively until they diverge, or until the hash bits are exhausted
   *  (`shift >= HashCodeLength`), in which case a `HashCollisionMapNode` is created.
   *
   *  @tparam V1 the value type of the returned node, a supertype of `V`
   *  @param key0 the first key; must not equal `key1`
   *  @param value0 the value associated with `key0`
   *  @param originalHash0 the original hash code of `key0` (via `key0.##`)
   *  @param keyHash0 the improved hash of `key0`
   *  @param key1 the second key
   *  @param value1 the value associated with `key1`
   *  @param originalHash1 the original hash code of `key1` (via `key1.##`)
   *  @param keyHash1 the improved hash of `key1`
   *  @param shift the bit-level offset at which the new node will sit, equal to
   *              `depth * BitPartitionSize`
   *  @return a node containing exactly the two key-value pairs
   */
  def mergeTwoKeyValPairs[V1 >: V](key0: K, value0: V1, originalHash0: Int, keyHash0: Int, key1: K, value1: V1, originalHash1: Int, keyHash1: Int, shift: Int): MapNode[K, V1] = {
    // assert(key0 != key1)

    if (shift >= HashCodeLength) {
      new HashCollisionMapNode[K, V1](originalHash0, keyHash0, Vector((key0, value0), (key1, value1)))
    } else {
      val mask0 = maskFrom(keyHash0, shift)
      val mask1 = maskFrom(keyHash1, shift)
      val newCachedHash = keyHash0 + keyHash1

      if (mask0 != mask1) {
        // unique prefixes, payload fits on same level
        val dataMap = bitposFrom(mask0) | bitposFrom(mask1)

        if (mask0 < mask1) {
          new BitmapIndexedMapNode[K, V1](dataMap, 0, Array(key0, value0, key1, value1), Array(originalHash0, originalHash1), 2, newCachedHash)
        } else {
          new BitmapIndexedMapNode[K, V1](dataMap, 0, Array(key1, value1, key0, value0), Array(originalHash1, originalHash0), 2, newCachedHash)
        }
      } else {
        // identical prefixes, payload must be disambiguated deeper in the trie
        val nodeMap = bitposFrom(mask0)
        val node = mergeTwoKeyValPairs(key0, value0, originalHash0, keyHash0, key1, value1, originalHash1, keyHash1, shift + BitPartitionSize)
        new BitmapIndexedMapNode[K, V1](0, nodeMap, Array(node), Array.emptyIntArray, node.size, node.cachedJavaKeySetHashCode)
      }
    }
  }

  /** Tests whether this node has at least one sub-node child, i.e. whether `nodeMap` is
   *  non-empty.
   */
  def hasNodes: Boolean = nodeMap != 0

  /** Returns the number of sub-node children of this node: the number of bits set in `nodeMap`. */
  def nodeArity: Int = bitCount(nodeMap)

  /** Tests whether this node stores at least one payload entry, i.e. whether `dataMap` is
   *  non-empty.
   */
  def hasPayload: Boolean = dataMap != 0

  /** Returns the number of payload entries stored directly in this node: the number of bits set
   *  in `dataMap`.
   */
  def payloadArity: Int = bitCount(dataMap)

  /** Returns the payload index for a bit position: the number of `dataMap` bits set below
   *  `bitpos`, which is the entry's rank in the compressed payload prefix of `content`.
   *
   *  @param bitpos the bit position (a single set bit) to locate; must be set in `dataMap`
   */
  def dataIndex(bitpos: Int) = bitCount(dataMap & (bitpos - 1))

  /** Returns the sub-node index for a bit position: the number of `nodeMap` bits set below
   *  `bitpos`, which is the sub-node's rank counted from the end of `content`.
   *
   *  @param bitpos the bit position (a single set bit) to locate; must be set in `nodeMap`
   */
  def nodeIndex(bitpos: Int) = bitCount(nodeMap & (bitpos - 1))

  /** Returns a copy of this node with the value of the payload entry at `bitpos` replaced by
   *  `newValue`. Only the `content` array is copied; bitmaps, hashes, size, and the cached hash
   *  code are shared, and the stored key is kept (the `newKey` parameter is never used).
   *
   *  @tparam V1 the value type of the returned node, a supertype of `V`
   *  @param bitpos the bit position of the payload entry to update; must be set in `dataMap`
   *  @param newKey the key being updated; never used, as the existing equal key is kept
   *  @param newValue the value to store
   *  @return a copy of this node with the value at `bitpos` replaced
   */
  def copyAndSetValue[V1 >: V](bitpos: Int, newKey: K, newValue: V1): BitmapIndexedMapNode[K, V1] = {
    val dataIx = dataIndex(bitpos)
    val idx = TupleLength * dataIx

    val src = this.content
    val dst = new Array[Any](src.length)

    // copy 'src' and set 1 element(s) at position 'idx'
    arraycopy(src, 0, dst, 0, src.length)
    //dst(idx) = newKey
    dst(idx + 1) = newValue
    new BitmapIndexedMapNode[K, V1](dataMap, nodeMap, dst, originalHashes, size, cachedJavaKeySetHashCode)
  }

  /** Returns a copy of this node with the sub-node at `bitpos` replaced by `newNode`. The size
   *  and cached hash code are adjusted by the difference between the old and new sub-nodes.
   *
   *  @tparam V1 the value type of the returned node, a supertype of `V`
   *  @param bitpos the bit position of the sub-node to replace; must be set in `nodeMap`
   *  @param oldNode the sub-node currently at `bitpos`, used to compute the size and hash deltas
   *  @param newNode the sub-node to store at `bitpos`
   *  @return a copy of this node with the sub-node at `bitpos` replaced
   */
  def copyAndSetNode[V1 >: V](bitpos: Int, oldNode: MapNode[K, V1], newNode: MapNode[K, V1]): BitmapIndexedMapNode[K, V1] = {
    val idx = this.content.length - 1 - this.nodeIndex(bitpos)

    val src = this.content
    val dst = new Array[Any](src.length)

    // copy 'src' and set 1 element(s) at position 'idx'
    arraycopy(src, 0, dst, 0, src.length)
    dst(idx) = newNode
    new BitmapIndexedMapNode[K, V1](
      dataMap,
      nodeMap,
      dst,
      originalHashes,
      size - oldNode.size + newNode.size,
      cachedJavaKeySetHashCode - oldNode.cachedJavaKeySetHashCode + newNode.cachedJavaKeySetHashCode
    )
  }

  /** Returns a copy of this node with a new payload entry inserted at `bitpos`. The key-value
   *  pair is spliced into the payload prefix of `content`, the original hash into
   *  `originalHashes`, the `dataMap` bit is set, and size and cached hash code grow accordingly.
   *
   *  @tparam V1 the value type of the returned node, a supertype of `V`
   *  @param bitpos the bit position at which to insert; must be clear in both `dataMap` and
   *               `nodeMap`
   *  @param key the key to insert
   *  @param originalHash the original hash code of `key` (via `key.##`)
   *  @param keyHash the improved hash of `key`, added to the cached hash code
   *  @param value the value to associate with `key`
   *  @return a copy of this node containing the additional payload entry
   */
  def copyAndInsertValue[V1 >: V](bitpos: Int, key: K, originalHash: Int, keyHash: Int, value: V1): BitmapIndexedMapNode[K, V1] = {
    val dataIx = dataIndex(bitpos)
    val idx = TupleLength * dataIx

    val src = this.content
    val dst = new Array[Any](src.length + TupleLength)

    // copy 'src' and insert 2 element(s) at position 'idx'
    arraycopy(src, 0, dst, 0, idx)
    dst(idx) = key
    dst(idx + 1) = value
    arraycopy(src, idx, dst, idx + TupleLength, src.length - idx)

    val dstHashes = insertElement(originalHashes, dataIx, originalHash)

    new BitmapIndexedMapNode[K, V1](dataMap | bitpos, nodeMap, dst, dstHashes, size + 1, cachedJavaKeySetHashCode + keyHash)
  }

  /** Returns a copy of this node with the payload entry at `bitpos` removed: the key-value pair
   *  and its cached hash are spliced out, the `dataMap` bit is cleared, and size and cached
   *  hash code shrink accordingly.
   *
   *  @param bitpos the bit position of the payload entry to remove; must be set in `dataMap`
   *  @param keyHash the improved hash of the removed key, subtracted from the cached hash code
   *  @return a copy of this node without the payload entry at `bitpos`
   */
  def copyAndRemoveValue(bitpos: Int, keyHash: Int): BitmapIndexedMapNode[K, V] = {
    val dataIx = dataIndex(bitpos)
    val idx = TupleLength * dataIx

    val src = this.content
    val dst = new Array[Any](src.length - TupleLength)

    // copy 'src' and remove 2 element(s) at position 'idx'
    arraycopy(src, 0, dst, 0, idx)
    arraycopy(src, idx + TupleLength, dst, idx, src.length - idx - TupleLength)

    val dstHashes = removeElement(originalHashes, dataIx)

    new BitmapIndexedMapNode[K, V](dataMap ^ bitpos, nodeMap, dst, dstHashes, size - 1, cachedJavaKeySetHashCode - keyHash)
  }

  /** Variant of `copyAndMigrateFromInlineToNode` which mutates `this` rather than returning a new node.
   *
   *  @tparam V1 the value type of the child node, a supertype of `V`
   *  @param bitpos the bit position of the data to migrate to node
   *  @param keyHash the improved hash of the key currently at `bitpos`
   *  @param node the node to place at `bitpos` beneath `this`
   *  @return `this`, after mutating it so that `node` replaces the inline data at `bitpos`
   */
  def migrateFromInlineToNodeInPlace[V1 >: V](bitpos: Int, keyHash: Int, node: MapNode[K, V1]): this.type = {
    val dataIx = dataIndex(bitpos)
    val idxOld = TupleLength * dataIx
    val idxNew = this.content.length - TupleLength - nodeIndex(bitpos)

    val src = this.content
    val dst = new Array[Any](src.length - TupleLength + 1)

    // copy 'src' and remove 2 element(s) at position 'idxOld' and
    // insert 1 element(s) at position 'idxNew'
    // assert(idxOld <= idxNew)
    arraycopy(src, 0, dst, 0, idxOld)
    arraycopy(src, idxOld + TupleLength, dst, idxOld, idxNew - idxOld)
    dst(idxNew) = node
    arraycopy(src, idxNew + TupleLength, dst, idxNew + 1, src.length - idxNew - TupleLength)

    val dstHashes = removeElement(originalHashes, dataIx)

    this.dataMap = dataMap ^ bitpos
    this.nodeMap = nodeMap | bitpos
    this.content = dst
    this.originalHashes = dstHashes
    this.size = size - 1 + node.size
    this.cachedJavaKeySetHashCode = cachedJavaKeySetHashCode - keyHash + node.cachedJavaKeySetHashCode
    this
  }

  /** Returns a copy of this node in which the payload entry at `bitpos` is replaced by the
   *  sub-node `node`: the key-value pair moves out of the payload prefix, `node` is inserted in
   *  the sub-node suffix, the bit moves from `dataMap` to `nodeMap`, and size and cached hash
   *  code are adjusted for the entry removed and the sub-node added.
   *
   *  @tparam V1 the value type of the returned node, a supertype of `V`
   *  @param bitpos the bit position of the payload entry to replace; must be set in `dataMap`
   *  @param keyHash the improved hash of the key being displaced, subtracted from the cached
   *                hash code
   *  @param node the sub-node to store at `bitpos`, typically containing the displaced pair and
   *             a new one
   *  @return a copy of this node with the payload entry at `bitpos` migrated to a sub-node
   */
  def copyAndMigrateFromInlineToNode[V1 >: V](bitpos: Int, keyHash: Int, node: MapNode[K, V1]): BitmapIndexedMapNode[K, V1] = {
    val dataIx = dataIndex(bitpos)
    val idxOld = TupleLength * dataIx
    val idxNew = this.content.length - TupleLength - nodeIndex(bitpos)

    val src = this.content
    val dst = new Array[Any](src.length - TupleLength + 1)

    // copy 'src' and remove 2 element(s) at position 'idxOld' and
    // insert 1 element(s) at position 'idxNew'
    // assert(idxOld <= idxNew)
    arraycopy(src, 0, dst, 0, idxOld)
    arraycopy(src, idxOld + TupleLength, dst, idxOld, idxNew - idxOld)
    dst(idxNew) = node
    arraycopy(src, idxNew + TupleLength, dst, idxNew + 1, src.length - idxNew - TupleLength)

    val dstHashes = removeElement(originalHashes, dataIx)

    new BitmapIndexedMapNode[K, V1](
      dataMap = dataMap ^ bitpos,
      nodeMap = nodeMap | bitpos,
      content = dst,
      originalHashes = dstHashes,
      size = size - 1 + node.size,
      cachedJavaKeySetHashCode = cachedJavaKeySetHashCode - keyHash + node.cachedJavaKeySetHashCode
    )
  }

  /** Returns a copy of this node in which the sub-node at `bitpos` is replaced by the single
   *  key-value pair of `node`, inlined as a payload entry: the reverse of
   *  `copyAndMigrateFromInlineToNode`, used to restore the canonical form after a removal
   *  shrinks a sub-node to one entry. The bit moves from `nodeMap` to `dataMap`, and size and
   *  cached hash code are adjusted accordingly.
   *
   *  @tparam V1 the value type of the returned node, a supertype of `V`
   *  @param bitpos the bit position of the sub-node to replace; must be set in `nodeMap`
   *  @param oldNode the sub-node currently at `bitpos`, used to compute the size and hash deltas
   *  @param node a single-entry node whose key, value, and hash are inlined
   *  @return a copy of this node with the sub-node at `bitpos` migrated to a payload entry
   */
  def copyAndMigrateFromNodeToInline[V1 >: V](bitpos: Int, oldNode: MapNode[K, V1], node: MapNode[K, V1]): BitmapIndexedMapNode[K, V1] = {
    val idxOld = this.content.length - 1 - nodeIndex(bitpos)
    val dataIxNew = dataIndex(bitpos)
    val idxNew = TupleLength * dataIxNew

    val key = node.getKey(0)
    val value = node.getValue(0)
    val src = this.content
    val dst = new Array[Any](src.length - 1 + TupleLength)

    // copy 'src' and remove 1 element(s) at position 'idxOld' and
    // insert 2 element(s) at position 'idxNew'
    // assert(idxOld >= idxNew)
    arraycopy(src, 0, dst, 0, idxNew)
    dst(idxNew) = key
    dst(idxNew + 1) = value
    arraycopy(src, idxNew, dst, idxNew + TupleLength, idxOld - idxNew)
    arraycopy(src, idxOld + 1, dst, idxOld + TupleLength, src.length - idxOld - 1)
    val hash = node.getHash(0)
    val dstHashes = insertElement(originalHashes, dataIxNew, hash)
    new BitmapIndexedMapNode[K, V1](
      dataMap = dataMap | bitpos,
      nodeMap = nodeMap ^ bitpos,
      content = dst,
      originalHashes = dstHashes,
      size = size - oldNode.size + 1,
      cachedJavaKeySetHashCode = cachedJavaKeySetHashCode - oldNode.cachedJavaKeySetHashCode + node.cachedJavaKeySetHashCode
    )
  }

  /** Applies a function to each key-value pair in the subtree rooted at this node: this node's
   *  payload entries first, then each sub-node recursively.
   *
   *  @tparam U the result type of `f`; the results are discarded
   *  @param f the function applied to each key-value pair
   */
  override def foreach[U](f: ((K, V)) => U): Unit = {
    val iN = payloadArity // arity doesn't change during this operation
    var i = 0
    while (i < iN) {
      f(getPayload(i))
      i += 1
    }

    val jN = nodeArity // arity doesn't change during this operation
    var j = 0
    while (j < jN) {
      getNode(j).foreach(f)
      j += 1
    }
  }

  /** Applies a two-argument function to each key and value in the subtree rooted at this node,
   *  without allocating a tuple per entry.
   *
   *  @tparam U the result type of `f`; the results are discarded
   *  @param f the function applied to each key and its associated value
   */
  override def foreachEntry[U](f: (K, V) => U): Unit = {
    val iN = payloadArity // arity doesn't change during this operation
    var i = 0
    while (i < iN) {
      f(getKey(i), getValue(i))
      i += 1
    }

    val jN = nodeArity // arity doesn't change during this operation
    var j = 0
    while (j < jN) {
      getNode(j).foreachEntry(f)
      j += 1
    }
  }

  /** Applies a function to each key, value, and cached original (unimproved) key hash in the
   *  subtree rooted at this node.
   *
   *  @param f the function applied to each key, value, and original hash triple
   */
  override def foreachWithHash(f: (K, V, Int) => Unit): Unit = {
    var i = 0
    val iN = payloadArity // arity doesn't change during this operation
    while (i < iN) {
      f(getKey(i), getValue(i), getHash(i))
      i += 1
    }

    val jN = nodeArity // arity doesn't change during this operation
    var j = 0
    while (j < jN) {
      getNode(j).foreachWithHash(f)
      j += 1
    }
  }
  /** Adds all key-value pairs in the subtree rooted at this node to a builder, passing along
   *  the cached original hashes so they need not be recomputed.
   *
   *  @tparam V1 the value type of the target builder, a supertype of `V`
   *  @param builder the builder to add the key-value pairs to
   */
  override def buildTo[V1 >: V](builder: HashMapBuilder[K, V1]): Unit = {
    var i = 0
    val iN = payloadArity
    val jN = nodeArity
    while (i < iN) {
      builder.addOne(getKey(i), getValue(i), getHash(i))
      i += 1
    }

    var j = 0
    while (j < jN) {
      getNode(j).buildTo(builder)
      j += 1
    }
  }

  /** Returns a node with the same keys and structure, where each value is the result of
   *  applying `f` to its key and current value. The `content` array is cloned lazily, on the
   *  first value or sub-node that `f` actually changes (by reference); if nothing changes, this
   *  node is returned.
   *
   *  @tparam W the value type of the returned node
   *  @param f the function applied to each key and its associated value
   *  @return a node with each value replaced by the result of `f`, or this node if nothing
   *          changed
   */
  override def transform[W](f: (K, V) => W): BitmapIndexedMapNode[K, W] = {
    @annotation.stableNull var newContent: Array[Any] | Null = null
    val iN = payloadArity // arity doesn't change during this operation
    val jN = nodeArity // arity doesn't change during this operation
    val newContentLength = content.length
    var i = 0
    while (i < iN) {
      val key = getKey(i)
      val value = getValue(i)
      val newValue = f(key, value)
      if (newContent eq null) {
        if (newValue.asInstanceOf[AnyRef] ne value.asInstanceOf[AnyRef]) {
          newContent = content.clone()
          newContent(TupleLength * i + 1) = newValue
        }
      } else {
        newContent(TupleLength * i + 1) = newValue
      }
      i += 1
    }

    var j = 0
    while (j < jN) {
      val node = getNode(j)
      val newNode = node.transform(f)
      if (newContent eq null) {
        if (newNode ne node) {
          newContent = content.clone()
          newContent(newContentLength - j - 1) = newNode
        }
      } else
        newContent(newContentLength - j - 1) = newNode
      j += 1
    }
    if (newContent eq null) this.asInstanceOf[BitmapIndexedMapNode[K, W]]
    else new BitmapIndexedMapNode[K, W](dataMap, nodeMap, newContent, originalHashes, size, cachedJavaKeySetHashCode)
  }

  /** Merges this node with `that`, a node at the same position in the right-hand trie of
   *  `left.merged(right)(mergef)`, adding each resulting pair to `builder`. The two nodes'
   *  bitmaps are walked in lockstep over each bit position; pairs whose keys occur on only one
   *  side are added directly, and keys present on both sides are combined with `mergef`.
   *
   *  @tparam V1 the value type of the merged result, a supertype of `V`
   *  @param that the node from the right-hand map at the same position as this node; must be a
   *             `BitmapIndexedMapNode`
   *  @param builder the builder used to accumulate the merged key-value pairs
   *  @param shift the bit-level offset into the hash code, equal to `depth * BitPartitionSize`
   *  @param mergef the function used to combine the two pairs bound to a key present in both
   *               nodes
   *  @throws RuntimeException if `that` is a `HashCollisionMapNode`, which can never sit at the
   *                          same level as a `BitmapIndexedMapNode`
   */
  override def mergeInto[V1 >: V](that: MapNode[K, V1], builder: HashMapBuilder[K, V1], shift: Int)(mergef: ((K, V), (K, V1)) => (K, V1)): Unit = that match {
    case bm: BitmapIndexedMapNode[K, V] @unchecked =>
      if (size == 0) {
        that.buildTo(builder)
        return
      } else if (bm.size == 0) {
        buildTo(builder)
        return
      }

      val allMap = dataMap | bm.dataMap | nodeMap | bm.nodeMap

      val minIndex: Int = Integer.numberOfTrailingZeros(allMap)
      val maxIndex: Int = Node.BranchingFactor - Integer.numberOfLeadingZeros(allMap)

      {
        var index = minIndex
        var leftIdx = 0
        var rightIdx = 0

        while (index < maxIndex) {
          val bitpos = bitposFrom(index)

          if ((bitpos & dataMap) != 0) {
            val leftKey = getKey(leftIdx)
            val leftValue = getValue(leftIdx)
            val leftOriginalHash = getHash(leftIdx)
            if ((bitpos & bm.dataMap) != 0) {
              // left data and right data
              val rightKey = bm.getKey(rightIdx)
              val rightValue = bm.getValue(rightIdx)
              val rightOriginalHash = bm.getHash(rightIdx)
              if (leftOriginalHash == rightOriginalHash && leftKey == rightKey) {
                builder.addOne(mergef((leftKey, leftValue), (rightKey, rightValue)))
              } else {
                builder.addOne(leftKey, leftValue, leftOriginalHash)
                builder.addOne(rightKey, rightValue, rightOriginalHash)
              }
              rightIdx += 1
            } else if ((bitpos & bm.nodeMap) != 0) {
              // left data and right node
              val subNode = bm.getNode(bm.nodeIndex(bitpos))
              val leftImprovedHash = improve(leftOriginalHash)
              val removed = subNode.removed(leftKey, leftOriginalHash, leftImprovedHash, shift + BitPartitionSize)
              if (removed eq subNode) {
                // no overlap in leftData and rightNode, just build both children to builder
                subNode.buildTo(builder)
                builder.addOne(leftKey, leftValue, leftOriginalHash, leftImprovedHash)
              } else {
                // there is collision, so special treatment for that key
                removed.buildTo(builder)
                builder.addOne(mergef((leftKey, leftValue), subNode.getTuple(leftKey, leftOriginalHash, leftImprovedHash, shift + BitPartitionSize)))
              }
            } else {
              // left data and nothing on right
              builder.addOne(leftKey, leftValue, leftOriginalHash)
            }
            leftIdx += 1
          } else if ((bitpos & nodeMap) != 0) {
            if ((bitpos & bm.dataMap) != 0) {
              // left node and right data
              val rightKey = bm.getKey(rightIdx)
              val rightValue = bm.getValue(rightIdx)
              val rightOriginalHash = bm.getHash(rightIdx)
              val rightImprovedHash = improve(rightOriginalHash)

              val subNode = getNode(nodeIndex(bitpos))
              val removed = subNode.removed(rightKey, rightOriginalHash, rightImprovedHash, shift + BitPartitionSize)
              if (removed eq subNode) {
                // no overlap in leftNode and rightData, just build both children to builder
                subNode.buildTo(builder)
                builder.addOne(rightKey, rightValue, rightOriginalHash, rightImprovedHash)
              } else {
                // there is collision, so special treatment for that key
                removed.buildTo(builder)
                builder.addOne(mergef(subNode.getTuple(rightKey, rightOriginalHash, rightImprovedHash, shift + BitPartitionSize), (rightKey, rightValue)))
              }
              rightIdx += 1

            } else if ((bitpos & bm.nodeMap) != 0) {
              // left node and right node
              getNode(nodeIndex(bitpos)).mergeInto(bm.getNode(bm.nodeIndex(bitpos)), builder, shift + BitPartitionSize)(mergef)
            } else {
              // left node and nothing on right
              getNode(nodeIndex(bitpos)).buildTo(builder)
            }
          } else if ((bitpos & bm.dataMap) != 0) {
            // nothing on left, right data
            val dataIndex = bm.dataIndex(bitpos)
            builder.addOne(bm.getKey(dataIndex),bm.getValue(dataIndex), bm.getHash(dataIndex))
            rightIdx += 1

          } else if ((bitpos & bm.nodeMap) != 0) {
            // nothing on left, right node
            bm.getNode(bm.nodeIndex(bitpos)).buildTo(builder)
          }

          index += 1
        }
      }
    case _: HashCollisionMapNode[?, ?] =>
      throw new RuntimeException("Cannot merge BitmapIndexedMapNode with HashCollisionMapNode")
  }

  /** Tests whether this node is equal to another object. Two `BitmapIndexedMapNode`s are equal
   *  when their cached key-set hash codes, bitmaps, sizes, and hash arrays match and their
   *  `content` arrays are element-wise equal, comparing sub-nodes recursively. Because equal
   *  maps have identical canonical trie structure, this decides map equality; the cheap scalar
   *  comparisons come first so unequal nodes are usually rejected without touching `content`.
   *
   *  @param that the object to compare with
   *  @return `true` if `that` is a `BitmapIndexedMapNode` with equal structure and contents
   */
  override def equals(that: Any): Boolean =
    that match {
      case node: BitmapIndexedMapNode[?, ?] =>
        (this eq node) ||
          (this.cachedJavaKeySetHashCode == node.cachedJavaKeySetHashCode) &&
          (this.nodeMap == node.nodeMap) &&
            (this.dataMap == node.dataMap) &&
              (this.size == node.size) &&
                java.util.Arrays.equals(this.originalHashes, node.originalHashes) &&
                  deepContentEquality(this.content, node.content, content.length)
      case _ => false
    }

  @inline private def deepContentEquality(a1: Array[Any], a2: Array[Any], length: Int): Boolean = {
    if (a1 eq a2)
      true
    else {
      var isEqual = true
      var i = 0

      while (isEqual && i < length) {
        isEqual = a1(i) == a2(i)
        i += 1
      }

      isEqual
    }
  }

  /** Always throws: trie nodes define `equals` for structural comparison but are never used as
   *  hash keys themselves.
   *
   *  @throws UnsupportedOperationException always
   */
  override def hashCode(): Int =
    throw new UnsupportedOperationException("Trie nodes do not support hashing.")

  /** Returns the default identity-based representation (class name and identity hash code);
   *  contents are deliberately not printed.
   */
  override def toString() = s"${getClass.getName}@${Integer.toHexString(System.identityHashCode(this))}"

  /** Returns a node containing all key-value pairs of this node and `that`, with `that`
   *  winning on keys present in both. Runs in two passes over the union of the four bitmaps:
   *  the first classifies every bit position (data/data, data/node, node/node, one side only,
   *  equal-key overwrite, or two distinct colliding keys to push into a new sub-node) and
   *  computes the result bitmaps; the second fills the result arrays, recursing where both
   *  sides have sub-nodes. Returns `that` unchanged when nothing of this node survives or no
   *  difference from `that` is produced, and this node when `that` is empty or `eq` this.
   *
   *  @tparam V1 the value type of the returned node, a supertype of `V`
   *  @param that the node to concatenate with, at the same position in its trie as this node;
   *             its bindings take precedence
   *  @param shift the bit-level offset into the hash code, equal to `depth * BitPartitionSize`
   *  @return a node containing the union of both nodes' key-value pairs
   *  @throws UnsupportedOperationException if `that` is a `HashCollisionMapNode`, which can
   *                                       never sit at the same level as a
   *                                       `BitmapIndexedMapNode`
   */
  override def concat[V1 >: V](that: MapNode[K, V1], shift: Int): BitmapIndexedMapNode[K, V1] = that match {
    case bm: BitmapIndexedMapNode[K, V] @unchecked =>
      if (size == 0) return bm
      else if (bm.size == 0 || (bm eq this)) return this
      else if (bm.size == 1) {
        val originalHash = bm.getHash(0)
        return this.updated(bm.getKey(0), bm.getValue(0), originalHash, improve(originalHash), shift, replaceValue = true)
      }
      // if we go through the merge and the result does not differ from `bm`, we can just return `bm`, to improve sharing
      // So, `anyChangesMadeSoFar` will be set to `true` as soon as we encounter a difference between the
      // currently-being-computed result, and `bm`
      var anyChangesMadeSoFar = false

      val allMap = dataMap | bm.dataMap | nodeMap | bm.nodeMap

      // minimumIndex is inclusive -- it is the first index for which there is data or nodes
      val minimumBitPos: Int = Node.bitposFrom(Integer.numberOfTrailingZeros(allMap))
      // maximumIndex is inclusive -- it is the last index for which there is data or nodes
      // it could not be exclusive, because then upper bound in worst case (Node.BranchingFactor) would be out-of-bound
      // of int bitposition representation
      val maximumBitPos: Int = Node.bitposFrom(Node.BranchingFactor - Integer.numberOfLeadingZeros(allMap) - 1)

      var leftNodeRightNode = 0
      var leftDataRightNode = 0
      var leftNodeRightData = 0
      var leftDataOnly = 0
      var rightDataOnly = 0
      var leftNodeOnly = 0
      var rightNodeOnly = 0
      var leftDataRightDataMigrateToNode = 0
      var leftDataRightDataRightOverwrites = 0

      var dataToNodeMigrationTargets = 0

      {
        var bitpos = minimumBitPos
        var leftIdx = 0
        var rightIdx = 0
        var finished = false

        while (!finished) {

          if ((bitpos & dataMap) != 0) {
            if ((bitpos & bm.dataMap) != 0) {
              val leftOriginalHash = getHash(leftIdx)
              if (leftOriginalHash == bm.getHash(rightIdx) && getKey(leftIdx) == bm.getKey(rightIdx)) {
                leftDataRightDataRightOverwrites |= bitpos
              } else {
                leftDataRightDataMigrateToNode |= bitpos
                dataToNodeMigrationTargets |= Node.bitposFrom(Node.maskFrom(improve(leftOriginalHash), shift))
              }
              rightIdx += 1
            } else if ((bitpos & bm.nodeMap) != 0) {
              leftDataRightNode |= bitpos
            } else {
              leftDataOnly |= bitpos
            }
            leftIdx += 1
          } else if ((bitpos & nodeMap) != 0) {
            if ((bitpos & bm.dataMap) != 0) {
              leftNodeRightData |= bitpos
              rightIdx += 1
            } else if ((bitpos & bm.nodeMap) != 0) {
              leftNodeRightNode |= bitpos
            } else {
              leftNodeOnly |= bitpos
            }
          } else if ((bitpos & bm.dataMap) != 0) {
            rightDataOnly |= bitpos
            rightIdx += 1
          } else if ((bitpos & bm.nodeMap) != 0) {
            rightNodeOnly |= bitpos
          }

          if (bitpos == maximumBitPos) {
            finished = true
          } else {
            bitpos = bitpos << 1
          }
        }
      }


      val newDataMap = leftDataOnly | rightDataOnly | leftDataRightDataRightOverwrites

      val newNodeMap =
        leftNodeRightNode |
          leftDataRightNode |
          leftNodeRightData |
          leftNodeOnly |
          rightNodeOnly |
          dataToNodeMigrationTargets


      if ((newDataMap == (rightDataOnly | leftDataRightDataRightOverwrites)) && (newNodeMap == rightNodeOnly)) {
        // nothing from `this` will make it into the result -- return early
        return bm
      }

      val newDataSize = bitCount(newDataMap)
      val newContentSize = (MapNode.TupleLength * newDataSize) + bitCount(newNodeMap)

      val newContent = new Array[Any](newContentSize)
      val newOriginalHashes = new Array[Int](newDataSize)
      var newSize = 0
      var newCachedHashCode = 0

      {
        var leftDataIdx = 0
        var rightDataIdx = 0
        var leftNodeIdx = 0
        var rightNodeIdx = 0

        val nextShift = shift + Node.BitPartitionSize

        var compressedDataIdx = 0
        var compressedNodeIdx = 0

        var bitpos = minimumBitPos
        var finished = false

        while (!finished) {

          if ((bitpos & leftNodeRightNode) != 0) {
            val rightNode = bm.getNode(rightNodeIdx)
            val newNode = getNode(leftNodeIdx).concat(rightNode, nextShift)
            if (rightNode ne newNode) {
              anyChangesMadeSoFar = true
            }
            newContent(newContentSize - compressedNodeIdx - 1) = newNode
            compressedNodeIdx += 1
            rightNodeIdx += 1
            leftNodeIdx += 1
            newSize += newNode.size
            newCachedHashCode += newNode.cachedJavaKeySetHashCode

          } else if ((bitpos & leftDataRightNode) != 0) {
            val newNode = {
              val n = bm.getNode(rightNodeIdx)
              val leftKey = getKey(leftDataIdx)
              val leftValue = getValue(leftDataIdx)
              val leftOriginalHash = getHash(leftDataIdx)
              val leftImproved = improve(leftOriginalHash)

              val updated = n.updated(leftKey, leftValue, leftOriginalHash, leftImproved, nextShift, replaceValue = false)

              if (updated ne n) {
                anyChangesMadeSoFar = true
              }

              updated
            }

            newContent(newContentSize - compressedNodeIdx - 1) = newNode
            compressedNodeIdx += 1
            rightNodeIdx += 1
            leftDataIdx += 1
            newSize += newNode.size
            newCachedHashCode += newNode.cachedJavaKeySetHashCode
          }
          else if ((bitpos & leftNodeRightData) != 0) {
            anyChangesMadeSoFar = true
            val newNode = {
              val rightOriginalHash = bm.getHash(rightDataIdx)
              getNode(leftNodeIdx).updated(
                key = bm.getKey(rightDataIdx),
                value = bm.getValue(rightDataIdx),
                originalHash = rightOriginalHash,
                hash = improve(rightOriginalHash),
                shift = nextShift,
                replaceValue = true
              )
            }

            newContent(newContentSize - compressedNodeIdx - 1) = newNode
            compressedNodeIdx += 1
            leftNodeIdx += 1
            rightDataIdx += 1
            newSize += newNode.size
            newCachedHashCode += newNode.cachedJavaKeySetHashCode

          } else if ((bitpos & leftDataOnly) != 0) {
            anyChangesMadeSoFar = true
            val originalHash = originalHashes(leftDataIdx)
            newContent(MapNode.TupleLength * compressedDataIdx) = getKey(leftDataIdx).asInstanceOf[AnyRef]
            newContent(MapNode.TupleLength * compressedDataIdx + 1) = getValue(leftDataIdx).asInstanceOf[AnyRef]
            newOriginalHashes(compressedDataIdx) = originalHash

            compressedDataIdx += 1
            leftDataIdx += 1
            newSize += 1
            newCachedHashCode += improve(originalHash)
          } else if ((bitpos & rightDataOnly) != 0) {
            val originalHash = bm.originalHashes(rightDataIdx)
            newContent(MapNode.TupleLength * compressedDataIdx) = bm.getKey(rightDataIdx).asInstanceOf[AnyRef]
            newContent(MapNode.TupleLength * compressedDataIdx + 1) = bm.getValue(rightDataIdx).asInstanceOf[AnyRef]
            newOriginalHashes(compressedDataIdx) = originalHash

            compressedDataIdx += 1
            rightDataIdx += 1
            newSize += 1
            newCachedHashCode += improve(originalHash)
          } else if ((bitpos & leftNodeOnly) != 0) {
            anyChangesMadeSoFar = true
            val newNode = getNode(leftNodeIdx)
            newContent(newContentSize - compressedNodeIdx - 1) = newNode
            compressedNodeIdx += 1
            leftNodeIdx += 1
            newSize += newNode.size
            newCachedHashCode += newNode.cachedJavaKeySetHashCode
          } else if ((bitpos & rightNodeOnly) != 0) {
            val newNode = bm.getNode(rightNodeIdx)
            newContent(newContentSize - compressedNodeIdx - 1) = newNode
            compressedNodeIdx += 1
            rightNodeIdx += 1
            newSize += newNode.size
            newCachedHashCode += newNode.cachedJavaKeySetHashCode
          } else if ((bitpos & leftDataRightDataMigrateToNode) != 0) {
            anyChangesMadeSoFar = true
            val newNode = {
              val leftOriginalHash = getHash(leftDataIdx)
              val rightOriginalHash = bm.getHash(rightDataIdx)

              bm.mergeTwoKeyValPairs(
                getKey(leftDataIdx), getValue(leftDataIdx), leftOriginalHash, improve(leftOriginalHash),
                bm.getKey(rightDataIdx), bm.getValue(rightDataIdx), rightOriginalHash, improve(rightOriginalHash),
                nextShift
              )
            }

            newContent(newContentSize - compressedNodeIdx - 1) = newNode
            compressedNodeIdx += 1
            leftDataIdx += 1
            rightDataIdx += 1
            newSize += newNode.size
            newCachedHashCode += newNode.cachedJavaKeySetHashCode
          } else if ((bitpos & leftDataRightDataRightOverwrites) != 0) {
            val originalHash = bm.originalHashes(rightDataIdx)
            newContent(MapNode.TupleLength * compressedDataIdx) = bm.getKey(rightDataIdx).asInstanceOf[AnyRef]
            newContent(MapNode.TupleLength * compressedDataIdx + 1) = bm.getValue(rightDataIdx).asInstanceOf[AnyRef]
            newOriginalHashes(compressedDataIdx) = originalHash

            compressedDataIdx += 1
            rightDataIdx += 1
            newSize += 1
            newCachedHashCode += improve(originalHash)
            leftDataIdx += 1
          }

          if (bitpos == maximumBitPos) {
            finished = true
          } else {
            bitpos = bitpos << 1
          }
        }
      }

      if (anyChangesMadeSoFar)
        new BitmapIndexedMapNode(
          dataMap = newDataMap,
          nodeMap = newNodeMap,
          content = newContent,
          originalHashes = newOriginalHashes,
          size = newSize,
          cachedJavaKeySetHashCode = newCachedHashCode
        )
      else bm

    case _ =>
      // should never happen -- hash collisions are never at the same level as bitmapIndexedMapNodes
      throw new UnsupportedOperationException("Cannot concatenate a HashCollisionMapNode with a BitmapIndexedMapNode")
  }

  /** Returns a deep copy of this node: the `content` and `originalHashes` arrays are cloned and
   *  every sub-node is copied recursively, so the result shares no mutable structure with this
   *  node. Used by `HashMapBuilder` to unalias a trie that has been handed out via `result()`.
   */
  override def copy(): BitmapIndexedMapNode[K, V] = {
    val contentClone = content.clone()
    val contentLength = contentClone.length
    var i = bitCount(dataMap) * TupleLength
    while (i < contentLength) {
      contentClone(i) = contentClone(i).asInstanceOf[MapNode[K, V]].copy()
      i += 1
    }
    new BitmapIndexedMapNode[K, V](dataMap, nodeMap, contentClone, originalHashes.clone(), size, cachedJavaKeySetHashCode)
  }

  /** Returns a node containing the key-value pairs of this subtree selected by the predicate;
   *  implements both `filter` (`flipped = false`) and `filterNot` (`flipped = true`). Nodes
   *  with only payload entries take a fast path that just rebuilds the compressed arrays;
   *  otherwise sub-nodes are filtered recursively, dropped when emptied, inlined as payload
   *  entries when reduced to a single pair, and shared when unchanged. Returns this node if
   *  nothing is removed and the canonical empty node if nothing remains.
   *
   *  @param pred the predicate used to test key-value pairs
   *  @param flipped if `false`, keeps the pairs satisfying `pred`; if `true`, keeps the pairs
   *                not satisfying it
   *  @return a node containing the selected key-value pairs
   */
  override def filterImpl(pred: ((K, V)) => Boolean, flipped: Boolean): BitmapIndexedMapNode[K, V] = {
    if (size == 0) this
    else if (size == 1) {
      if (pred(getPayload(0)) != flipped) this else MapNode.empty
    } else if (nodeMap == 0) {
      // Performance optimization for nodes of depth 1:
      //
      // this node has no "node" children, all children are inlined data elems, therefor logic is significantly simpler
      // approach:
      //   * traverse the content array, accumulating in `newDataMap: Int` any bit positions of keys which pass the filter
      //   * (bitCount(newDataMap) * TupleLength) tells us the new content array and originalHashes array size, so now perform allocations
      //   * traverse the content array once more, placing each passing element (according to `newDatamap`) in the new content and originalHashes arrays
      //
      // note:
      //   * this optimization significantly improves performance of not only small trees, but also larger trees, since
      //     even non-root nodes are affected by this improvement, and large trees will consist of many nodes as
      //     descendants
      //
      val minimumIndex: Int = Integer.numberOfTrailingZeros(dataMap)
      val maximumIndex: Int = Node.BranchingFactor - Integer.numberOfLeadingZeros(dataMap)

      var newDataMap = 0
      var newCachedHashCode = 0
      var dataIndex = 0

      var i = minimumIndex

      while(i < maximumIndex) {
        val bitpos = bitposFrom(i)

        if ((bitpos & dataMap) != 0) {
          val payload = getPayload(dataIndex)
          val passed = pred(payload) != flipped

          if (passed) {
            newDataMap |= bitpos
            newCachedHashCode += improve(getHash(dataIndex))
          }

          dataIndex += 1
        }

        i += 1
      }

      if (newDataMap == 0) {
        MapNode.empty
      } else if (newDataMap == dataMap) {
        this
      } else {
        val newSize = Integer.bitCount(newDataMap)
        val newContent = new Array[Any](newSize * TupleLength)
        val newOriginalHashCodes = new Array[Int](newSize)
        val newMaximumIndex: Int = Node.BranchingFactor - Integer.numberOfLeadingZeros(newDataMap)

        var j = Integer.numberOfTrailingZeros(newDataMap)

        var newDataIndex = 0


        while (j < newMaximumIndex) {
          val bitpos = bitposFrom(j)
          if ((bitpos & newDataMap) != 0) {
            val oldIndex = indexFrom(dataMap, bitpos)
            newContent(newDataIndex * TupleLength) = content(oldIndex * TupleLength)
            newContent(newDataIndex * TupleLength + 1) = content(oldIndex * TupleLength + 1)
            newOriginalHashCodes(newDataIndex) = originalHashes(oldIndex)
            newDataIndex += 1
          }
          j += 1
        }

        new BitmapIndexedMapNode(newDataMap, 0, newContent, newOriginalHashCodes, newSize, newCachedHashCode)
      }


    } else {
      val allMap = dataMap | nodeMap
      val minimumIndex: Int = Integer.numberOfTrailingZeros(allMap)
      val maximumIndex: Int = Node.BranchingFactor - Integer.numberOfLeadingZeros(allMap)

      var oldDataPassThrough = 0

      // bitmap of nodes which, when filtered, returned a single-element node. These must be migrated to data
      var nodeMigrateToDataTargetMap = 0
      // the queue of single-element, post-filter nodes
      @annotation.stableNull var nodesToMigrateToData: mutable.Queue[MapNode[K, V]] | Null = null

      // bitmap of all nodes which, when filtered, returned themselves. They are passed forward to the returned node
      var nodesToPassThroughMap = 0

      // bitmap of any nodes which, after being filtered, returned a node that is not empty, but also not `eq` itself
      // These are stored for later inclusion into the final `content` array
      // not named `newNodesMap` (plural) to avoid confusion with `newNodeMap` (singular)
      var mapOfNewNodes = 0
      // each bit in `mapOfNewNodes` corresponds to one element in this queue
      @annotation.stableNull var newNodes: mutable.Queue[MapNode[K, V]] | Null = null

      var newDataMap = 0
      var newNodeMap = 0
      var newSize = 0
      var newCachedHashCode = 0

      var dataIndex = 0
      var nodeIndex = 0

      var i = minimumIndex
      while (i < maximumIndex) {
        val bitpos = bitposFrom(i)

        if ((bitpos & dataMap) != 0) {
          val payload = getPayload(dataIndex)
          val passed = pred(payload) != flipped

          if (passed) {
            newDataMap |= bitpos
            oldDataPassThrough |= bitpos
            newSize += 1
            newCachedHashCode += improve(getHash(dataIndex))
          }

          dataIndex += 1
        } else if ((bitpos & nodeMap) != 0) {
          val oldSubNode = getNode(nodeIndex)
          val newSubNode = oldSubNode.filterImpl(pred, flipped)

          newSize += newSubNode.size
          newCachedHashCode += newSubNode.cachedJavaKeySetHashCode

          // if (newSubNode.size == 0) do nothing (drop it)
          if (newSubNode.size > 1) {
            newNodeMap |= bitpos
            if (oldSubNode eq newSubNode) {
              nodesToPassThroughMap |= bitpos
            } else {
              mapOfNewNodes |= bitpos
              if (newNodes == null) {
                newNodes = mutable.Queue.empty
              }
              newNodes += newSubNode
            }
          } else if (newSubNode.size == 1) {
            newDataMap |= bitpos
            nodeMigrateToDataTargetMap |= bitpos
            if (nodesToMigrateToData == null) {
              nodesToMigrateToData = mutable.Queue()
            }
            nodesToMigrateToData += newSubNode
          }

          nodeIndex += 1
        }

        i += 1
      }

      if (newSize == 0) {
        MapNode.empty
      } else if (newSize == size) {
        this
      } else {
        val newDataSize = bitCount(newDataMap)
        val newContentSize = (MapNode.TupleLength * newDataSize) + bitCount(newNodeMap)
        val newContent = new Array[Any](newContentSize)
        val newOriginalHashes = new Array[Int](newDataSize)

        val newAllMap = newDataMap | newNodeMap
        val maxIndex = Node.BranchingFactor - Integer.numberOfLeadingZeros(newAllMap)

        // note: We MUST start from the minimum index in the old (`this`) node, otherwise `old{Node,Data}Index` will
        // not be incremented properly. Otherwise we could have started at Integer.numberOfTrailingZeroes(newAllMap)
        var i = minimumIndex

        var oldDataIndex = 0
        var oldNodeIndex = 0

        var newDataIndex = 0
        var newNodeIndex = 0

        while (i < maxIndex) {
          val bitpos = bitposFrom(i)

          if ((bitpos & oldDataPassThrough) != 0) {
            newContent(newDataIndex * TupleLength) = getKey(oldDataIndex)
            newContent(newDataIndex * TupleLength + 1) = getValue(oldDataIndex)
            newOriginalHashes(newDataIndex) = getHash(oldDataIndex)
            newDataIndex += 1
            oldDataIndex += 1
          } else if ((bitpos & nodesToPassThroughMap) != 0) {
            newContent(newContentSize - newNodeIndex - 1) = getNode(oldNodeIndex)
            newNodeIndex += 1
            oldNodeIndex += 1
          } else if ((bitpos & nodeMigrateToDataTargetMap) != 0) {
            // we need not check for null here. If nodeMigrateToDataTargetMap != 0, then nodesMigrateToData must not be null
            val node = nodesToMigrateToData.nn.dequeue()
            newContent(TupleLength * newDataIndex) = node.getKey(0)
            newContent(TupleLength * newDataIndex + 1) = node.getValue(0)
            newOriginalHashes(newDataIndex) = node.getHash(0)
            newDataIndex += 1
            oldNodeIndex += 1
          } else if ((bitpos & mapOfNewNodes) != 0) {
            newContent(newContentSize - newNodeIndex - 1) = newNodes.nn.dequeue()
            newNodeIndex += 1
            oldNodeIndex += 1
          } else if ((bitpos & dataMap) != 0) {
            oldDataIndex += 1
          } else if ((bitpos & nodeMap) != 0) {
            oldNodeIndex += 1
          }

          i += 1
        }

        new BitmapIndexedMapNode[K, V](newDataMap, newNodeMap, newContent, newOriginalHashes, newSize, newCachedHashCode)
      }
    }
  }
}

private final class HashCollisionMapNode[K, +V ](
  /** The original (unimproved) `key.##` shared by every key in this node. */
  val originalHash: Int,
  /** The improved hash shared by every key in this node; the full 32-bit collision is what
   *  forced these keys into a collision node.
   */
  val hash: Int,
  /** The key-value pairs of this node, in insertion order; always at least two. `var` only so
   *  `HashMapBuilder` can swap in an updated vector in place.
   */
  var content: Vector[(K, V @uV)]
  ) extends MapNode[K, V] {

  import Node._

  require(content.length >= 2)

  releaseFence()

  private[immutable] def indexOf(key: Any): Int = {
    val iter = content.iterator
    var i = 0
    while (iter.hasNext) {
      if (iter.next()._1 == key) return i
      i += 1
    }
    -1
  }

  /** Returns the number of key-value pairs in this node; always at least 2. */
  def size: Int = content.length

  /** Returns the value associated with `key` in this node.
   *
   *  @param key the key to look up
   *  @param originalHash the original hash code of `key` (via `key.##`); never used
   *  @param hash the improved hash of `key`, compared against this node's collision hash
   *  @param shift the bit-level offset into the hash code; never used, as collision nodes have
   *              no sub-structure
   *  @return the value bound to `key`
   *  @throws NoSuchElementException if `key` is not bound in this node
   */
  def apply(key: K, originalHash: Int, hash: Int, shift: Int): V = get(key, originalHash, hash, shift).getOrElse(Iterator.empty.next())

  /** Optionally returns the value associated with `key` in this node, by linear search of
   *  `content` after checking the collision hash.
   *
   *  @param key the key to look up
   *  @param originalHash the original hash code of `key` (via `key.##`); never used
   *  @param hash the improved hash of `key`, compared against this node's collision hash
   *  @param shift the bit-level offset into the hash code; never used, as collision nodes have
   *              no sub-structure
   *  @return `Some(value)` if `key` is bound in this node, `None` otherwise
   */
  def get(key: K, originalHash: Int, hash: Int, shift: Int): Option[V] =
    if (this.hash == hash) {
      val index = indexOf(key)
      if (index >= 0) Some(content(index)._2) else None
    } else None

  /** Returns the stored `(key, value)` tuple for `key`, containing the exact (reference-equal)
   *  key instance held in this node.
   *
   *  @param key the key to look up
   *  @param originalHash the original hash code of `key` (via `key.##`); never used
   *  @param hash the improved hash of `key`; never used, as the linear search compares keys
   *             directly
   *  @param shift the bit-level offset into the hash code; never used
   *  @return the `(key, value)` tuple bound to `key` in this node
   *  @throws NoSuchElementException if `key` is not bound in this node
   */
  override def getTuple(key: K, originalHash: Int, hash: Int, shift: Int): (K, V) = {
    val index = indexOf(key)
    if (index >= 0) content(index) else Iterator.empty.next()
  }

  /** Returns the value associated with `key` in this node, or the default value if `key` is
   *  not bound.
   *
   *  @tparam V1 the result type, a supertype of `V`
   *  @param key the key to look up
   *  @param originalHash the original hash code of `key` (via `key.##`); never used
   *  @param hash the improved hash of `key`, compared against this node's collision hash
   *  @param shift the bit-level offset into the hash code; never used
   *  @param f a computation that yields the default value; only evaluated if `key` is not bound
   *  @return the value bound to `key`, or `f` if `key` is not bound
   */
  def getOrElse[V1 >: V](key: K, originalHash: Int, hash: Int, shift: Int, f: => V1): V1 = {
    if (this.hash == hash) {
      indexOf(key) match {
        case -1 => f
        case other => content(other)._2
      }
    } else f
  }

  /** Tests whether `key` is bound in this node.
   *
   *  @param key the key to look up
   *  @param originalHash the original hash code of `key` (via `key.##`); never used
   *  @param hash the improved hash of `key`, compared against this node's collision hash
   *  @param shift the bit-level offset into the hash code; never used
   *  @return `true` if `key` is bound in this node, `false` otherwise
   */
  override def containsKey(key: K, originalHash: Int, hash: Int, shift: Int): Boolean =
    this.hash == hash && indexOf(key) >= 0

  /** Tests whether this node binds `key` to exactly the given value, compared by reference.
   *
   *  @tparam V1 the type of the value to test, a supertype of `V`
   *  @param key the key to look up
   *  @param value the value that must be reference-equal to the stored value
   *  @param hash the improved hash of `key`, compared against this node's collision hash
   *  @param shift the bit-level offset into the hash code; never used
   *  @return `true` if `key` is bound to a value that is `eq` to `value`
   */
  def contains[V1 >: V](key: K, value: V1, hash: Int, shift: Int): Boolean =
    this.hash == hash && {
      val index = indexOf(key)
      index >= 0 && (content(index)._2.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef])
    }

  /** Returns a node with `key` bound to `value`: a new key is appended to `content`, and an
   *  existing binding is replaced only when `replaceValue` is `true`. Returns this node
   *  unchanged if `key` is present and either `replaceValue` is `false` or the stored value is
   *  reference-equal to `value`.
   *
   *  @tparam V1 the value type of the returned node, a supertype of `V`
   *  @param key the key to add or update; assumed to have this node's collision hash
   *  @param value the value to associate with `key`
   *  @param originalHash the original hash code of `key` (via `key.##`), passed on to the
   *                     resulting node
   *  @param hash the improved hash of `key`, passed on to the resulting node
   *  @param shift the bit-level offset into the hash code; never used
   *  @param replaceValue if `true`, an existing binding for `key` is replaced; if `false`, the
   *                     pair is only inserted when `key` is absent
   *  @return a node containing the binding, or this node if nothing changed
   */
  def updated[V1 >: V](key: K, value: V1, originalHash: Int, hash: Int, shift: Int, replaceValue: Boolean): MapNode[K, V1] = {
    val index = indexOf(key)
    if (index >= 0) {
      if (replaceValue) {
        if (content(index)._2.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) {
          this
        } else {
          new HashCollisionMapNode[K, V1](originalHash, hash, content.updated[(K, V1)](index, (key, value)))
        }
      } else {
        this
      }
    } else {
      new HashCollisionMapNode[K, V1](originalHash, hash, content.appended[(K, V1)]((key, value)))
    }
  }

  /** Returns a node with the binding for `key` removed, or this node unchanged if `key` is not
   *  bound. If only one pair remains, it is repackaged as a single-entry
   *  `BitmapIndexedMapNode` (keyed by the first 5-bit segment of the collision hash) so it can
   *  be inlined by an ancestor, keeping the trie canonical.
   *
   *  @tparam V1 the value type of the returned node, a supertype of `V`
   *  @param key the key to remove
   *  @param originalHash the original hash code of `key` (via `key.##`)
   *  @param hash the improved hash of `key`, compared against this node's collision hash
   *  @param shift the bit-level offset into the hash code; never used beyond the containment
   *              check
   *  @return a node without a binding for `key`, or this node if none was present
   */
  def removed[V1 >: V](key: K, originalHash: Int, hash: Int, shift: Int): MapNode[K, V1] = {
    if (!this.containsKey(key, originalHash, hash, shift)) {
      this
    } else {
      val updatedContent = content.filterNot(keyValuePair => keyValuePair._1 == key)
      // assert(updatedContent.size == content.size - 1)

      updatedContent.size match {
        case 1 =>
          val (k, v) = updatedContent(0)
          new BitmapIndexedMapNode[K, V1](bitposFrom(maskFrom(hash, 0)), 0, Array(k, v), Array(originalHash), 1, hash)
        case _ => new HashCollisionMapNode[K, V1](originalHash, hash, updatedContent)
      }
    }
  }

  /** Returns `false`: a collision node is a leaf and never has sub-nodes. */
  def hasNodes: Boolean = false

  /** Returns `0`: a collision node is a leaf and never has sub-nodes. */
  def nodeArity: Int = 0

  /** Always throws: a collision node is a leaf and never has sub-nodes.
   *
   *  @param index the requested sub-node position; never used
   *  @throws IndexOutOfBoundsException always
   */
  def getNode(index: Int): MapNode[K, V] =
    throw new IndexOutOfBoundsException("No sub-nodes present in hash-collision leaf node.")

  /** Returns `true`: a collision node always holds at least two payload entries. */
  def hasPayload: Boolean = true

  /** Returns the number of key-value pairs in this node; all entries are payload. */
  def payloadArity: Int = content.length

  /** Returns the key of the payload entry at the given position.
   *
   *  @param index the position among this node's entries, from `0` until `payloadArity`
   *  @return the key of the entry at `index`
   */
  def getKey(index: Int): K = getPayload(index)._1
  /** Returns the value of the payload entry at the given position.
   *
   *  @param index the position among this node's entries, from `0` until `payloadArity`
   *  @return the value of the entry at `index`
   */
  def getValue(index: Int): V = getPayload(index)._2

  /** Returns the payload entry at the given position as a key-value tuple.
   *
   *  @param index the position among this node's entries, from `0` until `payloadArity`
   *  @return the `(key, value)` pair at `index`
   */
  def getPayload(index: Int): (K, V) = content(index)

  /** Returns the original (unimproved) hash code shared by every key in this node, regardless
   *  of `index`.
   *
   *  @param index the position among this node's entries; never used, as all keys share the
   *              same hash
   *  @return this node's `originalHash`
   */
  override def getHash(index: Int): Int = originalHash

  /** Applies a function to each key-value pair of this node, in the order of `content`.
   *
   *  @tparam U the result type of `f`; the results are discarded
   *  @param f the function applied to each key-value pair
   */
  def foreach[U](f: ((K, V)) => U): Unit = content.foreach(f)

  /** Applies a two-argument function to each key and value of this node.
   *
   *  @tparam U the result type of `f`; the results are discarded
   *  @param f the function applied to each key and its associated value
   */
  def foreachEntry[U](f: (K, V) => U): Unit = content.foreach { case (k, v) => f(k, v)}

  /** Applies a function to each key and value of this node together with the shared original
   *  (unimproved) hash.
   *
   *  @param f the function applied to each key, value, and original hash triple
   */
  override def foreachWithHash(f: (K, V, Int) => Unit): Unit = {
    val iter = content.iterator
    while (iter.hasNext) {
      val next = iter.next()
      f(next._1, next._2, originalHash)
    }
  }

  /** Returns a node with the same keys, where each value is the result of applying `f` to its
   *  key and current value. Returns this node if every result is reference-equal to the value
   *  it replaces.
   *
   *  @tparam W the value type of the returned node
   *  @param f the function applied to each key and its associated value
   *  @return a node with each value replaced by the result of `f`, or this node if nothing
   *          changed
   */
  override def transform[W](f: (K, V) => W): HashCollisionMapNode[K, W] = {
    val newContent = Vector.newBuilder[(K, W)]
    val contentIter = content.iterator
    // true if any values have been transformed to a different value via `f`
    var anyChanges = false
    while(contentIter.hasNext) {
      val (k, v) = contentIter.next()
      val newValue = f(k, v)
      newContent.addOne((k, newValue))
      anyChanges ||= (v.asInstanceOf[AnyRef] ne newValue.asInstanceOf[AnyRef])
    }
    if (anyChanges) new HashCollisionMapNode(originalHash, hash, newContent.result())
    else this.asInstanceOf[HashCollisionMapNode[K, W]]
  }

  /** Tests whether this node is equal to another object. Two `HashCollisionMapNode`s are equal
   *  when they have the same collision hash and the same key-value pairs, compared as unordered
   *  sets: each pair of this node must occur in the other, in any order.
   *
   *  @param that the object to compare with
   *  @return `true` if `that` is a `HashCollisionMapNode` with the same hash and pairs
   */
  override def equals(that: Any): Boolean =
    that match {
      case node: HashCollisionMapNode[?, ?] =>
        (this eq node) ||
          (this.hash == node.hash) &&
            (this.content.length == node.content.length) && {
              val iter = content.iterator
              while (iter.hasNext) {
                val (key, value) = iter.next()
                val index = node.indexOf(key)
                if (index < 0 || value != node.content(index)._2) {
                  return false
                }
              }
              true
            }
      case _ => false
    }

  /** Returns a node containing all key-value pairs of this node and `that`, with the pairs of
   *  `that` winning on keys present in both: the result starts from `that`'s content and
   *  appends only the pairs of this node whose keys are absent from `that`. Returns `that` if
   *  every key of this node also occurs in `that`, and this node if `that eq this`.
   *
   *  @tparam V1 the value type of the returned node, a supertype of `V`
   *  @param that the node to concatenate with; must be a `HashCollisionMapNode` for the same
   *             collision hash
   *  @param shift the bit-level offset into the hash code; never used
   *  @return a collision node containing the union of both nodes' key-value pairs
   *  @throws UnsupportedOperationException if `that` is a `BitmapIndexedMapNode`, which can
   *                                       never sit at the same level as a collision node
   */
  override def concat[V1 >: V](that: MapNode[K, V1], shift: Int): HashCollisionMapNode[K, V1] = that match {
    case hc: HashCollisionMapNode[K, V1] =>
      if (hc eq this) {
        this
      } else {
        var newContent: VectorBuilder[(K, V1)] | Null = null
        val iter = content.iterator
        while (iter.hasNext) {
          val nextPayload = iter.next()
          if (hc.indexOf(nextPayload._1) < 0) {
            if (newContent == null) {
              newContent = new VectorBuilder[(K, V1)]()
              newContent.addAll(hc.content)
            }
            newContent.addOne(nextPayload)
          }
        }
        if (newContent eq null) hc else new HashCollisionMapNode(originalHash, hash, newContent.result())
      }
    case _: BitmapIndexedMapNode[K, V1] =>
      // should never happen -- hash collisions are never at the same level as bitmapIndexedMapNodes
      throw new UnsupportedOperationException("Cannot concatenate a HashCollisionMapNode with a BitmapIndexedMapNode")
  }


  /** Merges this collision node with `that`, a collision node at the same position in the
   *  right-hand trie, adding each resulting pair to `builder`. Pairs whose keys occur on only
   *  one side are added directly; keys present in both are combined with `mergef`. Consumed
   *  right-hand pairs are tracked in a scratch array so the remainder can be added afterwards.
   *
   *  @tparam V1 the value type of the merged result, a supertype of `V`
   *  @param that the node from the right-hand map at the same position as this node; must be a
   *             `HashCollisionMapNode`
   *  @param builder the builder used to accumulate the merged key-value pairs
   *  @param shift the bit-level offset into the hash code; never used
   *  @param mergef the function used to combine the two pairs bound to a key present in both
   *               nodes
   *  @throws RuntimeException if `that` is a `BitmapIndexedMapNode`, which can never sit at
   *                          the same level as a collision node
   */
  override def mergeInto[V1 >: V](that: MapNode[K, V1], builder: HashMapBuilder[K, V1], shift: Int)(mergef: ((K, V), (K, V1)) => (K, V1)): Unit = that match {
    case hc: HashCollisionMapNode[K, V1] =>
      val iter = content.iterator
      val rightArray: Array[AnyRef | Null] = hc.content.toArray[AnyRef | Null] // really Array[(K, V1)]

      def rightIndexOf(key: K): Int = {
        var i = 0
        while (i < rightArray.length) {
          val elem = rightArray(i)
          if ((elem ne null) && (elem.asInstanceOf[(K, V1)])._1 == key) return i
          i += 1
        }
        -1
      }

      while (iter.hasNext) {
        val nextPayload = iter.next()
        val index = rightIndexOf(nextPayload._1)

        if (index == -1) {
          builder.addOne(nextPayload)
        } else {
          val rightPayload = rightArray(index).asInstanceOf[(K, V1)]
          rightArray(index) = null

          builder.addOne(mergef(nextPayload, rightPayload))
        }
      }

      var i = 0
      while (i < rightArray.length) {
        val elem = rightArray(i)
        if (elem ne null) builder.addOne(elem.asInstanceOf[(K, V1)])
        i += 1
      }
    case _: BitmapIndexedMapNode[K, V1] =>
      throw new RuntimeException("Cannot merge HashCollisionMapNode with BitmapIndexedMapNode")

  }

  /** Adds all key-value pairs of this node to a builder, passing along the shared original and
   *  improved hashes so they need not be recomputed.
   *
   *  @tparam V1 the value type of the target builder, a supertype of `V`
   *  @param builder the builder to add the key-value pairs to
   */
  override def buildTo[V1 >: V](builder: HashMapBuilder[K, V1]): Unit = {
    val iter = content.iterator
    while (iter.hasNext) {
      val (k, v) = iter.next()
      builder.addOne(k, v, originalHash, hash)
    }
  }

  /** Returns a node containing the key-value pairs of this node selected by the predicate;
   *  implements both `filter` (`flipped = false`) and `filterNot` (`flipped = true`). Returns
   *  the canonical empty node if nothing remains, a single-entry `BitmapIndexedMapNode` (so it
   *  can be inlined by an ancestor) if one pair remains, and this node if all pairs remain.
   *
   *  @param pred the predicate used to test key-value pairs
   *  @param flipped if `false`, keeps the pairs satisfying `pred`; if `true`, keeps the pairs
   *                not satisfying it
   *  @return a node containing the selected key-value pairs
   */
  override def filterImpl(pred: ((K, V)) => Boolean, flipped: Boolean): MapNode[K, V] = {
    val newContent = content.filterImpl(pred, flipped)
    val newContentLength = newContent.length
    if (newContentLength == 0) {
      MapNode.empty
    } else if (newContentLength == 1) {
      val (k, v) = newContent.head
      new BitmapIndexedMapNode[K, V](bitposFrom(maskFrom(hash, 0)), 0, Array(k, v), Array(originalHash), 1, hash)
    } else if (newContentLength == content.length) this
    else new HashCollisionMapNode(originalHash, hash, newContent)
  }

  /** Returns a new collision node with the same hashes and the same (immutable) content
   *  vector. The copy is shallow but sufficient for `HashMapBuilder`'s unaliasing, because the
   *  builder mutates the `content` field, not the vector it points to.
   */
  override def copy(): HashCollisionMapNode[K, V] = new HashCollisionMapNode[K, V](originalHash, hash, content)

  /** Always throws: trie nodes define `equals` for structural comparison but are never used as
   *  hash keys themselves.
   *
   *  @throws UnsupportedOperationException always
   */
  override def hashCode(): Int =
    throw new UnsupportedOperationException("Trie nodes do not support hashing.")

  /** Returns the default identity-based representation (class name and identity hash code);
   *  contents are deliberately not printed.
   */
  override def toString() = s"${getClass.getName}@${Integer.toHexString(System.identityHashCode(this))}"

  /** Returns the sum of the improved hashes of all keys in this node: `size * hash`, since
   *  every key shares the same improved hash. Computed on demand rather than stored.
   */
  override def cachedJavaKeySetHashCode: Int = size * hash

}

private final class MapKeyIterator[K, V](rootNode: MapNode[K, V])
  extends ChampBaseIterator[K, MapNode[K, V]](rootNode) {

  /** Returns the next key and advances the cursor.
   *
   *  @throws NoSuchElementException if no keys remain
   */
  def next() = {
    if (!hasNext) Iterator.empty.next()

    val key = currentValueNode.getKey(currentValueCursor)
    currentValueCursor += 1

    key
  }

}

private final class MapValueIterator[K, V](rootNode: MapNode[K, V])
  extends ChampBaseIterator[V, MapNode[K, V]](rootNode) {

  /** Returns the next value and advances the cursor.
   *
   *  @throws NoSuchElementException if no values remain
   */
  def next() = {
    if (!hasNext) Iterator.empty.next()

    val value = currentValueNode.getValue(currentValueCursor)
    currentValueCursor += 1

    value
  }
}

private final class MapKeyValueTupleIterator[K, V](rootNode: MapNode[K, V])
  extends ChampBaseIterator[(K, V), MapNode[K, V]](rootNode) {

  /** Returns the next key-value pair and advances the cursor.
   *
   *  @throws NoSuchElementException if no pairs remain
   */
  def next() = {
    if (!hasNext) Iterator.empty.next()

    val payload = currentValueNode.getPayload(currentValueCursor)
    currentValueCursor += 1

    payload
  }

}

private final class MapKeyValueTupleReverseIterator[K, V](rootNode: MapNode[K, V])
  extends ChampBaseReverseIterator[(K, V), MapNode[K, V]](rootNode) {

  /** Returns the next key-value pair in reverse order and moves the cursor backwards.
   *
   *  @throws NoSuchElementException if no pairs remain
   */
  def next() = {
    if (!hasNext) Iterator.empty.next()

    val payload = currentValueNode.getPayload(currentValueCursor)
    currentValueCursor -= 1

    payload
  }
}

private final class MapKeyValueTupleHashIterator[K, V](rootNode: MapNode[K, V])
  extends ChampBaseReverseIterator[Any, MapNode[K, V]](rootNode) {
  private var hash = 0
  private var value: V = compiletime.uninitialized
  /** Returns the hash code of the current key-value pair, computed as the tuple hash of the
   *  key's cached original hash and the value's hash code. This equals `(key, value).##`
   *  without materializing the tuple, which is what lets `HashMap.hashCode` feed this iterator
   *  itself to `MurmurHash3.unorderedHash`.
   */
  override def hashCode(): Int = MurmurHash3.tuple2Hash(hash, value.##, MurmurHash3.productSeed)
  /** Advances to the next key-value pair and returns this iterator itself, whose `hashCode`
   *  then reflects that pair.
   *
   *  @throws NoSuchElementException if no pairs remain
   */
  def next(): MapKeyValueTupleHashIterator[K, V] = {
    if (!hasNext) Iterator.empty.next()

    hash = currentValueNode.getHash(currentValueCursor)
    value = currentValueNode.getValue(currentValueCursor)
    currentValueCursor -= 1
    this
  }
}

/** Used in HashMap[K, V]#removeAll(HashSet[K]).
 *
 *  @tparam K the key type
 *  @param rootSetNode the root node of the `HashSet` whose keys are to be removed
 */
private final class MapNodeRemoveAllSetNodeIterator[K](rootSetNode: SetNode[K]) extends ChampBaseIterator[K, SetNode[K]](rootSetNode) {
  /** Returns the result of immutably removing all keys in `rootSetNode` from `rootMapNode`.
   *
   *  @tparam V the value type of the map node
   *  @param rootMapNode the root node of the map from which keys will be removed
   */
  def removeAll[V](rootMapNode: BitmapIndexedMapNode[K, V]): BitmapIndexedMapNode[K, V] = {
    var curr = rootMapNode
    while (curr.size > 0 && hasNext) {
      val originalHash = currentValueNode.getHash(currentValueCursor)
      curr = curr.removed(
        key = currentValueNode.getPayload(currentValueCursor),
        keyHash = improve(originalHash),
        originalHash = originalHash,
        shift = 0
      )
      currentValueCursor += 1
    }
    curr
  }

  /** Always throws: this iterator is driven through `removeAll` and its cursor fields, and
   *  `next()` is never called.
   *
   *  @throws NoSuchElementException always
   */
  override def next(): K = Iterator.empty.next()
}

/** $factoryInfo
 *
 *  @define Coll `immutable.HashMap`
 *  @define coll immutable champ hash map
 */
@SerialVersionUID(3L)
object HashMap extends MapFactory[HashMap] {

  @transient
  private final val EmptyMap = new HashMap(MapNode.empty)

  /** Returns the empty `HashMap`: a single shared, immutable instance cast to the requested
   *  key and value types.
   *
   *  @tparam K the key type of the returned map
   *  @tparam V the value type of the returned map
   *  @return the shared empty hash map
   */
  def empty[K, V]: HashMap[K, V] =
    EmptyMap.asInstanceOf[HashMap[K, V]]

  /** Returns a `HashMap` containing the key-value pairs of the given collection. When keys
   *  repeat, later pairs win. A source that is already a `HashMap` is returned as is, a source
   *  known to be empty yields the shared empty map, and anything else is copied through a
   *  builder.
   *
   *  @tparam K the key type of the returned map
   *  @tparam V the value type of the returned map
   *  @param source the collection of key-value pairs
   *  @return a hash map containing the pairs of `source`
   */
  def from[K, V](source: collection.IterableOnce[(K, V)]^): HashMap[K, V] =
    (source: @unchecked) match {
      case hs: HashMap[K, V] => hs
      case _ if source.knownSize == 0 => empty[K, V]
      case _ => (newBuilder[K, V] ++= source).result()
    }

  /** Creates a new Builder which can be reused after calling `result()` without an
   *  intermediate call to `clear()` in order to build multiple related results.
   *
   *  @tparam K the key type of the HashMap
   *  @tparam V the value type of the HashMap
   *  @return a fresh `ReusableBuilder` that constructs `HashMap[K, V]` instances and can be reused across multiple results
   */
  def newBuilder[K, V]: ReusableBuilder[(K, V), HashMap[K, V]] = new HashMapBuilder[K, V]
}


/** A `Builder` for a `HashMap`.
 *  $multipleResults
 *
 *  @tparam K the key type of the HashMap being built
 *  @tparam V the value type of the HashMap being built
 */
private[immutable] final class HashMapBuilder[K, V] extends ReusableBuilder[(K, V), HashMap[K, V]] {
  import MapNode._
  import Node._

  private def newEmptyRootNode = new BitmapIndexedMapNode[K, V](0, 0, Array.emptyObjectArray.asInstanceOf[Array[Any]], Array.emptyIntArray, 0, 0)

  /** The last given out HashMap as a return value of `result()`, if any, otherwise null.
   *  Indicates that on next add, the elements should be copied to an identical structure, before continuing
   *  mutations.
   */
  @annotation.stableNull
  private var aliased: HashMap[K, V] | Null = compiletime.uninitialized

  private def isAliased: Boolean = aliased != null

  /** The root node of the partially built hashmap. */
  private var rootNode: BitmapIndexedMapNode[K, V] = newEmptyRootNode

  private[immutable] def getOrElse[V0 >: V](key: K, value: V0): V0 =
    if (rootNode.size == 0) value
    else {
      val originalHash = key.##
      rootNode.getOrElse(key, originalHash, improve(originalHash), 0, value)
    }

  /** Inserts element `elem` into array `as` at index `ix`, shifting right the trailing elems.
   *
   *  @param as the source array to insert into
   *  @param ix the zero-based index at which to insert `elem`
   *  @param elem the element to insert
   *  @return a new array of length `as.length + 1` containing the elements of `as` with `elem` inserted at index `ix`
   */
  private def insertElement(as: Array[Int], ix: Int, elem: Int): Array[Int] = {
    if (ix < 0) throw new ArrayIndexOutOfBoundsException
    if (ix > as.length) throw new ArrayIndexOutOfBoundsException
    val result = new Array[Int](as.length + 1)
    arraycopy(as, 0, result, 0, ix)
    result(ix) = elem
    arraycopy(as, ix, result, ix + 1, as.length - ix)
    result
  }

  /** Inserts key-value into the bitmapIndexMapNode. Requires that this is a new key-value pair.
   *
   *  @tparam V1 the value type being inserted, a supertype of `V`
   *  @param bm the bitmap-indexed map node to mutate
   *  @param bitpos the bit position in the bitmap where the key-value pair should be inserted
   *  @param key the key to insert
   *  @param originalHash the original hash code of `key` (via `key.##`)
   *  @param keyHash the improved hash of `key`
   *  @param value the value to associate with `key`
   */
  private def insertValue[V1 >: V](bm: BitmapIndexedMapNode[K, V],bitpos: Int, key: K, originalHash: Int, keyHash: Int, value: V1): Unit = {
    val dataIx = bm.dataIndex(bitpos)
    val idx = TupleLength * dataIx

    val src = bm.content
    val dst = new Array[Any](src.length + TupleLength)

    // copy 'src' and insert 2 element(s) at position 'idx'
    arraycopy(src, 0, dst, 0, idx)
    dst(idx) = key
    dst(idx + 1) = value
    arraycopy(src, idx, dst, idx + TupleLength, src.length - idx)

    val dstHashes = insertElement(bm.originalHashes, dataIx, originalHash)

    bm.dataMap |= bitpos
    bm.content = dst
    bm.originalHashes = dstHashes
    bm.size += 1
    bm.cachedJavaKeySetHashCode += keyHash
  }

  /** Upserts a key/value pair into mapNode, mutably.
   *
   *  @param mapNode the map node to update in place
   *  @param key the key to insert or update
   *  @param value the value to associate with `key`
   *  @param originalHash the original hash code of `key` (via `key.##`)
   *  @param keyHash the improved hash of `key`
   *  @param shift the bit-level offset into the hash code, equal to `depth * BitPartitionSize`
   */
  private[immutable] def update(mapNode: MapNode[K, V], key: K, value: V, originalHash: Int, keyHash: Int, shift: Int): Unit = {
    mapNode match {
      case bm: BitmapIndexedMapNode[K, V] =>
        val mask = maskFrom(keyHash, shift)
        val bitpos = bitposFrom(mask)
        if ((bm.dataMap & bitpos) != 0) {
          val index = indexFrom(bm.dataMap, mask, bitpos)
          val key0 = bm.getKey(index)
          val key0UnimprovedHash = bm.getHash(index)

          if (key0UnimprovedHash == originalHash && key0 == key) {
            bm.content(TupleLength * index + 1) = value
          } else {
            val value0 = bm.getValue(index)
            val key0Hash = improve(key0UnimprovedHash)

            val subNodeNew: MapNode[K, V] =
              bm.mergeTwoKeyValPairs(key0, value0, key0UnimprovedHash, key0Hash, key, value, originalHash, keyHash, shift + BitPartitionSize)

            bm.migrateFromInlineToNodeInPlace(bitpos, key0Hash, subNodeNew)
          }

        } else if ((bm.nodeMap & bitpos) != 0) {
          val index = indexFrom(bm.nodeMap, mask, bitpos)
          val subNode = bm.getNode(index)
          val beforeSize = subNode.size
          val beforeHash = subNode.cachedJavaKeySetHashCode
          update(subNode, key, value, originalHash, keyHash, shift + BitPartitionSize)
          bm.size += subNode.size - beforeSize
          bm.cachedJavaKeySetHashCode += subNode.cachedJavaKeySetHashCode - beforeHash
        } else {
          insertValue(bm, bitpos, key, originalHash, keyHash, value)
        }
      case hc: HashCollisionMapNode[K, V] =>
        val index = hc.indexOf(key)
        if (index < 0) {
          hc.content = hc.content.appended((key, value))
        } else {
          hc.content = hc.content.updated(index, (key, value))
        }
    }
  }

  /** If currently referencing aliased structure, copy elements to new mutable structure. */
  private def ensureUnaliased() = {
    if (isAliased) copyElems()
    aliased = null
  }

  /** Copies elements to new mutable structure. */
  private def copyElems(): Unit = {
    rootNode = rootNode.copy()
  }

  /** Returns the map built so far, without clearing the builder. The current root node is
   *  wrapped and remembered in `aliased`; because the returned map now shares that structure,
   *  the next mutating call first copies the trie (`ensureUnaliased`) so the result is never
   *  changed retroactively. Repeated calls without intervening additions return the same map.
   */
  override def result(): HashMap[K, V] =
    if (rootNode.size == 0) {
      HashMap.empty
    } else if (aliased != null) {
      aliased
    } else {
      aliased = new HashMap(rootNode)
      releaseFence()
      aliased
    }

  /** Adds a key-value pair to the map being built, replacing any existing binding for the key.
   *  The trie is mutated in place, after being unaliased if `result()` has been called.
   *
   *  @param elem the key-value pair to add
   *  @return this builder
   */
  override def addOne(elem: (K, V)): this.type = {
    ensureUnaliased()
    val h = elem._1.##
    val im = improve(h)
    update(rootNode, elem._1, elem._2, h, im, 0)
    this
  }

  /** Adds a key-value pair to the map being built, replacing any existing binding for the key,
   *  without requiring a tuple.
   *
   *  @param key the key to add or update
   *  @param value the value to associate with `key`
   *  @return this builder
   */
  def addOne(key: K, value: V): this.type = {
    ensureUnaliased()
    val originalHash = key.##
    update(rootNode, key, value, originalHash, improve(originalHash), 0)
    this
  }
  /** Adds a key-value pair whose original key hash is already known, replacing any existing
   *  binding for the key and skipping the `key.##` computation.
   *
   *  @param key the key to add or update
   *  @param value the value to associate with `key`
   *  @param originalHash the original hash code of `key` (via `key.##`)
   *  @return this builder
   */
  def addOne(key: K, value: V, originalHash: Int): this.type = {
    ensureUnaliased()
    update(rootNode, key, value, originalHash, improve(originalHash), 0)
    this
  }
  /** Adds a key-value pair whose original and improved key hashes are both already known,
   *  replacing any existing binding for the key and skipping all hash computation.
   *
   *  @param key the key to add or update
   *  @param value the value to associate with `key`
   *  @param originalHash the original hash code of `key` (via `key.##`)
   *  @param hash the improved hash of `key`
   *  @return this builder
   */
  def addOne(key: K, value: V, originalHash: Int, hash: Int): this.type = {
    ensureUnaliased()
    update(rootNode, key, value, originalHash, hash, 0)
    this
  }

  /** Adds all key-value pairs of the given collection to the map being built, replacing
   *  existing bindings for duplicate keys. Immutable and mutable hash maps are walked via
   *  their internal node iterators so their cached key hashes are reused instead of recomputed.
   *
   *  @param xs the collection of key-value pairs to add
   *  @return this builder
   */
  override def addAll(xs: IterableOnce[(K, V)]^): this.type = {
    ensureUnaliased()
    (xs: @unchecked) match {
      case hm: HashMap[K, V] =>
        new ChampBaseIterator[(K, V), MapNode[K, V]](hm.rootNode) {
          while(hasNext) {
            val originalHash = currentValueNode.getHash(currentValueCursor)
            update(
              mapNode = rootNode,
              key = currentValueNode.getKey(currentValueCursor),
              value = currentValueNode.getValue(currentValueCursor),
              originalHash = originalHash,
              keyHash = improve(originalHash),
              shift = 0
            )
            currentValueCursor += 1
          }

          override def next() = Iterator.empty.next()
        }
      case hm: collection.mutable.HashMap[K, V] =>
        val iter = hm.nodeIterator
        while (iter.hasNext) {
          val next = iter.next()
          val originalHash = hm.unimproveHash(next.hash)
          val hash = improve(originalHash)
          update(rootNode, next.key, next.value, originalHash, hash, 0)
        }
      case lhm: collection.mutable.LinkedHashMap[K, V] =>
        val iter = lhm.entryIterator
        while (iter.hasNext) {
          val next = iter.next()
          val originalHash = lhm.unimproveHash(next.hash)
          val hash = improve(originalHash)
          update(rootNode, next.key, next.value, originalHash, hash, 0)
        }
      case thatMap: Map[K, V] =>
        thatMap.foreachEntry((key, value) => addOne(key, value))
      case other =>
        val it = other.iterator
        while(it.hasNext) addOne(it.next())
    }

    this
  }

  /** Resets this builder to the empty state. Any previously returned map is unaffected: the
   *  alias to it is dropped and a fresh, empty root node is installed.
   */
  override def clear(): Unit = {
    aliased = null
    if (rootNode.size > 0) {
      rootNode = newEmptyRootNode
    }
  }

  private[collection] def size: Int = rootNode.size

  /** Returns the number of key-value pairs added so far; always known, never `-1`. */
  override def knownSize: Int = rootNode.size
}
