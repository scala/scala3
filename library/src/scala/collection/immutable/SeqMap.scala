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

import scala.collection.mutable.{Builder, ReusableBuilder}

/** A base trait for ordered, immutable maps.
 *
 *  Note that the [[equals]] method for [[SeqMap]] compares key-value pairs
 *  without regard to ordering.
 *
 *  All behavior is defined in terms of the abstract methods in `SeqMap`.
 *  It is sufficient for concrete subclasses to implement those methods.
 *  Methods that return a new map, in particular [[removed]] and [[updated]], must preserve ordering.
 *
 *  @tparam K      the type of the keys contained in this linked map.
 *  @tparam V      the type of the values associated with the keys in this linked map.
 *
 *  @define coll immutable seq map
 *  @define Coll `immutable.SeqMap`
 */

trait SeqMap[K, +V]
  extends Map[K, V]
    with collection.SeqMap[K, V]
    with MapOps[K, V, SeqMap, SeqMap[K, V]]
    with MapFactoryDefaults[K, V, SeqMap, Iterable] {
  /** Returns the [[SeqMap$ SeqMap]] companion object as the factory for maps of this kind. */
  override def mapFactory: MapFactory[SeqMap] = SeqMap
}


object SeqMap extends MapFactory[SeqMap] {
  /** An empty [[SeqMap]].
   *
   *  @tparam K the type of the keys
   *  @tparam V the type of the values
   *  @return the empty `SeqMap` (a single cached instance)
   */
  def empty[K, V]: SeqMap[K, V] = EmptySeqMap.asInstanceOf[SeqMap[K, V]]

  /** Returns a [[SeqMap]] containing the key-value pairs of `it`, in its iteration order.
   *
   *  If `it` is already one of the `SeqMap` implementations ([[ListMap]],
   *  [[TreeSeqMap]], [[VectorMap]], or a small `SeqMap`), it is returned unchanged.
   *  Otherwise a new map is built; if a key occurs more than once in `it`, its first
   *  occurrence determines its position and its last occurrence determines its value.
   *
   *  @tparam K the type of the keys
   *  @tparam V the type of the values
   *  @param it the source collection of key-value pairs
   *  @return a `SeqMap[K, V]` with the bindings of `it`
   */
  def from[K, V](it: collection.IterableOnce[(K, V)]^): SeqMap[K, V] =
    (it: @unchecked) match {
      //case sm: SeqMap[K, V] => sm
      case m: ListMap[K, V]    => m
      case m: TreeSeqMap[K, V] => m
      case m: VectorMap[K, V]  => m
      case m: SeqMap1[K, V]    => m
      case m: SeqMap2[K, V]    => m
      case m: SeqMap3[K, V]    => m
      case m: SeqMap4[K, V]    => m
      case it: Iterable[?] if it.isEmpty => empty[K, V]
      case _ => (newBuilder[K, V] ++= it).result()
    }

  /** Returns a new builder for a [[SeqMap]].
   *
   *  The builder uses the compact one- to four-entry representations while at most
   *  four distinct keys have been added, and switches to a [[VectorMap]] builder
   *  beyond that.
   *
   *  @tparam K the type of the keys
   *  @tparam V the type of the values
   *  @return a `Builder` that accepts key-value pairs and produces a `SeqMap[K, V]`
   */
  def newBuilder[K, V]: Builder[(K, V), SeqMap[K, V]] = new SeqMapBuilderImpl

  @SerialVersionUID(3L)
  private object EmptySeqMap extends SeqMap[Any, Nothing] with Serializable {
    /** Returns 0: this map is empty. */
    override def size: Int = 0
    /** Returns 0: the size of this map is always known. */
    override def knownSize: Int = 0
    /** Always throws: the empty map holds no keys.
     *
     *  @param key the key that was looked up, reported in the exception message
     *  @throws NoSuchElementException always
     */
    override def apply(key: Any) = throw new NoSuchElementException("key not found: " + key)
    /** Returns `false` for every key: the empty map holds no keys.
     *
     *  @param key the key to test; never used
     */
    override def contains(key: Any) = false
    /** Returns `None` for every key: the empty map holds no keys.
     *
     *  @param key the key to look up; never used
     */
    def get(key: Any): Option[Nothing] = None
    /** Returns `default`: the empty map holds no keys.
     *
     *  @tparam V1 the result type
     *  @param key the key to look up; never used
     *  @param default the value returned for every key
     */
    override def getOrElse [V1](key: Any, default: => V1): V1 = default
    /** Returns the empty iterator. */
    def iterator: Iterator[(Any, Nothing)] = Iterator.empty
    /** Returns a map containing the single binding `key -> value`.
     *
     *  @tparam V1 the value type of the returned map
     *  @param key the key to add
     *  @param value the value to associate with `key`
     *  @return a new one-entry `SeqMap`
     */
    def updated [V1] (key: Any, value: V1): SeqMap[Any, V1] = new SeqMap1(key, value)
    /** Returns this map itself: the empty map holds no binding to remove.
     *
     *  @param key the key to remove; never used
     */
    def removed(key: Any): SeqMap[Any, Nothing] = this
  }

  @SerialVersionUID(3L)
  private[immutable] final class SeqMap1[K, +V](key1: K, value1: V) extends SeqMap[K,V] with Serializable {
    /** Returns 1, the number of key-value pairs in this map. */
    override def size: Int = 1
    /** Returns 1: the size of this map is always known. */
    override def knownSize: Int = 1
    /** Returns the value associated with `key`.
     *
     *  @param key the key to look up
     *  @throws NoSuchElementException if `key` is not the single key of this map
     */
    override def apply(key: K) = if (key == key1) value1 else throw new NoSuchElementException("key not found: " + key)
    /** Returns `true` if `key` equals the single key of this map, `false` otherwise.
     *
     *  @param key the key to test
     */
    override def contains(key: K) = key == key1
    /** Returns the value associated with `key`, if present.
     *
     *  @param key the key to look up
     *  @return `Some` of the value bound to `key`, or `None` if `key` is not in this map
     */
    def get(key: K): Option[V] =
      if (key == key1) Some(value1) else None
    /** Returns the value associated with `key`, or `default` if `key` is not in this map.
     *
     *  @tparam V1 the result type, a supertype of this map's value type
     *  @param key the key to look up
     *  @param default the value to return if `key` is absent; evaluated only in that case
     */
    override def getOrElse [V1 >: V](key: K, default: => V1): V1 =
      if (key == key1) value1 else default
    /** Returns an iterator producing the single key-value pair of this map. */
    def iterator = Iterator.single((key1, value1))
    /** Returns a map with `key` bound to `value`.
     *
     *  If `key` equals the single key of this map, returns a one-entry map with the
     *  new value; otherwise returns a two-entry map with the new binding appended.
     *
     *  @tparam V1 the value type of the returned map, a supertype of `V`
     *  @param key the key to add or update
     *  @param value the value to associate with `key`
     *  @return a `SeqMap` containing the bindings of this map and `key -> value`
     */
    def updated[V1 >: V](key: K, value: V1): SeqMap[K, V1] =
      if (key == key1) new SeqMap1(key1, value)
      else new SeqMap2(key1, value1, key, value)
    /** Returns a map without a binding for `key`.
     *
     *  If `key` equals the single key of this map, returns the empty `SeqMap`;
     *  otherwise returns this map itself.
     *
     *  @param key the key to remove
     */
    def removed(key: K): SeqMap[K, V] =
      if (key == key1) SeqMap.empty else this
    /** Applies `f` to the single key-value pair of this map.
     *
     *  @tparam U the result type of `f`; the result is discarded
     *  @param f the function applied to the key-value pair
     */
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1))
    }
    /** Applies `f` to the single key and value of this map, passed as two arguments.
     *
     *  @tparam U the result type of `f`; the result is discarded
     *  @param f the function applied to the key and its value
     */
    override def foreachEntry[U](f: (K, V) => U): Unit = {
      f(key1, value1)
    }
  }

  @SerialVersionUID(3L)
  private[immutable] final class SeqMap2[K, +V](key1: K, value1: V, key2: K, value2: V) extends SeqMap[K,V] with Serializable {
    /** Returns 2, the number of key-value pairs in this map. */
    override def size: Int = 2
    /** Returns 2: the size of this map is always known. */
    override def knownSize: Int = 2
    /** Returns the value associated with `key`.
     *
     *  @param key the key to look up
     *  @throws NoSuchElementException if `key` equals neither of the two keys of this map
     */
    override def apply(key: K) =
      if (key == key1) value1
      else if (key == key2) value2
      else throw new NoSuchElementException("key not found: " + key)
    /** Returns `true` if `key` equals one of the two keys of this map, `false` otherwise.
     *
     *  @param key the key to test
     */
    override def contains(key: K) = (key == key1) || (key == key2)
    /** Returns the value associated with `key`, if present.
     *
     *  @param key the key to look up
     *  @return `Some` of the value bound to `key`, or `None` if `key` is not in this map
     */
    def get(key: K): Option[V] =
      if (key == key1) Some(value1)
      else if (key == key2) Some(value2)
      else None
    /** Returns the value associated with `key`, or `default` if `key` is not in this map.
     *
     *  @tparam V1 the result type, a supertype of this map's value type
     *  @param key the key to look up
     *  @param default the value to return if `key` is absent; evaluated only in that case
     */
    override def getOrElse [V1 >: V](key: K, default: => V1): V1 =
      if (key == key1) value1
      else if (key == key2) value2
      else default
    /** Returns an iterator over the two key-value pairs of this map, in insertion order. */
    def iterator = ((key1, value1) :: (key2, value2) :: Nil).iterator
    /** Returns a map with `key` bound to `value`.
     *
     *  If `key` equals one of the two keys of this map, the returned map has the new
     *  value at that key's existing position; otherwise returns a three-entry map with
     *  the new binding appended.
     *
     *  @tparam V1 the value type of the returned map, a supertype of `V`
     *  @param key the key to add or update
     *  @param value the value to associate with `key`
     *  @return a `SeqMap` containing the bindings of this map and `key -> value`
     */
    def updated[V1 >: V](key: K, value: V1): SeqMap[K, V1] =
      if (key == key1) new SeqMap2(key1, value, key2, value2)
      else if (key == key2) new SeqMap2(key1, value1, key2, value)
      else new SeqMap3(key1, value1, key2, value2, key, value)
    /** Returns a map without a binding for `key`.
     *
     *  If `key` equals one of the two keys of this map, returns a one-entry map with
     *  the remaining binding; otherwise returns this map itself.
     *
     *  @param key the key to remove
     */
    def removed(key: K): SeqMap[K, V] =
      if (key == key1) new SeqMap1(key2, value2)
      else if (key == key2) new SeqMap1(key1, value1)
      else this
    /** Applies `f` to each key-value pair of this map, in insertion order.
     *
     *  @tparam U the result type of `f`; the results are discarded
     *  @param f the function applied to each key-value pair
     */
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1)); f((key2, value2))
    }
    /** Applies `f` to each key and value of this map, passed as two arguments, in insertion order.
     *
     *  @tparam U the result type of `f`; the results are discarded
     *  @param f the function applied to each key and its value
     */
    override def foreachEntry[U](f: (K, V) => U): Unit = {
      f(key1, value1)
      f(key2, value2)
    }
  }

  @SerialVersionUID(3L)
  private[immutable] class SeqMap3[K, +V](key1: K, value1: V, key2: K, value2: V, key3: K, value3: V) extends SeqMap[K,V] with Serializable {
    /** Returns 3, the number of key-value pairs in this map. */
    override def size: Int = 3
    /** Returns 3: the size of this map is always known. */
    override def knownSize: Int = 3
    /** Returns the value associated with `key`.
     *
     *  @param key the key to look up
     *  @throws NoSuchElementException if `key` equals none of the three keys of this map
     */
    override def apply(key: K) =
      if (key == key1) value1
      else if (key == key2) value2
      else if (key == key3) value3
      else throw new NoSuchElementException("key not found: " + key)
    /** Returns `true` if `key` equals one of the three keys of this map, `false` otherwise.
     *
     *  @param key the key to test
     */
    override def contains(key: K) = (key == key1) || (key == key2) || (key == key3)
    /** Returns the value associated with `key`, if present.
     *
     *  @param key the key to look up
     *  @return `Some` of the value bound to `key`, or `None` if `key` is not in this map
     */
    def get(key: K): Option[V] =
      if (key == key1) Some(value1)
      else if (key == key2) Some(value2)
      else if (key == key3) Some(value3)
      else None
    /** Returns the value associated with `key`, or `default` if `key` is not in this map.
     *
     *  @tparam V1 the result type, a supertype of this map's value type
     *  @param key the key to look up
     *  @param default the value to return if `key` is absent; evaluated only in that case
     */
    override def getOrElse [V1 >: V](key: K, default: => V1): V1 =
      if (key == key1) value1
      else if (key == key2) value2
      else if (key == key3) value3
      else default
    /** Returns an iterator over the three key-value pairs of this map, in insertion order. */
    def iterator = ((key1, value1) :: (key2, value2) :: (key3, value3) :: Nil).iterator
    /** Returns a map with `key` bound to `value`.
     *
     *  If `key` equals one of the three keys of this map, the returned map has the new
     *  value at that key's existing position; otherwise returns a four-entry map with
     *  the new binding appended.
     *
     *  @tparam V1 the value type of the returned map, a supertype of `V`
     *  @param key the key to add or update
     *  @param value the value to associate with `key`
     *  @return a `SeqMap` containing the bindings of this map and `key -> value`
     */
    def updated[V1 >: V](key: K, value: V1): SeqMap[K, V1] =
      if (key == key1)      new SeqMap3(key1, value, key2, value2, key3, value3)
      else if (key == key2) new SeqMap3(key1, value1, key2, value, key3, value3)
      else if (key == key3) new SeqMap3(key1, value1, key2, value2, key3, value)
      else new SeqMap4(key1, value1, key2, value2, key3, value3, key, value)
    /** Returns a map without a binding for `key`.
     *
     *  If `key` equals one of the three keys of this map, returns a two-entry map with
     *  the remaining bindings in their original order; otherwise returns this map itself.
     *
     *  @param key the key to remove
     */
    def removed(key: K): SeqMap[K, V] =
      if (key == key1)      new SeqMap2(key2, value2, key3, value3)
      else if (key == key2) new SeqMap2(key1, value1, key3, value3)
      else if (key == key3) new SeqMap2(key1, value1, key2, value2)
      else this
    /** Applies `f` to each key-value pair of this map, in insertion order.
     *
     *  @tparam U the result type of `f`; the results are discarded
     *  @param f the function applied to each key-value pair
     */
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1)); f((key2, value2)); f((key3, value3))
    }
    /** Applies `f` to each key and value of this map, passed as two arguments, in insertion order.
     *
     *  @tparam U the result type of `f`; the results are discarded
     *  @param f the function applied to each key and its value
     */
    override def foreachEntry[U](f: (K, V) => U): Unit = {
      f(key1, value1)
      f(key2, value2)
      f(key3, value3)
    }
  }

  @SerialVersionUID(3L)
  private[immutable] final class SeqMap4[K, +V](key1: K, value1: V, key2: K, value2: V, key3: K, value3: V, key4: K, value4: V) extends SeqMap[K,V] with Serializable {
    /** Returns 4, the number of key-value pairs in this map. */
    override def size: Int = 4
    /** Returns 4: the size of this map is always known. */
    override def knownSize: Int = 4
    /** Returns the value associated with `key`.
     *
     *  @param key the key to look up
     *  @throws NoSuchElementException if `key` equals none of the four keys of this map
     */
    override def apply(key: K) =
      if (key == key1) value1
      else if (key == key2) value2
      else if (key == key3) value3
      else if (key == key4) value4
      else throw new NoSuchElementException("key not found: " + key)
    /** Returns `true` if `key` equals one of the four keys of this map, `false` otherwise.
     *
     *  @param key the key to test
     */
    override def contains(key: K) = (key == key1) || (key == key2) || (key == key3) || (key == key4)
    /** Returns the value associated with `key`, if present.
     *
     *  @param key the key to look up
     *  @return `Some` of the value bound to `key`, or `None` if `key` is not in this map
     */
    def get(key: K): Option[V] =
      if (key == key1) Some(value1)
      else if (key == key2) Some(value2)
      else if (key == key3) Some(value3)
      else if (key == key4) Some(value4)
      else None
    /** Returns the value associated with `key`, or `default` if `key` is not in this map.
     *
     *  @tparam V1 the result type, a supertype of this map's value type
     *  @param key the key to look up
     *  @param default the value to return if `key` is absent; evaluated only in that case
     */
    override def getOrElse [V1 >: V](key: K, default: => V1): V1 =
      if (key == key1) value1
      else if (key == key2) value2
      else if (key == key3) value3
      else if (key == key4) value4
      else default
    /** Returns an iterator over the four key-value pairs of this map, in insertion order. */
    def iterator = ((key1, value1) :: (key2, value2) :: (key3, value3) :: (key4, value4) :: Nil).iterator
    /** Returns a map with `key` bound to `value`.
     *
     *  If `key` equals one of the four keys of this map, the returned map has the new
     *  value at that key's existing position; otherwise returns a [[VectorMap]] of the
     *  five bindings, with the new binding appended.
     *
     *  @tparam V1 the value type of the returned map, a supertype of `V`
     *  @param key the key to add or update
     *  @param value the value to associate with `key`
     *  @return a `SeqMap` containing the bindings of this map and `key -> value`
     */
    def updated[V1 >: V](key: K, value: V1): SeqMap[K, V1] =
      if (key == key1)      new SeqMap4(key1, value, key2, value2, key3, value3, key4, value4)
      else if (key == key2) new SeqMap4(key1, value1, key2, value, key3, value3, key4, value4)
      else if (key == key3) new SeqMap4(key1, value1, key2, value2, key3, value, key4, value4)
      else if (key == key4) new SeqMap4(key1, value1, key2, value2, key3, value3, key4, value)
      else {
        // Directly create the elements for performance reasons
        val fields = Vector(key1, key2, key3, key4, key)
        val underlying: Map[K, (Int, V1)] =
          HashMap(
            (key1, (0, value1)),
            (key2, (1, value2)),
            (key3, (2, value3)),
            (key4, (3, value4)),
            (key, (4, value))
          )
        new VectorMap(fields, underlying)
      }
    /** Returns a map without a binding for `key`.
     *
     *  If `key` equals one of the four keys of this map, returns a three-entry map with
     *  the remaining bindings in their original order; otherwise returns this map itself.
     *
     *  @param key the key to remove
     */
    def removed(key: K): SeqMap[K, V] =
      if (key == key1)      new SeqMap3(key2, value2, key3, value3, key4, value4)
      else if (key == key2) new SeqMap3(key1, value1, key3, value3, key4, value4)
      else if (key == key3) new SeqMap3(key1, value1, key2, value2, key4, value4)
      else if (key == key4) new SeqMap3(key1, value1, key2, value2, key3, value3)
      else this
    /** Applies `f` to each key-value pair of this map, in insertion order.
     *
     *  @tparam U the result type of `f`; the results are discarded
     *  @param f the function applied to each key-value pair
     */
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1)); f((key2, value2)); f((key3, value3)); f((key4, value4))
    }
    /** Applies `f` to each key and value of this map, passed as two arguments, in insertion order.
     *
     *  @tparam U the result type of `f`; the results are discarded
     *  @param f the function applied to each key and its value
     */
    override def foreachEntry[U](f: (K, V) => U): Unit = {
      f(key1, value1)
      f(key2, value2)
      f(key3, value3)
      f(key4, value4)
    }

    private[SeqMap] def buildTo[V1 >: V](builder: Builder[(K, V1), SeqMap[K, V1]]): builder.type =
      builder.addOne((key1, value1)).addOne((key2, value2)).addOne((key3, value3)).addOne((key4, value4))
  }

  private final class SeqMapBuilderImpl[K, V] extends ReusableBuilder[(K, V), SeqMap[K, V]] {
    private var elems: SeqMap[K, V] = SeqMap.empty
    private var switchedToVectorMapBuilder: Boolean = false
    private var vectorMapBuilder: VectorMapBuilder[K, V] = compiletime.uninitialized

    /** Clears the contents of this builder, resetting it to the empty map.
     *
     *  Also clears the underlying `VectorMap` builder, if one was ever allocated,
     *  and reverts to the compact small-map representation.
     */
    override def clear(): Unit = {
      elems = SeqMap.empty
      if (vectorMapBuilder != null) {
        vectorMapBuilder.clear()
      }
      switchedToVectorMapBuilder = false
    }

    /** Returns the `SeqMap` built so far: a compact small map for up to four distinct
     *  keys, or the result of the underlying `VectorMap` builder beyond that.
     */
    override def result(): SeqMap[K, V] =
      if (switchedToVectorMapBuilder) vectorMapBuilder.result() else elems

    /** Adds the key-value pair `elem` to this builder and returns this builder.
     *
     *  While at most four distinct keys have been added, the pairs are held in a
     *  compact small map; the fifth distinct key switches this builder to an
     *  underlying `VectorMap` builder. Adding a key already present replaces its
     *  value but keeps its position.
     *
     *  @param elem the key-value pair to add
     */
    def addOne(elem: (K, V)) = {
      if (switchedToVectorMapBuilder) {
        vectorMapBuilder.addOne(elem)
      } else if (elems.size < 4) {
        elems = elems + elem
      } else {
        // assert(elems.size == 4)
        if (elems.contains(elem._1)) {
          elems = elems + elem // will not increase the size of the map
        } else {
          switchedToVectorMapBuilder = true
          if (vectorMapBuilder == null) {
            vectorMapBuilder = new VectorMapBuilder
          }
          elems.asInstanceOf[SeqMap4[K, V]].buildTo(vectorMapBuilder)
          vectorMapBuilder.addOne(elem)
        }
      }

      this
    }

    /** Adds all key-value pairs of `xs` to this builder.
     *
     *  @param xs the key-value pairs to add
     *  @return this builder
     */
    override def addAll(xs: IterableOnce[(K, V)]^): this.type =
      if (switchedToVectorMapBuilder) {
        vectorMapBuilder.addAll(xs)
        this
      } else {
        super.addAll(xs)
      }
  }
}
