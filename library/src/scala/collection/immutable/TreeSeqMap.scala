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

import scala.annotation.tailrec

/** This class implements an immutable map that preserves order using
  * a hash map for the key to value mapping to provide efficient lookup,
  * and a tree for the ordering of the keys to provide efficient
  * insertion/modification order traversal and destructuring.
  *
  * By default insertion order (`TreeSeqMap.OrderBy.Insertion`)
  * is used, but modification order (`TreeSeqMap.OrderBy.Modification`)
  * can be used instead if so specified at creation.
  *
  * The `orderingBy(orderBy: TreeSeqMap.OrderBy): TreeSeqMap[K, V]` method
  * can be used to switch to the specified ordering for the returned map.
  *
  * A key can be manually refreshed (i.e. placed at the end) via the
  * `refresh(key: K): TreeSeqMap[K, V]` method (regardless of the ordering in
  * use).
  *
  * Internally, an ordinal counter is increased for each insertion/modification
  * and then the current ordinal is used as key in the tree map. After 2<sup>32</sup>
  * insertions/modifications the entire map is copied (thus resetting the ordinal
  * counter).
  *
  *  @tparam K the type of the keys contained in this map.
  *  @tparam V the type of the values associated with the keys in this map.
  * @define coll immutable tree seq map
  * @define Coll `immutable.TreeSeqMap`
  */
final class TreeSeqMap[K, +V] private (
    private val ordering: TreeSeqMap.Ordering[K],
    private val mapping: TreeSeqMap.Mapping[K, V],
    private val ordinal: Int,
    /** The ordering this map maintains for traversal: by insertion or by modification. */
    val orderedBy: TreeSeqMap.OrderBy)
  extends AbstractMap[K, V]
    with SeqMap[K, V]
    with MapOps[K, V, TreeSeqMap, TreeSeqMap[K, V]]
    with StrictOptimizedIterableOps[(K, V), Iterable, TreeSeqMap[K, V]]
    with StrictOptimizedMapOps[K, V, TreeSeqMap, TreeSeqMap[K, V]]
    with MapFactoryDefaults[K, V, TreeSeqMap, Iterable] {

  import TreeSeqMap._

  /** The name of this collection class, used as the prefix in its `toString` representation. */
  override protected def className: String = "TreeSeqMap"

  /** Returns the [[TreeSeqMap$ TreeSeqMap]] companion object as the factory for maps of this kind. */
  override def mapFactory: MapFactory[TreeSeqMap] = TreeSeqMap

  /** The number of key-value pairs in this map. */
  override val size = mapping.size

  /** Returns the size of this map: the size is always known. */
  override def knownSize: Int = size

  /** Returns `true` if this map contains no key-value pairs, `false` otherwise. */
  override def isEmpty = size == 0

  /*
  // This should have been overridden in 2.13.0 but wasn't so it will have to wait since it is not forwards compatible
  // Now handled in inherited method from scala.collection.MapFactoryDefaults instead.
  override def empty = TreeSeqMap.empty[K, V](orderedBy)
  */

  /** Returns a map with the same bindings that maintains the given ordering.
   *
   *  Positions already assigned to the existing bindings are kept; only the
   *  policy for future updates changes. Returns this map itself if it is
   *  already ordered by `orderBy`.
   *
   *  @param orderBy whether the returned map orders its entries by insertion or
   *                 by modification
   *  @return a map with the same bindings, ordered by `orderBy`
   */
  def orderingBy(orderBy: OrderBy): TreeSeqMap[K, V] = {
    if (orderBy == this.orderedBy) this
    else if (isEmpty) TreeSeqMap.empty(orderBy)
    else new TreeSeqMap(ordering, mapping, ordinal, orderBy)
  }

  /** Returns a map with `key` bound to `value`, replacing any existing binding
   *  for `key`.
   *
   *  Under insertion ordering an already-present key keeps its position; under
   *  modification ordering it moves to the end. A new key is placed at the end
   *  in either mode. When the ordinal counter is exhausted (after 2^32
   *  insertions/modifications) and a fresh position is needed, the whole map is
   *  rebuilt first, which is expensive but rare.
   *
   *  @tparam V1 the type of the values in the resulting map, a supertype of this
   *             map's value type
   *  @param key the key to add or update
   *  @param value the value to associate with `key`
   *  @return a map containing the bindings of this map and the binding of `key`
   *          to `value`
   */
  def updated[V1 >: V](key: K, value: V1): TreeSeqMap[K, V1] = {
    mapping.get(key) match {
      case e if ordinal == -1 && (orderedBy == OrderBy.Modification || e.isEmpty) =>
        // Reinsert into fresh instance to restart ordinal counting, expensive but only done after 2^32 updates.
        TreeSeqMap.empty[K, V1](orderedBy) ++ this + (key -> value)
      case Some((o, _)) if orderedBy == OrderBy.Insertion =>
        new TreeSeqMap(
          ordering.include(o, key),
          mapping.updated[(Int, V1)](key, (o, value)),
          ordinal, // Do not increment the ordinal since the key is already present, i.e. o <= ordinal.
          orderedBy)
      case Some((o, _)) =>
        val o1 = increment(ordinal)
        new TreeSeqMap(
          ordering.exclude(o).append(o1, key),
          mapping.updated[(Int, V1)](key, (o1, value)),
          o1,
          orderedBy)
      case None =>
        val o1 = increment(ordinal)
        new TreeSeqMap(
          ordering.append(o1, key),
          mapping.updated[(Int, V1)](key, (o1, value)),
          o1,
          orderedBy)
    }
  }

  /** Returns a map without any binding for the given key, or this map itself if
   *  the key is not present.
   *
   *  The relative order of the remaining entries is unchanged.
   *
   *  @param key the key to remove
   *  @return a map containing the bindings of this map except for `key`
   */
  def removed(key: K): TreeSeqMap[K, V] = {
    mapping.get(key) match {
      case Some((o, _)) =>
        new TreeSeqMap(
          ordering.exclude(o),
          mapping.removed(key),
          ordinal,
          orderedBy)
      case None =>
        this
    }
  }

  /** Returns a map in which the given key is moved to the end of the traversal
   *  order, keeping its value, regardless of the ordering mode in use.
   *
   *  Returns this map itself if the key is not present.
   *
   *  @param key the key to move to the end
   *  @return a map with the same bindings, with `key` traversed last
   */
  def refresh(key: K): TreeSeqMap[K, V] = {
    mapping.get(key) match {
      case Some((o, _)) =>
        val o1 = increment(ordinal)
        new TreeSeqMap(
          ordering.exclude(o).append(o1, key),
          mapping,
          o1,
          orderedBy)
      case None =>
        this
    }
  }

  /** Optionally returns the value associated with the given key, looked up in
   *  the underlying hash map without traversing the ordering.
   *
   *  @param key the key to look up
   *  @return `Some(value)` if this map binds `key` to `value`, `None` otherwise
   */
  def get(key: K): Option[V] = mapping.get(key).map(value)

  /** Returns an iterator over the key/value pairs of this map in traversal
   *  (insertion or modification) order.
   */
  def iterator: Iterator[(K, V)] = new AbstractIterator[(K, V)] {
    private val iter = ordering.iterator

    override def hasNext: Boolean = iter.hasNext

    override def next(): (K, V) = binding(iter.next())
  }

  /** Returns an iterator over the keys of this map in traversal (insertion or
   *  modification) order.
   */
  override def keysIterator: Iterator[K] = new AbstractIterator[K] {
    private val iter = ordering.iterator

    override def hasNext: Boolean = iter.hasNext

    override def next(): K = iter.next()
  }

  /** Returns an iterator over the values of this map in traversal (insertion or
   *  modification) order of their keys.
   */
  override def valuesIterator: Iterator[V] = new AbstractIterator[V] {
    private val iter = ordering.iterator

    override def hasNext: Boolean = iter.hasNext

    override def next(): V = value(binding(iter.next()))
  }

  /** Returns `true` if this map has a binding for the given key, checked in the
   *  underlying hash map without traversing the ordering.
   *
   *  @param key the key to test
   *  @return `true` if `key` is bound in this map, `false` otherwise
   */
  override def contains(key: K): Boolean = mapping.contains(key)

  /** Returns the first key/value pair in traversal order.
   *
   *  @throws NoSuchElementException if this map is empty
   */
  override def head: (K, V) = binding(ordering.head)

  /** Optionally returns the first key/value pair in traversal order, or `None`
   *  if this map is empty.
   */
  override def headOption = ordering.headOption.map(binding)

  /** Returns the last key/value pair in traversal order.
   *
   *  @throws NoSuchElementException if this map is empty
   */
  override def last: (K, V) = binding(ordering.last)

  /** Optionally returns the last key/value pair in traversal order, or `None`
   *  if this map is empty.
   */
  override def lastOption: Option[(K, V)] = ordering.lastOption.map(binding)

  /** Returns a map without the first entry in traversal order.
   *
   *  @throws NoSuchElementException if this map is empty
   */
  override def tail: TreeSeqMap[K, V] = {
    val (head, tail) = ordering.headTail
    new TreeSeqMap(tail, mapping.removed(head), ordinal, orderedBy)
  }

  /** Returns a map without the last entry in traversal order.
   *
   *  @throws NoSuchElementException if this map is empty
   */
  override def init: TreeSeqMap[K, V] = {
    val (init, last) = ordering.initLast
    new TreeSeqMap(init, mapping.removed(last), ordinal, orderedBy)
  }

  /** Returns a map with the entries at positions `from` until `until` (in
   *  traversal order), keeping their relative order.
   *
   *  Out-of-range arguments are clamped: a negative `from` counts as 0 and an
   *  `until` beyond the size as the size; an empty range yields an empty map.
   *
   *  @param from the position of the first entry to keep
   *  @param until the position one past the last entry to keep
   *  @return a map containing the entries in the given position range
   */
  override def slice(from: Int, until: Int): TreeSeqMap[K, V] = {
    val sz = size
    if (sz == 0 || from >= until) TreeSeqMap.empty[K, V](orderedBy)
    else {
      val sz = size
      val f = if (from >= 0) from else 0
      val u = if (until <= sz) until else sz
      val l = u - f
      if (l <= 0) TreeSeqMap.empty[K, V](orderedBy)
      else if (l > sz / 2) {
        // Remove front and rear incrementally if majority of elements are to be kept
        val (front, rest) = ordering.splitAt(f)
        val (ong, rear) = rest.splitAt(l)
        var mng = this.mapping
        val frontIter = front.iterator
        while (frontIter.hasNext) {
          mng = mng - frontIter.next()
        }
        val rearIter = rear.iterator
        while (rearIter.hasNext) {
          mng = mng - rearIter.next()
        }
        new TreeSeqMap(ong, mng, ordinal, orderedBy)
      } else {
        // Populate with builder otherwise
        val bdr = newBuilder[K, V](orderedBy)
        val iter = ordering.iterator
        var i = 0
        while (i < f) {
          iter.next()
          i += 1
        }
        while (i < u) {
          val k = iter.next()
          bdr.addOne((k, mapping(k)._2))
          i += 1
        }
        bdr.result()
      }
    }
  }

  /** Returns a map built by applying `f` to each key/value pair of this map, in
   *  traversal order.
   *
   *  The result keeps this map's ordering mode; if `f` produces the same key
   *  more than once, the binding produced later overwrites the earlier one.
   *
   *  @tparam K2 the type of the keys in the resulting map
   *  @tparam V2 the type of the values in the resulting map
   *  @param f the function applied to each key/value pair
   *  @return a map containing the transformed pairs
   */
  override def map[K2, V2](f: ((K, V)) => (K2, V2)): TreeSeqMap[K2, V2] = {
    val bdr = newBuilder[K2, V2](orderedBy)
    val iter = ordering.iterator
    while (iter.hasNext) {
      val k = iter.next()
      val (_, v) = mapping(k)
      val (k2, v2) = f((k, v))
      bdr.addOne((k2, v2))
    }
    bdr.result()
  }

  /** Returns a map built by applying `f` to each key/value pair of this map, in
   *  traversal order, and collecting all the pairs it produces.
   *
   *  The result keeps this map's ordering mode; if the same key is produced
   *  more than once, the binding produced later overwrites the earlier one.
   *
   *  @tparam K2 the type of the keys in the resulting map
   *  @tparam V2 the type of the values in the resulting map
   *  @param f the function returning a collection of key/value pairs for each
   *           pair of this map
   *  @return a map containing all the pairs produced by `f`
   */
  override def flatMap[K2, V2](f: ((K, V)) => IterableOnce[(K2, V2)]^): TreeSeqMap[K2, V2] = {
    val bdr = newBuilder[K2, V2](orderedBy)
    val iter = ordering.iterator
    while (iter.hasNext) {
      val k = iter.next()
      val (_, v) = mapping(k)
      val jter = f((k, v)).iterator
      while (jter.hasNext) {
        val (k2, v2) = jter.next()
        bdr.addOne((k2, v2))
      }
    }
    bdr.result()
  }

  /** Returns a map built from the key/value pairs on which `pf` is defined,
   *  transformed by `pf`, in traversal order.
   *
   *  The result keeps this map's ordering mode; if `pf` produces the same key
   *  more than once, the binding produced later overwrites the earlier one.
   *
   *  @tparam K2 the type of the keys in the resulting map
   *  @tparam V2 the type of the values in the resulting map
   *  @param pf the partial function applied to each pair on which it is defined
   *  @return a map containing the transformed pairs
   */
  override def collect[K2, V2](pf: PartialFunction[(K, V), (K2, V2)]^): TreeSeqMap[K2, V2] = {
    val bdr = newBuilder[K2, V2](orderedBy)
    val iter = ordering.iterator
    while (iter.hasNext) {
      val k = iter.next()
      val (_, v) = mapping(k)
      pf.runWith({ case (k2, v2) => bdr.addOne((k2, v2)) })((k, v))
    }
    bdr.result()
  }

  /** Returns a map containing the bindings of this map followed by those of
   *  `suffix`.
   *
   *  Pairs of `suffix` are added under this map's ordering mode: a key already
   *  present keeps its position under insertion ordering (its value updated) and
   *  moves to the end under modification ordering; new keys are appended in the
   *  order produced by `suffix`.
   *
   *  @tparam V2 the type of the values in the resulting map, a supertype of this
   *             map's value type
   *  @param suffix the collection of key/value pairs to add
   *  @return a map containing the combined bindings
   */
  override def concat[V2 >: V](suffix: IterableOnce[(K, V2)]^): TreeSeqMap[K, V2] = {
    var ong: Ordering[K] = ordering
    var mng: Mapping[K, V2] = mapping
    var ord = increment(ordinal)
    val iter = suffix.iterator
    while (iter.hasNext) {
      val (k, v2) = iter.next()
      mng.get(k) match {
        case Some((o, v)) =>
          if (orderedBy == OrderBy.Insertion && v != v2) mng = mng.updated(k, (o, v2))
          else if (orderedBy == OrderBy.Modification) {
            mng = mng.updated(k, (ord, v2))
            ong = ong.exclude(o).append(ord, k)
            ord = increment(ord)
          }
        case None =>
          mng = mng.updated(k, (ord, v2))
          ong = ong.append(ord, k)
          ord = increment(ord)
      }
    }
    new TreeSeqMap[K, V2](ong, mng, ord, orderedBy)
  }

  @`inline` private def value(p: (?, V)) = p._2
  @`inline` private def binding(k: K) = mapping(k).copy(_1 = k)
}
object TreeSeqMap extends MapFactory[TreeSeqMap] {
  /** The ordering modes a [[TreeSeqMap]] can maintain: [[OrderBy.Insertion]]
   *  keeps a key at its original position when it is updated, while
   *  [[OrderBy.Modification]] moves an updated key to the end.
   */
  sealed trait OrderBy
  object OrderBy {
    case object Insertion extends OrderBy
    case object Modification extends OrderBy
  }

  private val EmptyByInsertion = new TreeSeqMap[Nothing, Nothing](Ordering.empty, HashMap.empty, 0, OrderBy.Insertion)
  private val EmptyByModification = new TreeSeqMap[Nothing, Nothing](Ordering.empty, HashMap.empty, 0, OrderBy.Modification)
  /** The empty map, ordered by insertion. */
  val Empty = EmptyByInsertion
  /** Returns the empty map, ordered by insertion.
   *
   *  @tparam K the type of the keys
   *  @tparam V the type of the values
   *  @return the empty `TreeSeqMap`
   */
  def empty[K, V]: TreeSeqMap[K, V] = empty(OrderBy.Insertion)
  /** Returns the empty map with the given ordering mode.
   *
   *  @tparam K the type of the keys
   *  @tparam V the type of the values
   *  @param orderBy whether the map orders its entries by insertion or by
   *                 modification
   *  @return the empty `TreeSeqMap` ordered by `orderBy`
   */
  def empty[K, V](orderBy: OrderBy): TreeSeqMap[K, V] = {
    if (orderBy == OrderBy.Modification) EmptyByModification
    else EmptyByInsertion
  }.asInstanceOf[TreeSeqMap[K, V]]

  /** Returns a `TreeSeqMap`, ordered by insertion, containing the key/value
   *  pairs of the given collection.
   *
   *  Returns the collection itself if it is already a `TreeSeqMap`; otherwise
   *  the pairs are inserted in iteration order, later pairs overwriting earlier
   *  ones with the same key.
   *
   *  @tparam K the type of the keys
   *  @tparam V the type of the values
   *  @param it the collection of key/value pairs
   *  @return a `TreeSeqMap` containing the pairs of `it`
   */
  def from[K, V](it: collection.IterableOnce[(K, V)]^): TreeSeqMap[K, V] =
    (it: @unchecked) match {
      case om: TreeSeqMap[K, V] => om
      case _ => (newBuilder[K, V] ++= it).result()
    }

  @inline private def increment(ord: Int) = if (ord == Int.MaxValue) Int.MinValue else ord + 1

  /** Returns a builder for a `TreeSeqMap` ordered by insertion.
   *
   *  @tparam K the type of the keys
   *  @tparam V the type of the values
   *  @return a fresh builder
   */
  def newBuilder[K, V]: mutable.Builder[(K, V), TreeSeqMap[K, V]] = newBuilder(OrderBy.Insertion)
  /** Returns a builder for a `TreeSeqMap` with the given ordering mode.
   *
   *  @tparam K the type of the keys
   *  @tparam V the type of the values
   *  @param orderedBy whether the built map orders its entries by insertion or
   *                   by modification
   *  @return a fresh builder
   */
  def newBuilder[K, V](orderedBy: OrderBy): mutable.Builder[(K, V), TreeSeqMap[K, V]] = new Builder[K, V](orderedBy)

  /** A builder for [[TreeSeqMap]], accumulating entries with the given ordering
   *  mode.
   *
   *  After `result()` has been called, further additions go through the built
   *  map's functional `updated`, so the builder remains usable.
   *
   *  @tparam K the type of the keys
   *  @tparam V the type of the values
   *  @param orderedBy whether the built map orders its entries by insertion or
   *                   by modification
   */
  final class Builder[K, V](orderedBy: OrderBy) extends mutable.Builder[(K, V), TreeSeqMap[K, V]] {
    private val bdr = new MapBuilderImpl[K, (Int, V)]
    private var ong = Ordering.empty[K]
    private var ord = 0
    @annotation.stableNull
    private var aliased: TreeSeqMap[K, V] | Null = null

    /** Adds the given key/value pair to this builder.
     *
     *  @param elem the key/value pair to add
     *  @return this builder
     */
    override def addOne(elem: (K, V)): this.type = addOne(elem._1, elem._2)
    /** Adds a binding of `key` to `value` to this builder.
     *
     *  Under insertion ordering an already-added key keeps its position (its
     *  value updated); under modification ordering it moves to the end.
     *
     *  @param key the key to add or update
     *  @param value the value to associate with `key`
     *  @return this builder
     */
    def addOne(key: K, value: V): this.type = {
      if (aliased ne null) {
        aliased = aliased.updated(key, value)
      } else {
        bdr.getOrElse(key, null) match {
          case (o, v) =>
            if (orderedBy == OrderBy.Insertion && v != value) bdr.addOne(key, (o, value))
            else if (orderedBy == OrderBy.Modification) {
              bdr.addOne(key, (ord, value))
              ong = ong.exclude(o).appendInPlace(ord, key)
              ord = increment(ord)
            }
          case null =>
            bdr.addOne(key, (ord, value))
            ong = ong.appendInPlace(ord, key)
            ord = increment(ord)
        }
      }
      this
    }

    /** Clears the contents of this builder, making it ready to build a fresh
     *  map.
     */
    override def clear(): Unit = {
      ong = Ordering.empty
      ord = 0
      bdr.clear()
      aliased = null
    }

    /** Returns the map built from the entries added so far.
     *
     *  Repeated calls return the same map; additions made after this call go
     *  through that map's functional `updated`.
     */
    override def result(): TreeSeqMap[K, V] = {
      if (aliased eq null) {
        aliased = new TreeSeqMap(ong, bdr.result(), ord, orderedBy)
      }
      aliased
    }
  }

  private type Mapping[K, +V] = Map[K, (Int, V)]
  @annotation.unused
  private val Mapping = Map

  /* The ordering implementation below is an adapted version of immutable.IntMap. */
  private[immutable] object Ordering {
    import scala.collection.generic.BitOperations.Int.{Int => _, _}

    @inline private[immutable] def toBinaryString(i: Int): String = s"$i/${i.toBinaryString}"

    def empty[T] : Ordering[T] = Zero

    def apply[T](elems: (Int, T)*): Ordering[T] =
      elems.foldLeft(empty[T])((x, y) => x.include(y._1, y._2))

    // Iterator over a non-empty Ordering.
    final class Iterator[+V](it: Ordering[V]) {
      // Basically this uses a simple stack to emulate conversion over the tree. However
      // because we know that Ints are at least 32 bits we can have at most 32 Bins and
      // one Tip sitting on the tree at any point. Therefore we know the maximum stack
      // depth is 33
      private var index = 0
      private val buffer = new Array[AnyRef](33)

      private def pop = {
        index -= 1
        buffer(index).asInstanceOf[Ordering[V]]
      }

      private def push[V2 >: V](x: Ordering[V2]): Unit = {
        buffer(index) = x.asInstanceOf[AnyRef]
        index += 1
      }

      if (it != Zero) push(it)

      def hasNext = index > 0
      @tailrec
      def next(): V =
        if (!hasNext) scala.collection.Iterator.empty.next()
        else pop match {
          case Bin(_,_, Tip(_, v), right) =>
            push(right)
            v
          case Bin(_, _, left, right) =>
            push(right)
            push(left)
            next()
          case Tip(_, v) => v
          // This should never happen. We don't allow Ordering.Zero in subtrees of the Ordering
          // and don't return an Ordering.Iterator for Ordering.Zero.
          case Zero => throw new IllegalStateException("empty subtree not allowed")
        }
    }

    object Iterator {
      val Empty = new Iterator[Nothing](Ordering.empty[Nothing])
      def empty[V]: Iterator[V] = Empty.asInstanceOf[Iterator[V]]
    }

    case object Zero extends Ordering[Nothing] {
      // Important! Without this equals method in place, an infinite
      // loop from Map.equals => size => pattern-match-on-Nil => equals
      // develops.  Case objects and custom equality don't mix without
      // careful handling.
      override def equals(that : Any): Boolean = that match {
        case _: this.type => true
        case _: Ordering[?] => false // The only empty Orderings are eq Nil
        case _ => super.equals(that)
      }
      protected def format(sb: StringBuilder, prefix: String, subPrefix: String): Unit = sb ++= s"${prefix}Ø"
    }

    final case class Tip[+T](ord: Int, value: T) extends Ordering[T] {
      def withValue[S](s: S) =
        if (s.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) this.asInstanceOf[Tip[S]]
        else Tip(ord, s)
      protected def format(sb: StringBuilder, prefix: String, subPrefix: String): Unit = sb ++= s"${prefix}Tip(${toBinaryString(ord)} -> $value)\n"
    }

    final case class Bin[+T](prefix: Int, mask: Int, left: Ordering[T], var right: Ordering[T] @scala.annotation.unchecked.uncheckedVariance) extends Ordering[T] {
      def bin[S](left: Ordering[S], right: Ordering[S]): Ordering[S] = {
        if ((this.left eq left) && (this.right eq right)) this.asInstanceOf[Bin[S]]
        else Bin[S](prefix, mask, left, right)
      }
      protected def format(sb: StringBuilder, prefix: String, subPrefix: String): Unit = {
        sb ++= s"${prefix}Bin(${toBinaryString(this.prefix)}:${toBinaryString(mask)})\n"
        left.format(sb, subPrefix + "├── ", subPrefix + "│   ")
        right.format(sb, subPrefix + "└── ", subPrefix + "    ")
      }
    }

    private def branchMask(i: Int, j: Int) = highestOneBit(i ^ j)

    private def join[T](p1: Int, t1: Ordering[T], p2: Int, t2: Ordering[T]): Ordering[T] = {
      val m = branchMask(p1, p2)
      val p = mask(p1, m)
      if (zero(p1, m)) Bin(p, m, t1, t2)
      else Bin(p, m, t2, t1)
    }

    private def bin[T](prefix: Int, mask: Int, left: Ordering[T], right: Ordering[T]): Ordering[T] = (left, right) match {
      case (l, Zero) => l
      case (Zero, r) => r
      case (l, r) => Bin(prefix, mask, l, r)
    }
  }

  /** The modification-order trie of a [[TreeSeqMap]]: a big-endian patricia
   *  trie (an adapted version of [[IntMap]]) keyed by the map's ordinal
   *  counter, whose in-order traversal yields values in ascending unsigned
   *  ordinal order, i.e. oldest first.
   *
   *  Since ordinals are assigned by an increasing counter that wraps from
   *  `Int.MaxValue` to `Int.MinValue`, unsigned order coincides with
   *  assignment order.
   *
   *  @tparam T the type of the values (the map keys of the owning `TreeSeqMap`)
   */
  sealed abstract class Ordering[+T] {
    import Ordering._
    import scala.annotation.tailrec
    import scala.collection.generic.BitOperations.Int._

    /** Returns the multi-line debug rendering of this trie. */
    override final def toString(): String = format
    /** Returns a multi-line rendering of this trie, one node per line, drawn as
     *  a tree.
     */
    final def format: String = {
      val sb = new StringBuilder
      format(sb, "", "")
      sb.toString()
    }
    /** Appends this node's debug representation to `sb`.
     *
     *  @param sb the builder to append to
     *  @param prefix the prefix for this node's line
     *  @param subPrefix the prefix for child lines
     */
    protected def format(sb: StringBuilder, prefix: String, subPrefix: String): Unit

    /** Returns the value at the lowest ordinal, in unsigned order.
     *
     *  @throws NoSuchElementException if this ordering is empty
     */
    @tailrec
    final def head: T = this match {
      case Zero => throw new NoSuchElementException("head of empty map")
      case Tip(k, v) => v
      case Bin(_, _, l, _) => l.head
    }

    /** Optionally returns the value at the lowest ordinal, in unsigned order,
     *  or `None` if this ordering is empty.
     */
    @tailrec
    final def headOption: Option[T] = this match {
      case Zero => None
      case Tip(_, v) => Some(v)
      case Bin(_, _, l, _) => l.headOption
    }

    /** Returns the value at the highest ordinal, in unsigned order.
     *
     *  @throws NoSuchElementException if this ordering is empty
     */
    @tailrec
    final def last: T = this match {
      case Zero => throw new NoSuchElementException("last of empty map")
      case Tip(_, v) => v
      case Bin(_, _, _, r) => r.last
    }

    /** Optionally returns the value at the highest ordinal, in unsigned order,
     *  or `None` if this ordering is empty.
     */
    @tailrec
    final def lastOption: Option[T] = this match {
      case Zero => None
      case Tip(_, v) => Some(v)
      case Bin(_, _, _, r) => r.lastOption
    }

    /** Returns the highest ordinal in this ordering, in unsigned order, or 0 if
     *  it is empty.
     */
    @tailrec
    final def ordinal: Int = this match {
      case Zero => 0
      case Tip(o, _) => o
      case Bin(_, _, _, r) => r.ordinal
    }

    /** Returns an ordering without the entry at the lowest ordinal.
     *
     *  @throws NoSuchElementException if this ordering is empty
     */
    final def tail: Ordering[T] = this match {
      case Zero => throw new NoSuchElementException("tail of empty map")
      case Tip(_, _) => Zero
      case Bin(p, m, l, r) => bin(p, m, l.tail, r)
    }

    /** Returns the value at the lowest ordinal together with the ordering
     *  without that entry.
     *
     *  @throws NoSuchElementException if this ordering is empty
     */
    final def headTail: (T, Ordering[T]) = this match {
      case Zero => throw new NoSuchElementException("init of empty map")
      case Tip(_, v) => (v, Zero)
      case Bin(p, m, l, r) =>
        val (head, tail) = l.headTail
        (head, bin(p, m, tail, r))
    }

    /** Returns an ordering without the entry at the highest ordinal.
     *
     *  @throws NoSuchElementException if this ordering is empty
     */
    final def init: Ordering[T] = this match {
      case Zero => throw new NoSuchElementException("init of empty map")
      case Tip(_, _) => Zero
      case Bin(p, m, l, r) =>
        bin(p, m, l, r.init)
    }

    /** Returns the ordering without the entry at the highest ordinal together
     *  with the value of that entry.
     *
     *  @throws NoSuchElementException if this ordering is empty
     */
    final def initLast: (Ordering[T], T) = this match {
      case Zero => throw new NoSuchElementException("init of empty map")
      case Tip(_, v) => (Zero, v)
      case Bin(p, m, l, r) =>
        val (init, last) = r.initLast
        (bin(p, m, l, init), last)
    }

    /** Returns an iterator over the values of this ordering in ascending
     *  unsigned ordinal order.
     */
    final def iterator: Iterator[T] = this match {
      case Zero => Iterator.empty
      case _ => new Iterator(this)
    }

    /** Returns an ordering with `value` stored at `ordinal`, replacing any value
     *  already stored at that ordinal.
     *
     *  Unlike `append`, the ordinal may lie anywhere relative to the existing
     *  entries.
     *
     *  @tparam S the type of the values in the resulting ordering
     *  @param ordinal the ordinal to store at
     *  @param value the value to store
     *  @return an ordering containing the entries of this one and `value` at
     *          `ordinal`
     */
    final def include[S >: T](ordinal: Int, value: S): Ordering[S] = this match {
      case Zero =>
        Tip(ordinal, value)
      case Tip(o, _) =>
        if (ordinal == o) Tip(ordinal, value)
        else join(ordinal, Tip(ordinal, value), o, this)
      case Bin(p, m, l, r) =>
        if (!hasMatch(ordinal, p, m)) join(ordinal, Tip(ordinal, value), p, this)
        else if (zero(ordinal, m)) Bin(p, m, l.include(ordinal, value), r)
        else Bin(p, m, l, r.include(ordinal, value))
    }

    /** Returns an ordering with `value` stored at `ordinal`, which is meant to be
     *  greater (in unsigned order) than the current maximum ordinal.
     *
     *  That precondition is only partly enforced. A smaller `ordinal` throws only
     *  where it belongs on the low side of a `Bin` that already covers it; where it
     *  falls outside the prefix of a `Bin`, or where this ordering is a `Tip` or is
     *  empty, it is joined in like any other ordinal, and an ordinal equal to a
     *  `Tip`'s replaces its value.
     *
     *  @tparam S the type of the values in the resulting ordering
     *  @param ordinal the ordinal to store at
     *  @param value the value to store
     *  @return an ordering containing the entries of this one and `value` at
     *          `ordinal`
     *  @throws IllegalArgumentException if `ordinal` falls within the prefix of a
     *          `Bin` on the side already covered by its lower subtree
     */
    final def append[S >: T](ordinal: Int, value: S): Ordering[S] = this match {
      case Zero =>
        Tip(ordinal, value)
      case Tip(o, _) =>
        if (ordinal == o) Tip(ordinal, value)
        else join(ordinal, Tip(ordinal, value), o, this)
      case Bin(p, m, l, r) =>
        if (!hasMatch(ordinal, p, m)) join(ordinal, Tip(ordinal, value), p, this)
        else if (zero(ordinal, m)) throw new IllegalArgumentException(s"Append called with ordinal out of range: $ordinal is not greater than current max ordinal ${this.ordinal}")
        else Bin(p, m, l, r.append(ordinal, value))
    }

    @inline private[collection] final def appendInPlace[S >: T](ordinal: Int, value: S): Ordering[S] = appendInPlace1(null, ordinal, value)
    private[collection] final def appendInPlace1[S >: T](parent: Bin[S] | Null, ordinal: Int, value: S): Ordering[S] = this match {
      case Zero =>
        Tip(ordinal, value)
      case Tip(o, _) if o >= ordinal =>
        throw new IllegalArgumentException(s"Append called with ordinal out of range: $o is not greater than current max ordinal ${this.ordinal}")
      case Tip(o, _) if parent == null =>
        join(ordinal, Tip(ordinal, value), o, this)
      case Tip(o, _) =>
        parent.nn.right = join(ordinal, Tip(ordinal, value), o, this)
        parent.nn
      case b @ Bin(p, m, _, r) =>
        if (!hasMatch(ordinal, p, m)) {
          val b2 = join(ordinal, Tip(ordinal, value), p, this)
          if (parent != null) {
            parent.right = b2
            parent
          } else b2
        } else if (zero(ordinal, m)) throw new IllegalArgumentException(s"Append called with ordinal out of range: $ordinal is not greater than current max ordinal ${this.ordinal}")
        else {
          r.appendInPlace1(b, ordinal, value)
          this
        }
    }

    /** Returns an ordering without any entry at the given ordinal, or this
     *  ordering unchanged if no entry is stored there.
     *
     *  @param ordinal the ordinal to remove
     *  @return an ordering containing the entries of this one except at
     *          `ordinal`
     */
    final def exclude(ordinal: Int): Ordering[T] = this match {
      case Zero =>
        Zero
      case Tip(o, _) =>
        if (ordinal == o) Zero
        else this
      case Bin(p, m, l, r) =>
        if (!hasMatch(ordinal, p, m)) this
        else if (zero(ordinal, m)) bin(p, m, l.exclude(ordinal), r)
        else bin(p, m, l, r.exclude(ordinal))
    }

    /** Returns the ordering of the first `n` entries (in unsigned ordinal
     *  order) and the ordering of the rest, both keeping their original
     *  ordinals.
     *
     *  @param n the number of entries in the first ordering
     *  @return a pair of the first `n` entries and the remaining entries
     */
    final def splitAt(n: Int): (Ordering[T], Ordering[T]) = {
      var rear = Ordering.empty[T]
      var i = n
      (modifyOrRemove { (o, v) =>
        i -= 1
        if (i >= 0) Some(v)
        else {
          rear = rear.appendInPlace(o, v)
          None
        }
      }, rear)
    }

    /**
      * A combined transform and filter function. Returns an `Ordering` such that
      * for each `(key, value)` mapping in this map, if `f(key, value) == None`
      * the map contains no mapping for key, and if `f(key, value) == Some(x)` the
      * map contains `(key, x)`.
      *
      * @tparam S  The type of the values in the resulting `LongMap`.
      * @param f   The transforming function.
      * @return    The modified map.
      */
    final def modifyOrRemove[S](f: (Int, T) => Option[S]): Ordering[S] = this match {
      case Zero => Zero
      case Tip(key, value) =>
        f(key, value) match {
          case None => Zero
          case Some(value2) =>
            // hack to preserve sharing
            if (value.asInstanceOf[AnyRef] eq value2.asInstanceOf[AnyRef]) this.asInstanceOf[Ordering[S]]
            else Tip(key, value2)
        }
      case Bin(prefix, mask, left, right) =>
        val l = left.modifyOrRemove(f)
        val r = right.modifyOrRemove(f)
        if ((left eq l) && (right eq r)) this.asInstanceOf[Ordering[S]]
        else bin(prefix, mask, l, r)
    }
  }
}
