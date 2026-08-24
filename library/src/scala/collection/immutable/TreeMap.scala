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
import scala.collection.Stepper.EfficientSplit
import scala.collection.generic.DefaultSerializable
import scala.collection.immutable.{RedBlackTree => RB}
import scala.collection.mutable.ReusableBuilder
import scala.runtime.AbstractFunction2

/** An immutable SortedMap whose values are stored in a red-black tree.
 *
 *  This class is optimal when range queries will be performed,
 *  or when traversal in order of an ordering is desired.
 *  If you only need key lookups, and don't care in which order key-values
 *  are traversed in, consider using * [[scala.collection.immutable.HashMap]],
 *  which will generally have better performance. If you need insertion order,
 *  consider a * [[scala.collection.immutable.SeqMap]], which does not need to
 *  have an ordering supplied.
 *
 *  @example ```scala sc:compile
 *  import scala.collection.immutable.TreeMap
 *
 *  // Make a TreeMap via the companion object factory
 *  val weekdays = TreeMap(
 *    2 -> "Monday",
 *    3 -> "Tuesday",
 *    4 -> "Wednesday",
 *    5 -> "Thursday",
 *    6 -> "Friday"
 *  )
 *  // TreeMap(2 -> Monday, 3 -> Tuesday, 4 -> Wednesday, 5 -> Thursday, 6 -> Friday)
 *
 *  val days = weekdays ++ List(1 -> "Sunday", 7 -> "Saturday")
 *  // TreeMap(1 -> Sunday, 2 -> Monday, 3 -> Tuesday, 4 -> Wednesday, 5 -> Thursday, 6 -> Friday, 7 -> Saturday)
 *
 *  val day3 = days.get(3) // Some("Tuesday")
 *
 *  val rangeOfDays = days.range(2, 5) // TreeMap(2 -> Monday, 3 -> Tuesday, 4 -> Wednesday)
 *
 *  val daysUntil2 = days.rangeUntil(2) // TreeMap(1 -> Sunday)
 *  val daysTo2 = days.rangeTo(2) // TreeMap(1 -> Sunday, 2 -> Monday)
 *  val daysAfter5 = days.rangeFrom(5) //  TreeMap(5 -> Thursday, 6 -> Friday, 7 -> Saturday)
 *  ```
 *
 *  @tparam K         the type of the keys contained in this tree map.
 *  @tparam V         the type of the values associated with the keys.
 *  @param ordering   the implicit ordering used to compare objects of type `A`.
 *
 *  @see [[https://docs.scala-lang.org/overviews/collections-2.13/concrete-immutable-collection-classes.html#red-black-trees "Scala's Collection Library overview"]]
 *  section on `Red-Black Trees` for more information.
 *
 *  @define Coll immutable.TreeMap
 *  @define coll immutable tree map
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
final class TreeMap[K, +V] private (private val tree: RB.Tree[K, V] | Null)(implicit val ordering: Ordering[K])
  extends AbstractMap[K, V]
    with SortedMap[K, V]
    with StrictOptimizedSortedMapOps[K, V, TreeMap, TreeMap[K, V]]
    with SortedMapFactoryDefaults[K, V, TreeMap, Iterable, Map]
    with DefaultSerializable {

  /** Creates an empty tree map.
   *
   *  @param ordering the ordering used to compare keys
   */
  def this()(implicit ordering: Ordering[K]) = this(null)(using ordering)
  private[immutable] def tree0: RB.Tree[K, V] | Null = tree

  private def newMapOrSelf[V1 >: V](t: RB.Tree[K, V1] | Null): TreeMap[K, V1] = if(t eq tree) this else new TreeMap[K, V1](t)

  /** Returns the `TreeMap` companion object, the factory used by transformation methods to build new tree maps. */
  override def sortedMapFactory: SortedMapFactory[TreeMap] = TreeMap

  /** Returns an iterator over the key-value pairs of this tree map, in ascending order of keys. */
  def iterator: Iterator[(K, V)] = RB.iterator(tree)

  /** Returns an iterator over the keys of this tree map that are greater than or equal to `start`,
   *  in ascending order.
   *
   *  @param start the lower bound (inclusive) on the keys to return
   */
  def keysIteratorFrom(start: K): Iterator[K] = RB.keysIterator(tree, Some(start))

  /** Returns the keys of this tree map as an immutable [[scala.collection.immutable.TreeSet]].
   *
   *  The returned set uses the same ordering as this map and shares this map's underlying
   *  red-black tree, so this operation takes constant time and space.
   */
  override def keySet: TreeSet[K] = new TreeSet(tree)(using ordering)

  /** Returns an iterator over the key-value pairs of this tree map whose keys are greater than
   *  or equal to `start`, in ascending order of keys.
   *
   *  @param start the lower bound (inclusive) on the keys of the entries to return
   */
  def iteratorFrom(start: K): Iterator[(K, V)] = RB.iterator(tree, Some(start))

  /** Returns an iterator over the values of the entries of this tree map whose keys are greater
   *  than or equal to `start`, in ascending order of the associated keys.
   *
   *  @param start the lower bound (inclusive) on the keys of the entries whose values are returned
   */
  override def valuesIteratorFrom(start: K): Iterator[V] = RB.valuesIterator(tree, Some(start))

  /** Returns a stepper over the key-value pairs of this tree map, in ascending order of keys.
   *
   *  The returned stepper supports efficient splitting for parallel processing.
   *
   *  @tparam S the type of the stepper
   *  @param shape an implicit witness selecting the stepper type for element type `(K, V)`
   */
  override def stepper[S <: Stepper[?]](implicit shape: StepperShape[(K, V), S]): S & EfficientSplit =
    shape.parUnbox(
      scala.collection.convert.impl.AnyBinaryTreeStepper.from[(K, V), RB.Tree[K, V]](
        size, tree, _.left, _.right, x => (x.key, x.value)
      )
    )

  /** Returns a stepper over the keys of this tree map, in ascending order.
   *
   *  The returned stepper supports efficient splitting for parallel processing. If `shape`
   *  indicates `Int`, `Long` or `Double` keys, the stepper is a primitive stepper of the
   *  corresponding type, avoiding boxing.
   *
   *  @tparam S the type of the stepper
   *  @param shape an implicit witness selecting the stepper type for key type `K`
   */
  override def keyStepper[S <: Stepper[?]](implicit shape: StepperShape[K, S]): S & EfficientSplit = {
    import scala.collection.convert.impl._
    type T = RB.Tree[K, V]
    val s = shape.shape match {
      case StepperShape.IntShape    => IntBinaryTreeStepper.from[T]   (size, tree, _.left, _.right, _.key.asInstanceOf[Int])
      case StepperShape.LongShape   => LongBinaryTreeStepper.from[T]  (size, tree, _.left, _.right, _.key.asInstanceOf[Long])
      case StepperShape.DoubleShape => DoubleBinaryTreeStepper.from[T](size, tree, _.left, _.right, _.key.asInstanceOf[Double])
      case _         => shape.parUnbox(AnyBinaryTreeStepper.from[K, T](size, tree, _.left, _.right, _.key))
    }
    s.asInstanceOf[S & EfficientSplit]
  }

  /** Returns a stepper over the values of this tree map, in ascending order of the associated keys.
   *
   *  The returned stepper supports efficient splitting for parallel processing. If `shape`
   *  indicates `Int`, `Long` or `Double` values, the stepper is a primitive stepper of the
   *  corresponding type, avoiding boxing.
   *
   *  @tparam S the type of the stepper
   *  @param shape an implicit witness selecting the stepper type for value type `V`
   */
  override def valueStepper[S <: Stepper[?]](implicit shape: StepperShape[V, S]): S & EfficientSplit = {
    import scala.collection.convert.impl._
    type T = RB.Tree[K, V]
    val s = shape.shape match {
      case StepperShape.IntShape    => IntBinaryTreeStepper.from[T]    (size, tree, _.left, _.right, _.value.asInstanceOf[Int])
      case StepperShape.LongShape   => LongBinaryTreeStepper.from[T]   (size, tree, _.left, _.right, _.value.asInstanceOf[Long])
      case StepperShape.DoubleShape => DoubleBinaryTreeStepper.from[T] (size, tree, _.left, _.right, _.value.asInstanceOf[Double])
      case _         => shape.parUnbox(AnyBinaryTreeStepper.from[V, T] (size, tree, _.left, _.right, _.value.asInstanceOf[V]))
    }
    s.asInstanceOf[S & EfficientSplit]
  }

  /** Returns the value associated with `key` in this tree map, wrapped in a `Some`, or `None`
   *  if `key` is not present.
   *
   *  @param key the key to look up
   */
  def get(key: K): Option[V] = RB.get(tree, key)
  /** Returns the value associated with `key` in this tree map, or `default` if `key` is not
   *  present.
   *
   *  Overridden to avoid allocating an intermediate `Option`.
   *
   *  @tparam V1 the result type, a supertype of this map's value type
   *  @param key the key to look up
   *  @param default the value to return if `key` is not present; only evaluated in that case
   */
  override def getOrElse[V1 >: V](key: K, default: => V1): V1 = {
    val resultOrNull = RB.lookup(tree, key)
    if (resultOrNull eq null) default
    else resultOrNull.value
  }

  // override for performance -- no Some allocation
  /** Returns the value associated with `key` in this tree map.
   *
   *  Overridden to avoid allocating an intermediate `Option`.
   *
   *  @param key the key to look up
   *  @return the value bound to `key`
   *  @throws NoSuchElementException if `key` is not present in this tree map
   */
  override def apply(key: K): V = {
    val resultOrNull = RB.lookup(tree, key)
    if (resultOrNull eq null) default(key)
    else resultOrNull.value
  }

  // override for performance -- no Some allocation
  /** Returns `true` if this tree map contains a binding for `key`.
   *
   *  @param key the key to test for membership
   */
  override def contains(key: K): Boolean = RB.contains(tree, key)

  /** Returns a tree map containing all entries of this map except the one with key `key`.
   *
   *  @param key the key to remove
   *  @return a tree map without a binding for `key`; this map itself if it contains no such binding
   */
  def removed(key: K): TreeMap[K,V] =
    newMapOrSelf(RB.delete(tree, key))

  /** Returns a tree map containing all entries of this map as well as a binding of `key` to
   *  `value`, replacing the current value if `key` is already present.
   *
   *  @tparam V1 the type of the added value, a supertype of this map's value type
   *  @param key the key to add or update
   *  @param value the value to associate with `key`
   *  @return a tree map with the updated binding; this map itself if `key` is already bound to
   *          the very same (referentially identical) value
   */
  def updated[V1 >: V](key: K, value: V1): TreeMap[K, V1] =
    newMapOrSelf(RB.update(tree, key, value, overwrite = true))

  /** Returns a tree map containing all entries of this map and all key-value pairs of `that`.
   *
   *  If a key occurs in both, the value from `that` is retained. If `that` is a `TreeMap` with
   *  the same ordering, the result is computed by an efficient tree union; otherwise the pairs
   *  of `that` are added one by one.
   *
   *  @tparam V1 the value type of the result, a supertype of this map's value type
   *  @param that the key-value pairs to add
   *  @return a tree map with the combined entries
   */
  override def concat[V1 >: V](that: collection.IterableOnce[(K, V1)]^): TreeMap[K, V1] =
    newMapOrSelf(that match {
      case tm: TreeMap[K, V] @unchecked if ordering == tm.ordering =>
        RB.union(tree, tm.tree)
      case ls: LinearSeq[(K,V1) @unchecked] =>
        if (ls.isEmpty) tree //to avoid the creation of the adder
        else {
          val adder = new Adder[V1]
          adder.addAll(ls)
          adder.finalTree
        }
      case _ =>
        val adder = new Adder[V1]
        val it = that.iterator
        while (it.hasNext) {
          adder.apply(it.next())
        }
        adder.finalTree
    })

  /** Returns a tree map containing all entries of this map whose keys are not in `keys`.
   *
   *  If `keys` is a `TreeSet` with the same ordering, the result is computed by an efficient
   *  tree difference; otherwise the keys are removed one by one.
   *
   *  @param keys the keys to remove
   *  @return a tree map without bindings for the given keys
   */
  override def removedAll(keys: IterableOnce[K]^): TreeMap[K, V] = (keys: @unchecked) match {
    case ts: TreeSet[K] if ordering == ts.ordering =>
      newMapOrSelf(RB.difference(tree, ts.tree))
    case _ => super.removedAll(keys)
  }

  /** A new TreeMap with the entry added is returned,
   *  assuming that key is <em>not</em> in the TreeMap.
   *
   *  @tparam V1    type of the values of the new bindings, a supertype of `V`
   *  @param key    the key to be inserted
   *  @param value  the value to be associated with `key`
   *  @return       a new $coll with the inserted binding, if it wasn't present in the map
   */
  @deprecated("Use `updated` instead", "2.13.0")
  def insert[V1 >: V](key: K, value: V1): TreeMap[K, V1] = {
    assert(!RB.contains(tree, key))
    updated(key, value)
  }

  /** Returns a tree map containing exactly those entries of this map whose keys lie within the
   *  given optional bounds.
   *
   *  Unlike the ranged projection of a mutable `TreeMap`, the result is an independent map,
   *  built by extracting the requested range from the underlying tree.
   *
   *  @param from the lower bound (inclusive) on keys wrapped in a `Some`, or `None` if there is
   *              no lower bound
   *  @param until the upper bound (exclusive) on keys wrapped in a `Some`, or `None` if there is
   *               no upper bound
   *  @return a tree map with the entries in the given key range; this map itself if neither
   *          bound is given
   */
  def rangeImpl(from: Option[K], until: Option[K]): TreeMap[K, V] = newMapOrSelf(RB.rangeImpl(tree, from, until))

  /** Returns the entry with the smallest key greater than or equal to `key`, if any.
   *
   *  @param key the lower bound (inclusive) for the key lookup
   *  @return a `Some` containing the entry with the smallest key greater than or equal to `key`,
   *          or `None` if no such entry exists
   */
  override def minAfter(key: K): Option[(K, V)] = RB.minAfter(tree, key) match {
    case null => Option.empty
    case x => Some((x.key, x.value))
  }

  /** Returns the entry with the largest key strictly less than `key`, if any.
   *
   *  @param key the upper bound (exclusive) for the key lookup
   *  @return a `Some` containing the entry with the largest key strictly less than `key`,
   *          or `None` if no such entry exists
   */
  override def maxBefore(key: K): Option[(K, V)] = RB.maxBefore(tree, key) match {
    case null => Option.empty
    case x => Some((x.key, x.value))
  }

  /** Returns a tree map containing exactly those entries of this map whose keys are greater
   *  than or equal to `from` and less than `until`.
   *
   *  @param from the lower bound (inclusive) on the keys of the entries to keep
   *  @param until the upper bound (exclusive) on the keys of the entries to keep
   *  @return a tree map with the entries in the given key range
   */
  override def range(from: K, until: K): TreeMap[K,V] = newMapOrSelf(RB.range(tree, from, until))

  /** Applies `f` to each key-value pair of this tree map, in ascending order of keys.
   *
   *  @tparam U the result type of `f`; the results are discarded
   *  @param f the function to apply to each key-value pair
   */
  override def foreach[U](f: ((K, V)) => U): Unit = RB.foreach(tree, f)
  /** Applies the two-argument function `f` to each key and associated value of this tree map,
   *  in ascending order of keys.
   *
   *  Unlike `foreach`, does not allocate a tuple per entry.
   *
   *  @tparam U the result type of `f`; the results are discarded
   *  @param f the function to apply to each key and value
   */
  override def foreachEntry[U](f: (K, V) => U): Unit = RB.foreachEntry(tree, f)
  /** Returns the number of entries in this tree map. Takes constant time: subtree sizes are cached. */
  override def size: Int = RB.count(tree)
  /** Returns the number of entries in this tree map; never `-1`, since the size is known in constant time. */
  override def knownSize: Int = size

  /** Returns `true` if this tree map contains no entries. */
  override def isEmpty = size == 0

  /** Returns the smallest key of this tree map.
   *
   *  @throws NoSuchElementException if this tree map is empty
   */
  override def firstKey: K = RB.smallest(tree).key

  /** Returns the largest key of this tree map.
   *
   *  @throws NoSuchElementException if this tree map is empty
   */
  override def lastKey: K = RB.greatest(tree).key

  /** Returns the entry with the smallest key of this tree map.
   *
   *  @throws NoSuchElementException if this tree map is empty
   */
  override def head: (K, V) = {
    val smallest = RB.smallest(tree)
    (smallest.key, smallest.value)
  }

  /** Returns the entry with the largest key of this tree map.
   *
   *  @throws NoSuchElementException if this tree map is empty
   */
  override def last: (K, V) = {
    val greatest = RB.greatest(tree)
    (greatest.key, greatest.value)
  }

  /** Returns a tree map containing all entries of this map except the one with the smallest key.
   *
   *  @throws NoSuchElementException if this tree map is empty
   */
  override def tail: TreeMap[K, V] = new TreeMap(RB.tail(tree))

  /** Returns a tree map containing all entries of this map except the one with the largest key.
   *
   *  @throws NoSuchElementException if this tree map is empty
   */
  override def init: TreeMap[K, V] = new TreeMap(RB.init(tree))

  /** Returns a tree map containing all entries of this map except the `n` entries with the
   *  smallest keys.
   *
   *  @param n the number of entries to drop
   *  @return a tree map without the first `n` entries in key order; this map itself if
   *          `n <= 0`, or the empty map if `n >= size`
   */
  override def drop(n: Int): TreeMap[K, V] = {
    if (n <= 0) this
    else if (n >= size) empty
    else new TreeMap(RB.drop(tree, n))
  }

  /** Returns a tree map containing only the `n` entries of this map with the smallest keys.
   *
   *  @param n the number of entries to take
   *  @return a tree map with the first `n` entries in key order; the empty map if `n <= 0`,
   *          or this map itself if `n >= size`
   */
  override def take(n: Int): TreeMap[K, V] = {
    if (n <= 0) empty
    else if (n >= size) this
    else new TreeMap(RB.take(tree, n))
  }

  /** Returns a tree map containing the entries of this map at indices `from` until `until`,
   *  where indices count entries in ascending order of keys, starting from zero.
   *
   *  @param from the index of the first entry to keep
   *  @param until the index one past the last entry to keep
   */
  override def slice(from: Int, until: Int) = {
    if (until <= from) empty
    else if (from <= 0) take(until)
    else if (until >= size) drop(from)
    else new TreeMap(RB.slice(tree, from, until))
  }

  /** Returns a tree map containing all entries of this map except the `n` entries with the
   *  largest keys.
   *
   *  @param n the number of entries to drop; treated as `0` if negative
   *  @return a tree map without the last `n` entries in key order
   */
  override def dropRight(n: Int): TreeMap[K, V] = take(size - math.max(n, 0))

  /** Returns a tree map containing only the `n` entries of this map with the largest keys.
   *
   *  @param n the number of entries to take; treated as `0` if negative
   *  @return a tree map with the last `n` entries in key order
   */
  override def takeRight(n: Int): TreeMap[K, V] = drop(size - math.max(n, 0))

  private def countWhile(p: ((K, V)) => Boolean): Int = {
    var result = 0
    val it = iterator
    while (it.hasNext && p(it.next())) result += 1
    result
  }

  /** Returns a tree map containing all entries of this map except its longest prefix, in
   *  ascending order of keys, of entries that satisfy `p`.
   *
   *  @param p the predicate used to test entries
   *  @return a tree map without the longest prefix of entries satisfying `p`
   */
  override def dropWhile(p: ((K, V)) => Boolean): TreeMap[K, V] = drop(countWhile(p))

  /** Returns a tree map containing the longest prefix of this map, in ascending order of keys,
   *  of entries that satisfy `p`.
   *
   *  @param p the predicate used to test entries
   *  @return a tree map with the longest prefix of entries satisfying `p`
   */
  override def takeWhile(p: ((K, V)) => Boolean): TreeMap[K, V] = take(countWhile(p))

  /** Returns a pair of tree maps: the longest prefix of this map, in ascending order of keys,
   *  of entries that satisfy `p`, and the rest of this map.
   *
   *  @param p the predicate used to test entries
   *  @return a pair of the longest prefix of entries satisfying `p` and the remaining entries
   */
  override def span(p: ((K, V)) => Boolean): (TreeMap[K, V], TreeMap[K, V]) = splitAt(countWhile(p))

  /** Returns a tree map containing exactly those entries of this map that satisfy the
   *  predicate `f`.
   *
   *  @param f the predicate used to test entries
   *  @return a tree map with the entries satisfying `f`; this map itself if every entry
   *          satisfies it
   */
  override def filter(f: ((K, V)) => Boolean): TreeMap[K, V] =
    newMapOrSelf(RB.filterEntries[K, V](tree, (k, v) => f((k, v))))

  /** Returns a pair of tree maps: the entries of this map that satisfy the predicate `p`, and
   *  those that do not.
   *
   *  @param p the predicate used to test entries
   *  @return a pair of tree maps with the entries that satisfy `p` and those that do not
   */
  override def partition(p: ((K, V)) => Boolean): (TreeMap[K, V], TreeMap[K, V]) = {
    val (l, r) = RB.partitionEntries[K, V](tree, (k, v) => p((k, v)))
    (newMapOrSelf(l), newMapOrSelf(r))
  }

  /** Returns a tree map with the same keys as this map, where each value is replaced by the
   *  result of applying `f` to the key and the current value.
   *
   *  @tparam W the value type of the resulting map
   *  @param f the transformation to apply to each key-value pair
   *  @return a tree map with transformed values; this map itself if `f` returns the very same
   *          (referentially identical) value for every entry
   */
  override def transform[W](f: (K, V) => W): TreeMap[K, W] = {
    val t2 = RB.transform[K, V, W](tree, f)
    if(t2 eq tree) this.asInstanceOf[TreeMap[K, W]]
    else new TreeMap(t2)
  }

  private final class Adder[B1 >: V]
    extends RB.MapHelper[K, B1] with Function1[(K, B1), Unit] {
    private var currentMutableTree: RB.Tree[K,B1] | Null = tree0
    /** Returns the accumulated tree, converted to an immutable tree that is safe to share. */
    def finalTree = beforePublish(currentMutableTree)
    /** Adds the key-value pair `kv` to the accumulated tree, overwriting the value if the key
     *  is already present. Unshared tree nodes are updated in place.
     *
     *  @param kv the key-value pair to add
     */
    override def apply(kv: (K, B1)): Unit = {
      currentMutableTree = mutableUpd(currentMutableTree, kv._1, kv._2)
    }
    /** Adds all key-value pairs of `ls` to the accumulated tree, in order.
     *
     *  @param ls the sequence of key-value pairs to add
     */
    @tailrec def addAll(ls: LinearSeq[(K, B1)]): Unit = {
      if (!ls.isEmpty) {
        val kv = ls.head
        currentMutableTree = mutableUpd(currentMutableTree, kv._1, kv._2)
        addAll(ls.tail)
      }
    }
  }
  /** Returns `true` if `obj` is a map containing the same key-value pairs as this map.
   *
   *  If `obj` is a `TreeMap` with the same ordering, this is decided by an efficient structural
   *  comparison in which keys are compared with the ordering and values with `==`; otherwise
   *  falls back to the general map equality, which compares keys using `equals` and `hashCode`.
   *
   *  @param obj the object to compare with
   */
  override def equals(obj: Any): Boolean = obj match {
    case that: TreeMap[K @unchecked, ?] if ordering == that.ordering => RB.entriesEqual(tree, that.tree)
    case _ => super.equals(obj)
  }

  /** The prefix of this map's string representation: `"TreeMap"`. */
  override protected def className = "TreeMap"
}

/** $factoryInfo
 *  @define Coll immutable.TreeMap
 *  @define coll immutable tree map
 */
@SerialVersionUID(3L)
object TreeMap extends SortedMapFactory[TreeMap] {

  /** Returns an empty tree map, using the implicit ordering on keys.
   *
   *  @tparam K the key type of the map, which must have an implicit `Ordering`
   *  @tparam V the value type of the map
   */
  def empty[K : Ordering, V]: TreeMap[K, V] = new TreeMap()

  /** Returns a tree map containing the key-value pairs of `it`, ordered by `ordering`.
   *
   *  If `it` is already a `TreeMap` with the same ordering, `it` itself is returned. If it is
   *  another sorted map with the same ordering, the tree is built directly from its iterator in
   *  linear time. Otherwise the pairs are inserted one by one; if `it` contains several pairs
   *  with the same key, the value of the last one is retained.
   *
   *  @tparam K the key type of the map
   *  @tparam V the value type of the map
   *  @param it the collection of key-value pairs
   *  @param ordering the ordering used to compare keys
   *  @return a tree map with the entries of `it`
   */
  def from[K, V](it: IterableOnce[(K, V)]^)(implicit ordering: Ordering[K]): TreeMap[K, V] =
    (it: @unchecked) match {
      case tm: TreeMap[K, V] if ordering == tm.ordering => tm
      case sm: scala.collection.SortedMap[K, V] if ordering == sm.ordering =>
        new TreeMap[K, V](RB.fromOrderedEntries(sm.iterator, sm.size))
      case _ =>
        var t: RB.Tree[K, V] | Null = null
        val i = it.iterator
        while (i.hasNext) {
          val (k, v) = i.next()
          t = RB.update(t, k, v, overwrite = true)
        }
        new TreeMap[K, V](t)
    }

  /** Returns a new builder for tree maps ordered by `ordering`.
   *
   *  The builder can be reused after calling `result()`.
   *
   *  @tparam K the key type of the maps built
   *  @tparam V the value type of the maps built
   *  @param ordering the ordering used to compare keys
   *  @return a reusable builder producing a `TreeMap[K, V]`
   */
  def newBuilder[K, V](implicit ordering: Ordering[K]): ReusableBuilder[(K, V), TreeMap[K, V]] = new TreeMapBuilder[K, V]

  private class TreeMapBuilder[K, V](implicit ordering: Ordering[K])
    extends RB.MapHelper[K, V]
      with ReusableBuilder[(K, V), TreeMap[K, V]] {
    type Tree = RB.Tree[K, V]
    private var tree: Tree | Null = null

    /** Adds the key-value pair `elem` to this builder, overwriting the value if the key is
     *  already present.
     *
     *  The internal tree is updated in place while its nodes are unshared; nodes already
     *  published by `result()` are copied instead of mutated.
     *
     *  @param elem the key-value pair to add
     *  @return this builder
     */
    def addOne(elem: (K, V)): this.type = {
      tree = mutableUpd(tree, elem._1, elem._2)
      this
    }
    private object adder extends AbstractFunction2[K, V, Unit] {
      // we cache tree to avoid the outer access to tree
      // in the hot path (apply)
      private var accumulator: Tree | Null = null
      /** Adds all entries of `hasForEach` to the builder's tree, iterating with `foreachEntry`
       *  to avoid allocating a tuple per entry.
       *
       *  @param hasForEach the map whose entries are added
       */
      def addForEach(hasForEach: collection.Map[K, V]): Unit = {
        accumulator = tree
        hasForEach.foreachEntry(this)
        tree = accumulator
        // be friendly to GC
        accumulator = null
      }

      /** Adds a binding of `key` to `value` to the accumulated tree, overwriting the value if
       *  the key is already present.
       *
       *  @param key the key to add or update
       *  @param value the value to associate with `key`
       */
      override def apply(key: K, value: V): Unit = {
        accumulator = mutableUpd(accumulator, key, value)
      }
    }

    /** Adds all key-value pairs of `xs` to this builder.
     *
     *  If `xs` is a `TreeMap` with the same ordering, the pairs are added by an efficient tree
     *  union, after making this builder's current tree immutable; if it is another map, its
     *  entries are added without allocating a tuple per entry; otherwise the pairs are added
     *  one by one.
     *
     *  @param xs the key-value pairs to add
     *  @return this builder
     */
    override def addAll(xs: IterableOnce[(K, V)]^): this.type = {
      (xs: @unchecked) match {
        // TODO consider writing a mutable-safe union for TreeSet/TreeMap builder ++=
        // for the moment we have to force immutability before the union
        // which will waste some time and space
        // calling `beforePublish` makes `tree` immutable
        case ts: TreeMap[K, V] if ts.ordering == ordering =>
          if (tree eq null) tree = ts.tree0
          else tree = RB.union(beforePublish(tree), ts.tree0)
        case that: collection.Map[K, V]                  =>
          //add avoiding creation of tuples
          adder.addForEach(that)
        case _                                            =>
          super.addAll(xs)
      }
      this
    }

    /** Resets this builder to an empty state, leaving previously built maps unaffected. */
    override def clear(): Unit = {
      tree = null
    }

    /** Returns a tree map with the key-value pairs added so far.
     *
     *  Makes the internal tree immutable, so this builder remains usable afterwards: later
     *  additions copy nodes instead of mutating them.
     */
    override def result(): TreeMap[K, V] = new TreeMap[K, V](beforePublish(tree))
  }
}
