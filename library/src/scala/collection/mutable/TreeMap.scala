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
import scala.collection.Stepper.EfficientSplit
import scala.collection.generic.DefaultSerializable
import scala.collection.mutable.{RedBlackTree => RB}

/** A mutable sorted map implemented using a mutable red-black tree as underlying data structure.
 *
 *  @tparam K the type of the keys contained in this tree map.
 *  @tparam V the type of the values associated with the keys.
 *  @param ordering the implicit ordering used to compare objects of type `A`.
 *
 *  @define Coll mutable.TreeMap
 *  @define coll mutable tree map
 */
sealed class TreeMap[K, V] private (tree: RB.Tree[K, V])(implicit val ordering: Ordering[K])
  extends AbstractMap[K, V]
    with SortedMap[K, V]
    with SortedMapOps[K, V, TreeMap, TreeMap[K, V]]
    with StrictOptimizedIterableOps[(K, V), Iterable, TreeMap[K, V]]
    with StrictOptimizedMapOps[K, V, Map, TreeMap[K, V]]
    with StrictOptimizedSortedMapOps[K, V, TreeMap, TreeMap[K, V]]
    with SortedMapFactoryDefaults[K, V, TreeMap, Iterable, Map]
    with DefaultSerializable {

  /** Returns the companion object [[TreeMap]], the factory used to create new tree maps of the same type */
  override def sortedMapFactory: TreeMap.type = TreeMap

  /** Creates an empty `TreeMap`.
   *  @param ord the implicit ordering used to compare objects of type `K`.
   *  @return an empty `TreeMap`.
   */
  def this()(implicit ord: Ordering[K]) = this(RB.Tree.empty)(using ord)

  /** Returns an iterator over the key-value pairs of this tree map, in ascending order of keys */
  def iterator: Iterator[(K, V)] = {
    if (isEmpty) Iterator.empty
    else RB.iterator(tree)
  }

  /** Returns an iterator over the keys of this tree map, in ascending order */
  override def keysIterator: Iterator[K] = {
    if (isEmpty) Iterator.empty
    else RB.keysIterator(tree, None)
  }

  /** Returns an iterator over the values of this tree map, in ascending order of the associated keys */
  override def valuesIterator: Iterator[V] = {
    if (isEmpty) Iterator.empty
    else RB.valuesIterator(tree, None)
  }

  /** Returns an iterator over the keys of this tree map that are greater than or equal to `start`, in ascending
   *  order.
   *
   *  @param start the lower bound (inclusive) on the keys to return
   */
  def keysIteratorFrom(start: K): Iterator[K] = {
    if (isEmpty) Iterator.empty
    else RB.keysIterator(tree, Some(start))
  }

  /** Returns an iterator over the key-value pairs of this tree map whose keys are greater than or equal to `start`,
   *  in ascending order of keys.
   *
   *  @param start the lower bound (inclusive) on the keys of the entries to return
   */
  def iteratorFrom(start: K): Iterator[(K, V)] = {
    if (isEmpty) Iterator.empty
    else RB.iterator(tree, Some(start))
  }

  /** Returns an iterator over the values of the entries of this tree map whose keys are greater than or equal to
   *  `start`, in ascending order of the associated keys.
   *
   *  @param start the lower bound (inclusive) on the keys of the entries whose values to return
   */
  override def valuesIteratorFrom(start: K): Iterator[V] = {
    if (isEmpty) Iterator.empty
    else RB.valuesIterator(tree, Some(start))
  }

  /** Returns a [[Stepper]] for the key-value pairs of this tree map. The stepper visits the pairs in ascending
   *  order of keys and supports efficient splitting, so the converters in [[scala.jdk.StreamConverters]] can create
   *  parallel streams from it.
   *
   *  @tparam S the type of the returned `Stepper`, determined by the implicit `StepperShape`
   *  @param shape the `StepperShape` that determines the concrete `Stepper` subtype to return
   */
  override def stepper[S <: Stepper[?]](implicit shape: StepperShape[(K, V), S]): S & EfficientSplit =
    shape.parUnbox(
      scala.collection.convert.impl.AnyBinaryTreeStepper.from[(K, V), RB.Node[K, V]](
        size, tree.root, _.left, _.right, x => (x.key, x.value)
      )
    )

  /** Returns a [[Stepper]] for the keys of this tree map. The stepper visits the keys in ascending order and
   *  supports efficient splitting, so the converters in [[scala.jdk.StreamConverters]] can create parallel streams
   *  from it.
   *
   *  @tparam S the type of the returned `Stepper`, determined by the implicit `StepperShape`
   *  @param shape the `StepperShape` that determines the concrete `Stepper` subtype to return
   *  @return a stepper over the keys, using a primitive-typed `Stepper` subclass when the resolved `StepperShape` corresponds to `Int`, `Long`, or `Double`
   */
  override def keyStepper[S <: Stepper[?]](implicit shape: StepperShape[K, S]): S & EfficientSplit = {
    import scala.collection.convert.impl._
    type T = RB.Node[K, V]
    val s = shape.shape match {
      case StepperShape.IntShape    => IntBinaryTreeStepper.from[T]   (size, tree.root, _.left, _.right, _.key.asInstanceOf[Int])
      case StepperShape.LongShape   => LongBinaryTreeStepper.from[T]  (size, tree.root, _.left, _.right, _.key.asInstanceOf[Long])
      case StepperShape.DoubleShape => DoubleBinaryTreeStepper.from[T](size, tree.root, _.left, _.right, _.key.asInstanceOf[Double])
      case _         => shape.parUnbox(AnyBinaryTreeStepper.from[K, T](size, tree.root, _.left, _.right, _.key))
    }
    s.asInstanceOf[S & EfficientSplit]
  }

  /** Returns a [[Stepper]] for the values of this tree map. The stepper visits the values in ascending order of the
   *  associated keys and supports efficient splitting, so the converters in [[scala.jdk.StreamConverters]] can
   *  create parallel streams from it.
   *
   *  @tparam S the type of the returned `Stepper`, determined by the implicit `StepperShape`
   *  @param shape the `StepperShape` that determines the concrete `Stepper` subtype to return
   *  @return a stepper over the values, using a primitive-typed `Stepper` subclass when the resolved `StepperShape` corresponds to `Int`, `Long`, or `Double`
   */
  override def valueStepper[S <: Stepper[?]](implicit shape: StepperShape[V, S]): S & EfficientSplit = {
    import scala.collection.convert.impl._
    type T = RB.Node[K, V]
    val s = shape.shape match {
      case StepperShape.IntShape    => IntBinaryTreeStepper.from[T]    (size, tree.root, _.left, _.right, _.value.asInstanceOf[Int])
      case StepperShape.LongShape   => LongBinaryTreeStepper.from[T]   (size, tree.root, _.left, _.right, _.value.asInstanceOf[Long])
      case StepperShape.DoubleShape => DoubleBinaryTreeStepper.from[T] (size, tree.root, _.left, _.right, _.value.asInstanceOf[Double])
      case _         => shape.parUnbox(AnyBinaryTreeStepper.from[V, T] (size, tree.root, _.left, _.right, _.value))
    }
    s.asInstanceOf[S & EfficientSplit]
  }

  /** Adds the key-value pair `elem` to this tree map. If the map already contains an entry whose key is equal to
   *  `elem._1` under the ordering, that entry's value is replaced by `elem._2`.
   *
   *  @param elem the key-value pair to add
   *  @return this tree map
   */
  def addOne(elem: (K, V)): this.type = { RB.insert(tree, elem._1, elem._2); this }

  /** Removes the entry with key `elem` from this tree map, if one exists; otherwise does nothing.
   *
   *  @param elem the key of the entry to remove
   *  @return this tree map
   */
  def subtractOne(elem: K): this.type = { RB.delete(tree, elem); this }

  /** Removes all entries from this tree map, leaving it empty */
  override def clear(): Unit = RB.clear(tree)

  /** Returns the value associated with `key` in this tree map.
   *
   *  @param key the key to look up
   *  @return a `Some` containing the value associated with `key`, or `None` if the key is not present
   */
  def get(key: K): Option[V] = RB.get(tree, key)

  /** Creates a ranged projection of this map. Any mutations in the ranged projection will update the original map and
   *  vice versa.
   *
   *  Only entries with keys between this projection's key range will ever appear as elements of this map, independently
   *  of whether the entries are added through the original map or through this view. That means that if one inserts a
   *  key-value in a view whose key is outside the view's bounds, calls to `get` or `contains` will _not_ consider the
   *  newly added entry. Mutations are always reflected in the original map, though.
   *
   *  @param from the lower bound (inclusive) of this projection wrapped in a `Some`, or `None` if there is no lower
   *             bound.
   *  @param until the upper bound (exclusive) of this projection wrapped in a `Some`, or `None` if there is no upper
   *              bound.
   *  @return a new `TreeMap` that is a ranged projection of this map, sharing the same underlying data
   */
  def rangeImpl(from: Option[K], until: Option[K]): TreeMap[K, V] = new TreeMapProjection(from, until)

  /** Applies `f` to each key-value pair of this tree map, in ascending order of keys, for its side effects.
   *
   *  @tparam U the result type of `f`, which is discarded
   *  @param f the function to apply to each key-value pair
   */
  override def foreach[U](f: ((K, V)) => U): Unit = RB.foreach(tree, f)
  /** Applies `f` to the key and value of each entry of this tree map, passed as two separate arguments, in ascending
   *  order of keys, for its side effects.
   *
   *  @tparam U the result type of `f`, which is discarded
   *  @param f the function applied to the key and value of each entry
   */
  override def foreachEntry[U](f: (K, V) => U): Unit = RB.foreachEntry(tree, f)

  /** Returns the number of entries in this tree map, in O(1) time */
  override def size: Int = RB.size(tree)
  /** Returns the number of entries in this tree map; never -1, because the size is always known */
  override def knownSize: Int = size
  /** Returns `true` if this tree map contains no entries */
  override def isEmpty: Boolean = RB.isEmpty(tree)

  /** Returns `true` if this tree map contains an entry whose key is equal to `key` under the ordering.
   *
   *  @param key the key to look for
   */
  override def contains(key: K): Boolean = RB.contains(tree, key)

  /** Returns the entry with the smallest key of this tree map.
   *
   *  @throws NoSuchElementException if this tree map is empty
   */
  override def head: (K, V) = RB.min(tree).get

  /** Returns the entry with the largest key of this tree map.
   *
   *  @throws NoSuchElementException if this tree map is empty
   */
  override def last: (K, V) = RB.max(tree).get

  /** Returns the entry with the smallest key greater than or equal to `key`, if any.
   *
   *  @param key the lower bound (inclusive) for the key lookup
   *  @return a `Some` containing the entry with the smallest key greater than or equal to `key`, or `None` if no such entry exists
   */
  override def minAfter(key: K): Option[(K, V)] = RB.minAfter(tree, key)

  /** Returns the entry with the largest key strictly less than `key`, if any.
   *
   *  @param key the upper bound (exclusive) for the key lookup
   *  @return a `Some` containing the entry with the largest key strictly less than `key`, or `None` if no such entry exists
   */
  override def maxBefore(key: K): Option[(K, V)] = RB.maxBefore(tree, key)

  /** Returns `"TreeMap"`, the name used in this map's string representation */
  override protected def className: String = "TreeMap"


  /** A ranged projection of a [[TreeMap]]. Mutations on this map affect the original map and vice versa.
   *
   *  Only entries with keys between this projection's key range will ever appear as elements of this map, independently
   *  of whether the entries are added through the original map or through this view. That means that if one inserts a
   *  key-value in a view whose key is outside the view's bounds, calls to `get` or `contains` will _not_ consider the
   *  newly added entry. Mutations are always reflected in the original map, though.
   *
   *  @param from the lower bound (inclusive) of this projection wrapped in a `Some`, or `None` if there is no lower
   *             bound.
   *  @param until the upper bound (exclusive) of this projection wrapped in a `Some`, or `None` if there is no upper
   *              bound.
   */
  private final class TreeMapProjection(from: Option[K], until: Option[K]) extends TreeMap[K, V](tree) {

    /** Given a possible new lower bound, chooses and returns the most constraining one (the maximum).
     *
     *  @param newFrom a possible new lower bound wrapped in a `Some`, or `None` if unconstrained
     *  @return the more constraining of the current `from` and `newFrom` lower bounds, or `None` if both are unconstrained
     */
    private def pickLowerBound(newFrom: Option[K]): Option[K] = (from, newFrom) match {
      case (Some(fr), Some(newFr)) => Some(ordering.max(fr, newFr))
      case (None, _) => newFrom
      case _ => from
    }

    /** Given a possible new upper bound, chooses and returns the most constraining one (the minimum).
     *
     *  @param newUntil a possible new upper bound wrapped in a `Some`, or `None` if unconstrained
     *  @return the more constraining of the current `until` and `newUntil` upper bounds, or `None` if both are unconstrained
     */
    private def pickUpperBound(newUntil: Option[K]): Option[K] = (until, newUntil) match {
      case (Some(unt), Some(newUnt)) => Some(ordering.min(unt, newUnt))
      case (None, _) => newUntil
      case _ => until
    }

    /** Returns true if the argument is inside the view bounds (between `from` and `until`).
     *
     *  @param key the key to check against the view bounds
     *  @return `true` if `key` is at or above `from` (when defined) and strictly below `until` (when defined)
     */
    private def isInsideViewBounds(key: K): Boolean = {
      val afterFrom = from.isEmpty || ordering.compare(from.get, key) <= 0
      val beforeUntil = until.isEmpty || ordering.compare(key, until.get) < 0
      afterFrom && beforeUntil
    }

    override def rangeImpl(from: Option[K], until: Option[K]): TreeMap[K, V] =
      new TreeMapProjection(pickLowerBound(from), pickUpperBound(until))

    override def get(key: K) = if (isInsideViewBounds(key)) RB.get(tree, key) else None

    override def iterator: Iterator[(K, V)] = if (RB.size(tree) == 0) Iterator.empty else RB.iterator(tree, from, until)
    override def keysIterator: Iterator[K] = if (RB.size(tree) == 0) Iterator.empty else RB.keysIterator(tree, from, until)
    override def valuesIterator: Iterator[V] = if (RB.size(tree) == 0) Iterator.empty else RB.valuesIterator(tree, from, until)
    override def keysIteratorFrom(start: K) = if (RB.size(tree) == 0) Iterator.empty else RB.keysIterator(tree, pickLowerBound(Some(start)), until)
    override def iteratorFrom(start: K) = if (RB.size(tree) == 0) Iterator.empty else RB.iterator(tree, pickLowerBound(Some(start)), until)
    override def valuesIteratorFrom(start: K) = if (RB.size(tree) == 0) Iterator.empty else RB.valuesIterator(tree, pickLowerBound(Some(start)), until)
    override def size = if (RB.size(tree) == 0) 0 else iterator.length
    override def knownSize: Int = if (RB.size(tree) == 0) 0 else -1
    override def isEmpty = RB.size(tree) == 0 || !iterator.hasNext
    override def contains(key: K) = isInsideViewBounds(key) && RB.contains(tree, key)

    override def head = headOption.get
    override def headOption = {
      val entry = if (from.isDefined) RB.minAfter(tree, from.get) else RB.min(tree)
      (entry, until) match {
        case (Some(e), Some(unt)) if ordering.compare(e._1, unt) >= 0 => None
        case _ => entry
      }
    }

    override def last = lastOption.get
    override def lastOption = {
      val entry = if (until.isDefined) RB.maxBefore(tree, until.get) else RB.max(tree)
      (entry, from) match {
        case (Some(e), Some(fr)) if ordering.compare(e._1, fr) < 0 => None
        case _ => entry
      }
    }

    // Using the iterator should be efficient enough; if performance is deemed a problem later, specialized
    // `foreach(f, from, until)` and `transform(f, from, until)` methods can be created in `RedBlackTree`. See
    // https://github.com/scala/scala/pull/4608#discussion_r34307985 for a discussion about this.
    override def foreach[U](f: ((K, V)) => U): Unit = iterator.foreach(f)

    override def clone() = super.clone().rangeImpl(from, until)
  }

}

/** $factoryInfo
 *
 *  @define Coll mutable.TreeMap
 *  @define coll mutable tree map
 */
@SerialVersionUID(3L)
object TreeMap extends SortedMapFactory[TreeMap] {

  /** Returns a new `TreeMap` containing the key-value pairs of `it`, ordered by the implicit `Ordering` on the keys.
   *  If `it` contains several pairs whose keys are equal under the ordering, the value of the last such pair wins.
   *
   *  @tparam K the type of the keys, which must have an implicit `Ordering`
   *  @tparam V the type of the values
   *  @param it the key-value pairs of the new tree map
   *  @return a new `TreeMap` containing the entries of `it`
   */
  def from[K : Ordering, V](it: IterableOnce[(K, V)]^): TreeMap[K, V] =
    Growable.from(empty[K, V], it)

  /** Returns a new, empty `TreeMap` ordered by the implicit `Ordering` on the keys.
   *
   *  @tparam K the type of the keys, which must have an implicit `Ordering`
   *  @tparam V the type of the values
   *  @return an empty `TreeMap`
   */
  def empty[K : Ordering, V]: TreeMap[K, V] = new TreeMap[K, V]()

  /** Returns a new builder that builds a `TreeMap` by inserting the supplied key-value pairs into an initially
   *  empty tree map.
   *
   *  @tparam K the type of the keys, which must have an implicit `Ordering`
   *  @tparam V the type of the values
   *  @return a builder for a new `TreeMap`
   */
  def newBuilder[K: Ordering, V]: Builder[(K, V), TreeMap[K, V]] = new GrowableBuilder(empty[K, V])

}
