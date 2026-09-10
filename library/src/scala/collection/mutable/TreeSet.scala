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
package collection.mutable

import scala.language.`2.13`
import language.experimental.captureChecking
import scala.collection.Stepper.EfficientSplit
import scala.collection.generic.DefaultSerializable
import scala.collection.mutable.{RedBlackTree => RB}
import scala.collection.{SortedIterableFactory, SortedSetFactoryDefaults, Stepper, StepperShape, StrictOptimizedIterableOps, StrictOptimizedSortedSetOps, mutable}

/** A mutable sorted set implemented using a mutable red-black tree as underlying data structure.
 *
 *  @tparam A the type of the keys contained in this tree set.
 *  @param ordering the implicit ordering used to compare objects of type `A`.
 *
 *  @define Coll mutable.TreeSet
 *  @define coll mutable tree set
 */
// Original API designed in part by Lucien Pereira
sealed class TreeSet[A] private (private val tree: RB.Tree[A, Null])(implicit val ordering: Ordering[A])
  extends AbstractSet[A]
    with SortedSet[A]
    with SortedSetOps[A, TreeSet, TreeSet[A]]
    with StrictOptimizedIterableOps[A, Set, TreeSet[A]]
    with StrictOptimizedSortedSetOps[A, TreeSet, TreeSet[A]]
    with SortedSetFactoryDefaults[A, TreeSet, Set]
    with DefaultSerializable {

  if (ordering eq null)
    throw new NullPointerException("ordering must not be null")

  /** Creates an empty `TreeSet`.
   *  @param ord the implicit ordering used to compare objects of type `A`.
   *  @return an empty `TreeSet`.
   */
  def this()(implicit ord: Ordering[A]) = this(RB.Tree.empty)(using ord)

  /** Returns the companion object [[TreeSet]], the factory used to create new tree sets of the same type */
  override def sortedIterableFactory: SortedIterableFactory[TreeSet] = TreeSet

  /** Returns an iterator over the elements of this tree set, in ascending order */
  def iterator: collection.Iterator[A] = RB.keysIterator(tree)

  /** Returns an iterator over the elements of this tree set that are greater than or equal to `start`, in ascending
   *  order.
   *
   *  @param start the lower bound (inclusive) on the elements to return
   */
  def iteratorFrom(start: A): collection.Iterator[A] = RB.keysIterator(tree, Some(start))

  /** Returns a [[Stepper]] for the elements of this tree set. The stepper visits the elements in ascending order
   *  and supports efficient splitting, so the converters in [[scala.jdk.StreamConverters]] can create parallel
   *  streams from it.
   *
   *  @tparam S the type of the returned `Stepper`, determined by the implicit `StepperShape`
   *  @param shape the `StepperShape` that determines the concrete `Stepper` subtype to return
   *  @return a stepper over the elements, using a primitive-typed `Stepper` subclass when the resolved `StepperShape` corresponds to `Int`, `Long`, or `Double`
   */
  override def stepper[S <: Stepper[?]](implicit shape: StepperShape[A, S]): S & EfficientSplit = {
    import scala.collection.convert.impl._
    type T = RB.Node[A, Null]
    val s = shape.shape match {
      case StepperShape.IntShape    => IntBinaryTreeStepper.from[T]   (size, tree.root, _.left, _.right, _.key.asInstanceOf[Int])
      case StepperShape.LongShape   => LongBinaryTreeStepper.from[T]  (size, tree.root, _.left, _.right, _.key.asInstanceOf[Long])
      case StepperShape.DoubleShape => DoubleBinaryTreeStepper.from[T](size, tree.root, _.left, _.right, _.key.asInstanceOf[Double])
      case _         => shape.parUnbox(AnyBinaryTreeStepper.from[A, T](size, tree.root, _.left, _.right, _.key))
    }
    s.asInstanceOf[S & EfficientSplit]
  }

  /** Adds `elem` to this tree set. If the set already contains an element equal to `elem` under the ordering, the
   *  set is unchanged.
   *
   *  @param elem the element to add
   *  @return this tree set
   */
  def addOne(elem: A): this.type = {
    RB.insert(tree, elem, null)
    this
  }

  /** Removes the element equal to `elem` under the ordering from this tree set, if one exists; otherwise does
   *  nothing.
   *
   *  @param elem the element to remove
   *  @return this tree set
   */
  def subtractOne(elem: A): this.type = {
    RB.delete(tree, elem)
    this
  }

  /** Removes all elements from this tree set, leaving it empty */
  def clear(): Unit = RB.clear(tree)

  /** Returns `true` if this tree set contains an element equal to `elem` under the ordering.
   *
   *  @param elem the element to look for
   */
  def contains(elem: A): Boolean = RB.contains(tree, elem)

  /** Returns this set itself, with its static type widened to `collection.Set`; no bounds are removed */
  def unconstrained: collection.Set[A] = this

  /** Creates a ranged projection of this set. Any mutations in the ranged projection affect the original set and
   *  vice versa.
   *
   *  Only elements between this projection's bounds will ever appear as elements of this set, independently of
   *  whether the elements are added through the original set or through this view. That means that if one inserts an
   *  element in a view whose key is outside the view's bounds, calls to `contains` will _not_ consider the newly
   *  added element. Mutations are always reflected in the original set, though.
   *
   *  @param from the lower bound (inclusive) of this projection wrapped in a `Some`, or `None` if there is no lower
   *             bound.
   *  @param until the upper bound (exclusive) of this projection wrapped in a `Some`, or `None` if there is no upper
   *              bound.
   *  @return a new `TreeSet` that is a ranged projection of this set, sharing the same underlying data
   */
  def rangeImpl(from: Option[A], until: Option[A]): TreeSet[A] = new TreeSetProjection(from, until)

  /** Returns `"TreeSet"`, the name used in this set's string representation */
  override protected def className: String = "TreeSet"

  /** Returns the number of elements in this tree set, in O(1) time */
  override def size: Int = RB.size(tree)
  /** Returns the number of elements in this tree set; never -1, because the size is always known */
  override def knownSize: Int = size
  /** Returns `true` if this tree set contains no elements */
  override def isEmpty: Boolean = RB.isEmpty(tree)

  /** Returns the smallest element of this tree set.
   *
   *  @throws NoSuchElementException if this tree set is empty
   */
  override def head: A = RB.minKey(tree).get

  /** Returns the largest element of this tree set.
   *
   *  @throws NoSuchElementException if this tree set is empty
   */
  override def last: A = RB.maxKey(tree).get

  /** Returns the smallest element greater than or equal to `key`, if any.
   *
   *  @param key the lower bound (inclusive) for the lookup
   *  @return a `Some` containing the smallest element greater than or equal to `key`, or `None` if no such element exists
   */
  override def minAfter(key: A): Option[A] = RB.minKeyAfter(tree, key)

  /** Returns the largest element strictly less than `key`, if any.
   *
   *  @param key the upper bound (exclusive) for the lookup
   *  @return a `Some` containing the largest element strictly less than `key`, or `None` if no such element exists
   */
  override def maxBefore(key: A): Option[A] = RB.maxKeyBefore(tree, key)

  /** Applies `f` to each element of this tree set, in ascending order, for its side effects.
   *
   *  @tparam U the result type of `f`, which is discarded
   *  @param f the function to apply to each element
   */
  override def foreach[U](f: A => U): Unit = RB.foreachKey(tree, f)


  /** A ranged projection of a [[TreeSet]]. Mutations on this set affect the original set and vice versa.
   *
   *  Only keys between this projection's key range will ever appear as elements of this set, independently of whether
   *  the elements are added through the original set or through this view. That means that if one inserts an element in
   *  a view whose key is outside the view's bounds, calls to `contains` will _not_ consider the newly added element.
   *  Mutations are always reflected in the original set, though.
   *
   *  @param from the lower bound (inclusive) of this projection wrapped in a `Some`, or `None` if there is no lower
   *             bound.
   *  @param until the upper bound (exclusive) of this projection wrapped in a `Some`, or `None` if there is no upper
   *              bound.
   */
  private final class TreeSetProjection(from: Option[A], until: Option[A]) extends TreeSet[A](tree) {
    self: TreeSetProjection^{} =>

    /** Given a possible new lower bound, chooses and returns the most constraining one (the maximum).
     *
     *  @param newFrom a possible new lower bound wrapped in a `Some`, or `None` if unbounded
     *  @return the more restrictive of the existing `from` bound and `newFrom`, or `None` if both are unbounded
     */
    private def pickLowerBound(newFrom: Option[A]): Option[A] = (from, newFrom) match {
      case (Some(fr), Some(newFr)) => Some(ordering.max(fr, newFr))
      case (None, _) => newFrom
      case _ => from
    }

    /** Given a possible new upper bound, chooses and returns the most constraining one (the minimum).
     *
     *  @param newUntil a possible new upper bound wrapped in a `Some`, or `None` if unbounded
     *  @return the more restrictive of the existing `until` bound and `newUntil`, or `None` if both are unbounded
     */
    private def pickUpperBound(newUntil: Option[A]): Option[A] = (until, newUntil) match {
      case (Some(unt), Some(newUnt)) => Some(ordering.min(unt, newUnt))
      case (None, _) => newUntil
      case _ => until
    }

    /** Returns true if the argument is inside the view bounds (between `from` and `until`).
     *
     *  @param key the element to check against the view bounds
     *  @return `true` if `key` is greater than or equal to the `from` bound (if any) and strictly less than the `until` bound (if any)
     */
    private def isInsideViewBounds(key: A): Boolean = {
      val afterFrom = from.isEmpty || ordering.compare(from.get, key) <= 0
      val beforeUntil = until.isEmpty || ordering.compare(key, until.get) < 0
      afterFrom && beforeUntil
    }

    override def rangeImpl(from: Option[A], until: Option[A]): TreeSet[A] =
      new TreeSetProjection(pickLowerBound(from), pickUpperBound(until))

    override def contains(key: A) = isInsideViewBounds(key) && RB.contains(tree, key)

    override def iterator = RB.keysIterator(tree, from, until)
    override def iteratorFrom(start: A) = RB.keysIterator(tree, pickLowerBound(Some(start)), until)

    override def size = if (RB.size(tree) == 0) 0 else iterator.length
    override def knownSize: Int = if (RB.size(tree) == 0) 0 else -1
    override def isEmpty: Boolean = RB.size(tree) == 0 || !iterator.hasNext

    override def head: A = headOption.get
    override def headOption: Option[A] = {
      val elem = if (from.isDefined) RB.minKeyAfter(tree, from.get) else RB.minKey(tree)
      (elem, until) match {
        case (Some(e), Some(unt)) if ordering.compare(e, unt) >= 0 => None
        case _ => elem
      }
    }

    override def last: A = lastOption.get
    override def lastOption = {
      val elem = if (until.isDefined) RB.maxKeyBefore(tree, until.get) else RB.maxKey(tree)
      (elem, from) match {
        case (Some(e), Some(fr)) if ordering.compare(e, fr) < 0 => None
        case _ => elem
      }
    }

    // Using the iterator should be efficient enough; if performance is deemed a problem later, a specialized
    // `foreachKey(f, from, until)` method can be created in `RedBlackTree`. See
    // https://github.com/scala/scala/pull/4608#discussion_r34307985 for a discussion about this.
    override def foreach[U](f: A => U): Unit = iterator.foreach(f)

    override def clone(): mutable.TreeSet[A] = super.clone().rangeImpl(from, until)

  }

}

/** $factoryInfo
 *  @define Coll `mutable.TreeSet`
 *  @define coll mutable tree set
 */
@SerialVersionUID(3L)
object TreeSet extends SortedIterableFactory[TreeSet] {

  /** Returns a new, empty `TreeSet` ordered by the implicit `Ordering`.
   *
   *  @tparam A the type of the elements, which must have an implicit `Ordering`
   *  @return an empty `TreeSet`
   */
  def empty[A : Ordering]: TreeSet[A] = new TreeSet[A]()

  /** Returns a new `TreeSet` containing the elements of `it`, ordered by `ordering`.
   *
   *  When `it` is itself a `TreeSet` with an equal ordering, its tree is copied node by node; when it is another
   *  sorted set with an equal ordering, or a `Range` ordered by `Ordering.Int` or its reverse, the new tree is built
   *  in one pass from the already ordered elements; otherwise the elements are inserted one by one.
   *
   *  @tparam E the type of the elements
   *  @param it the elements of the new tree set
   *  @param ordering the ordering used to compare elements
   *  @return a new `TreeSet` containing the elements of `it`
   */
  def from[E](it: IterableOnce[E]^)(implicit ordering: Ordering[E]): TreeSet[E] =
    (it: @unchecked) match {
      case ts: TreeSet[E] if ordering == ts.ordering =>
        new TreeSet[E](ts.tree.treeCopy())
      case ss: scala.collection.SortedSet[E] if ordering == ss.ordering =>
        new TreeSet[E](RB.fromOrderedKeys(ss.iterator, ss.size))
      case r: Range if (ordering eq Ordering.Int) || (ordering eq Ordering.Int.reverse) =>
        val it = if((ordering eq Ordering.Int) == (r.step > 0)) r.iterator else r.reverseIterator
        new TreeSet[E](RB.fromOrderedKeys(it.asInstanceOf[Iterator[E]], r.size))
      case _ =>
        val t: RB.Tree[E, Null] = RB.Tree.empty
        val i = it.iterator
        while (i.hasNext) RB.insert(t, i.next(), null)
        new TreeSet[E](t)
    }

  /** Returns a new builder that builds a `TreeSet` by inserting the supplied elements into an initially empty tree.
   *  The builder is reusable: calling `result()` and then `clear()` allows building another, independent tree set.
   *
   *  @tparam A the type of the elements
   *  @param ordering the ordering used to compare elements
   *  @return a builder for a new `TreeSet`
   */
  def newBuilder[A](implicit ordering: Ordering[A]): Builder[A, TreeSet[A]] = new ReusableBuilder[A, TreeSet[A]] {
    private var tree: RB.Tree[A, Null] = RB.Tree.empty
    def addOne(elem: A): this.type = { RB.insert(tree, elem, null); this }
    def result(): TreeSet[A] = new TreeSet[A](tree)
    def clear(): Unit = { tree = RB.Tree.empty }
  }
}
