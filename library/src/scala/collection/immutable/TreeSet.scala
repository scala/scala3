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

import scala.collection.Stepper.EfficientSplit
import scala.collection.generic.DefaultSerializable
import scala.collection.mutable.ReusableBuilder
import scala.collection.immutable.{RedBlackTree => RB}
import scala.runtime.AbstractFunction1


/** This class implements immutable sorted sets using a tree.
 *
 *  @tparam A         the type of the elements contained in this tree set
 *  @param ordering   the implicit ordering used to compare objects of type `A`
 *
 *  @see ["Scala's Collection Library overview"](https://docs.scala-lang.org/overviews/collections-2.13/concrete-immutable-collection-classes.html#red-black-trees)
 *  section on `Red-Black Trees` for more information.
 *
 *  @define Coll `immutable.TreeSet`
 *  @define coll immutable tree set
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
final class TreeSet[A] private[immutable] (private[immutable] val tree: RB.Tree[A, Any] | Null)(implicit val ordering: Ordering[A])
  extends AbstractSet[A]
    with SortedSet[A]
    with SortedSetOps[A, TreeSet, TreeSet[A]]
    with StrictOptimizedSortedSetOps[A, TreeSet, TreeSet[A]]
    with SortedSetFactoryDefaults[A, TreeSet, Set]
    with DefaultSerializable {

  if (ordering eq null) throw new NullPointerException("ordering must not be null")

  /** Creates an empty tree set.
   *
   *  @param ordering the ordering used to compare elements
   */
  def this()(implicit ordering: Ordering[A]) = this(null)(using ordering)

  /** Returns the `TreeSet` companion object, the factory used by transformation methods to build new tree sets. */
  override def sortedIterableFactory: TreeSet.type = TreeSet

  private def newSetOrSelf(t: RB.Tree[A, Any] | Null) = if(t eq tree) this else new TreeSet[A](t)

  /** Returns the number of elements in this tree set. Takes constant time: subtree sizes are cached. */
  override def size: Int = RB.count(tree)

  /** Returns `true` if this tree set contains no elements. */
  override def isEmpty = size == 0

  /** Returns the smallest element of this tree set.
   *
   *  @throws NoSuchElementException if this tree set is empty
   */
  override def head: A = RB.smallest(tree).key

  /** Returns the largest element of this tree set.
   *
   *  @throws NoSuchElementException if this tree set is empty
   */
  override def last: A = RB.greatest(tree).key

  /** Returns a tree set containing all elements of this set except the smallest.
   *
   *  @throws NoSuchElementException if this tree set is empty
   */
  override def tail: TreeSet[A] = new TreeSet(RB.tail(tree))

  /** Returns a tree set containing all elements of this set except the largest.
   *
   *  @throws NoSuchElementException if this tree set is empty
   */
  override def init: TreeSet[A] = new TreeSet(RB.init(tree))

  /** Returns the smallest element of this tree set with respect to `ord`.
   *
   *  When `ord` is the same object as this set's ordering, this is the first element, found in
   *  logarithmic time; otherwise all elements are scanned.
   *
   *  @tparam A1 the type on which `ord` is defined, a supertype of the element type
   *  @param ord the ordering used to compare elements
   *  @return the smallest element with respect to `ord`
   *  @throws UnsupportedOperationException if this tree set is empty
   */
  override def min[A1 >: A](implicit ord: Ordering[A1]): A = {
    if ((ord eq ordering) && nonEmpty) {
      head
    } else {
      super.min(using ord)
    }
  }

  /** Returns the largest element of this tree set with respect to `ord`.
   *
   *  When `ord` is the same object as this set's ordering, this is the last element, found in
   *  logarithmic time; otherwise all elements are scanned.
   *
   *  @tparam A1 the type on which `ord` is defined, a supertype of the element type
   *  @param ord the ordering used to compare elements
   *  @return the largest element with respect to `ord`
   *  @throws UnsupportedOperationException if this tree set is empty
   */
  override def max[A1 >: A](implicit ord: Ordering[A1]): A = {
    if ((ord eq ordering) && nonEmpty) {
      last
    } else {
      super.max(using ord)
    }
  }

  /** Returns a tree set containing all elements of this set except the `n` smallest.
   *
   *  @param n the number of elements to drop
   *  @return a tree set without the first `n` elements in ascending order; this set itself if
   *          `n <= 0`, or the empty set if `n >= size`
   */
  override def drop(n: Int): TreeSet[A] = {
    if (n <= 0) this
    else if (n >= size) empty
    else new TreeSet(RB.drop(tree, n))
  }

  /** Returns a tree set containing only the `n` smallest elements of this set.
   *
   *  @param n the number of elements to take
   *  @return a tree set with the first `n` elements in ascending order; the empty set if
   *          `n <= 0`, or this set itself if `n >= size`
   */
  override def take(n: Int): TreeSet[A] = {
    if (n <= 0) empty
    else if (n >= size) this
    else new TreeSet(RB.take(tree, n))
  }

  /** Returns a tree set containing the elements of this set at indices `from` until `until`,
   *  where indices count elements in ascending order, starting from zero.
   *
   *  @param from the index of the first element to keep
   *  @param until the index one past the last element to keep
   *  @return a tree set with the elements at the given indices
   */
  override def slice(from: Int, until: Int): TreeSet[A] = {
    if (until <= from) empty
    else if (from <= 0) take(until)
    else if (until >= size) drop(from)
    else new TreeSet(RB.slice(tree, from, until))
  }

  /** Returns a tree set containing all elements of this set except the `n` largest.
   *
   *  @param n the number of elements to drop; treated as `0` if negative
   *  @return a tree set without the last `n` elements in ascending order
   */
  override def dropRight(n: Int): TreeSet[A] = take(size - math.max(n, 0))

  /** Returns a tree set containing only the `n` largest elements of this set.
   *
   *  @param n the number of elements to take; treated as `0` if negative
   *  @return a tree set with the last `n` elements in ascending order
   */
  override def takeRight(n: Int): TreeSet[A] = drop(size - math.max(n, 0))

  private def countWhile(p: A => Boolean): Int = {
    var result = 0
    val it = iterator
    while (it.hasNext && p(it.next())) result += 1
    result
  }
  /** Returns a tree set containing all elements of this set except its longest prefix, in
   *  ascending order, of elements that satisfy `p`.
   *
   *  @param p the predicate used to test elements
   *  @return a tree set without the longest prefix of elements satisfying `p`
   */
  override def dropWhile(p: A => Boolean): TreeSet[A] = drop(countWhile(p))

  /** Returns a tree set containing the longest prefix of this set, in ascending order, of
   *  elements that satisfy `p`.
   *
   *  @param p the predicate used to test elements
   *  @return a tree set with the longest prefix of elements satisfying `p`
   */
  override def takeWhile(p: A => Boolean): TreeSet[A] = take(countWhile(p))

  /** Returns a pair of tree sets: the longest prefix of this set, in ascending order, of
   *  elements that satisfy `p`, and the rest of this set.
   *
   *  @param p the predicate used to test elements
   *  @return a pair of the longest prefix of elements satisfying `p` and the remaining elements
   */
  override def span(p: A => Boolean): (TreeSet[A], TreeSet[A]) = splitAt(countWhile(p))

  /** Applies `f` to each element of this tree set, in ascending order.
   *
   *  @tparam U the result type of `f`; the results are discarded
   *  @param f the function to apply to each element
   */
  override def foreach[U](f: A => U): Unit = RB.foreachKey(tree, f)

  /** Returns the smallest element greater than or equal to `key`, if any.
   *
   *  @param key the lower bound (inclusive) for the lookup
   *  @return a `Some` containing the smallest element greater than or equal to `key`,
   *          or `None` if no such element exists
   */
  override def minAfter(key: A): Option[A] = {
    val v = RB.minAfter(tree, key)
    if (v eq null) Option.empty else Some(v.key)
  }

  /** Returns the largest element strictly less than `key`, if any.
   *
   *  @param key the upper bound (exclusive) for the lookup
   *  @return a `Some` containing the largest element strictly less than `key`,
   *          or `None` if no such element exists
   */
  override def maxBefore(key: A): Option[A] = {
    val v = RB.maxBefore(tree, key)
    if (v eq null) Option.empty else Some(v.key)
  }

  /** Returns an iterator over the elements of this tree set, in ascending order. */
  def iterator: Iterator[A] = RB.keysIterator(tree)

  /** Returns an iterator over the elements of this tree set that are greater than or equal to
   *  `start`, in ascending order.
   *
   *  @param start the lower bound (inclusive) on the elements to return
   */
  def iteratorFrom(start: A): Iterator[A] = RB.keysIterator(tree, Some(start))

  /** Returns a stepper over the elements of this tree set, in ascending order.
   *
   *  The returned stepper supports efficient splitting for parallel processing. If `shape`
   *  indicates `Int`, `Long` or `Double` elements, the stepper is a primitive stepper of the
   *  corresponding type, avoiding boxing.
   *
   *  @tparam S the type of the stepper
   *  @param shape an implicit witness selecting the stepper type for element type `A`
   */
  override def stepper[S <: Stepper[?]](implicit shape: StepperShape[A, S]): S & EfficientSplit = {
    import scala.collection.convert.impl._
    type T = RB.Tree[A, Any]
    val s = shape.shape match {
      case StepperShape.IntShape    => IntBinaryTreeStepper.from[T]   (size, tree, _.left, _.right, _.key.asInstanceOf[Int])
      case StepperShape.LongShape   => LongBinaryTreeStepper.from[T]  (size, tree, _.left, _.right, _.key.asInstanceOf[Long])
      case StepperShape.DoubleShape => DoubleBinaryTreeStepper.from[T](size, tree, _.left, _.right, _.key.asInstanceOf[Double])
      case _         => shape.parUnbox(AnyBinaryTreeStepper.from[A, T](size, tree, _.left, _.right, _.key))
    }
    s.asInstanceOf[S & EfficientSplit]
  }

  /** Checks if this set contains element `elem`.
   *
   *  @param  elem    the element to check for membership.
   *  @return true, iff `elem` is contained in this set.
   */
  def contains(elem: A): Boolean = RB.contains(tree, elem)

  /** Returns a tree set containing exactly those elements of this set that are greater than or
   *  equal to `from` and less than `until`.
   *
   *  @param from the lower bound (inclusive) on the elements to keep
   *  @param until the upper bound (exclusive) on the elements to keep
   *  @return a tree set with the elements in the given range
   */
  override def range(from: A, until: A): TreeSet[A] = newSetOrSelf(RB.range(tree, from, until))

  /** Returns a tree set containing exactly those elements of this set that lie within the
   *  given optional bounds.
   *
   *  Unlike the ranged projection of a mutable `TreeSet`, the result is an independent set,
   *  built by extracting the requested range from the underlying tree.
   *
   *  @param from the lower bound (inclusive) wrapped in a `Some`, or `None` if there is no
   *              lower bound
   *  @param until the upper bound (exclusive) wrapped in a `Some`, or `None` if there is no
   *               upper bound
   *  @return a tree set with the elements in the given range; this set itself if neither bound
   *          is given
   */
  def rangeImpl(from: Option[A], until: Option[A]): TreeSet[A] = newSetOrSelf(RB.rangeImpl(tree, from, until))

  /** Creates a new `TreeSet` with the entry added.
   *
   *  @param elem    a new element to add.
   *  @return        a new $coll containing `elem` and all the elements of this $coll.
   */
  def incl(elem: A): TreeSet[A] =
    newSetOrSelf(RB.update(tree, elem, null, overwrite = false))

  /** Creates a new `TreeSet` with the entry removed.
   *
   *  @param elem    a new element to add.
   *  @return        a new $coll containing all the elements of this $coll except `elem`.
   */
  def excl(elem: A): TreeSet[A] =
    newSetOrSelf(RB.delete(tree, elem))

  /** Returns a tree set containing all elements of this set and all elements of `that`.
   *
   *  If `that` is a `TreeSet` with the same ordering, the result is computed by an efficient
   *  tree union; otherwise the elements of `that` are added one by one.
   *
   *  @param that the elements to add
   *  @return a tree set with the combined elements
   */
  override def concat(that: collection.IterableOnce[A]^): TreeSet[A] = {
    val t = (that: @unchecked) match {
      case ts: TreeSet[A] if ordering == ts.ordering =>
        RB.union(tree, ts.tree)
      case _ =>
        val it = that.iterator
        var t = tree
        while (it.hasNext) t = RB.update(t, it.next(), null, overwrite = false)
        t
    }
    newSetOrSelf(t)
  }

  /** Returns a tree set containing all elements of this set that are not in `that`.
   *
   *  If `that` is a `TreeSet` with the same ordering, the result is computed by an efficient
   *  tree difference; otherwise the elements of `that` are removed one by one.
   *
   *  @param that the elements to remove
   *  @return a tree set without the given elements
   */
  override def removedAll(that: IterableOnce[A]^): TreeSet[A] = (that: @unchecked) match {
    case ts: TreeSet[A] if ordering == ts.ordering =>
      newSetOrSelf(RB.difference(tree, ts.tree))
    case _ =>
      //TODO add an implementation of a mutable subtractor similar to TreeMap
      //but at least this doesn't create a TreeSet for each iteration
      object sub extends AbstractFunction1[A, Unit] {
        /** The intermediate tree, with the elements processed so far removed. */
        var currentTree = tree
        /** Removes `k` from the intermediate tree.
         *
         *  @param k the element to remove
         */
        override def apply(k: A): Unit = {
          currentTree = RB.delete(currentTree, k)
        }
      }
      that.iterator.foreach(sub)
      newSetOrSelf(sub.currentTree)
  }

  /** Returns a tree set containing the elements of this set that are also in `that`.
   *
   *  If `that` is a `TreeSet` with the same ordering, the result is computed by an efficient
   *  tree intersection; otherwise this set is filtered by membership in `that`.
   *
   *  @param that the set to intersect with
   *  @return a tree set with the elements common to this set and `that`
   */
  override def intersect(that: collection.Set[A]): TreeSet[A] = that match {
    case ts: TreeSet[A] if ordering == ts.ordering =>
      newSetOrSelf(RB.intersect(tree, ts.tree))
    case _ =>
      super.intersect(that)
  }

  /** Returns a tree set containing the elements of this set that are not in `that`.
   *
   *  If `that` is a `TreeSet` with the same ordering, the result is computed by an efficient
   *  tree difference; otherwise this set is filtered by non-membership in `that`.
   *
   *  @param that the set of elements to exclude
   *  @return a tree set with the elements of this set that are not in `that`
   */
  override def diff(that: collection.Set[A]): TreeSet[A] = that match {
    case ts: TreeSet[A] if ordering == ts.ordering =>
      newSetOrSelf(RB.difference(tree, ts.tree))
    case _ =>
      super.diff(that)
  }

  /** Returns a tree set containing exactly those elements of this set that satisfy the
   *  predicate `f`.
   *
   *  @param f the predicate used to test elements
   *  @return a tree set with the elements satisfying `f`; this set itself if every element
   *          satisfies it
   */
  override def filter(f: A => Boolean): TreeSet[A] = newSetOrSelf(RB.filterEntries[A, Any](tree, {(k, _) => f(k)}))

  /** Returns a pair of tree sets: the elements of this set that satisfy the predicate `p`, and
   *  those that do not.
   *
   *  @param p the predicate used to test elements
   *  @return a pair of tree sets with the elements that satisfy `p` and those that do not
   */
  override def partition(p: A => Boolean): (TreeSet[A], TreeSet[A]) = {
    val (l, r) = RB.partitionEntries(tree, {(a:A, _: Any) => p(a)})
    (newSetOrSelf(l), newSetOrSelf(r))
  }

  /** Returns `true` if `obj` is a set containing the same elements as this set.
   *
   *  If `obj` is a `TreeSet` with the same ordering, this is decided by an efficient structural
   *  comparison in which elements are compared with the ordering; otherwise falls back to the
   *  general set equality, which tests the sizes and then looks each element of this set up in
   *  `obj`. That lookup uses whatever notion of equality `obj` implements, so comparing against
   *  a `TreeSet` with a different ordering compares by that ordering, and the result need not
   *  be symmetric.
   *
   *  @param obj the object to compare with
   */
  override def equals(obj: Any): Boolean = obj match {
    case that: TreeSet[A @unchecked] if ordering == that.ordering => RB.keysEqual(tree, that.tree)
    case _ => super.equals(obj)
  }

  /** The prefix of this set's string representation: `"TreeSet"`. */
  override protected def className = "TreeSet"
}

/** $factoryInfo
 *
 *  @define Coll `immutable.TreeSet`
 *  @define coll immutable tree set
 */
@SerialVersionUID(3L)
object TreeSet extends SortedIterableFactory[TreeSet] {

  /** Returns an empty tree set, using the implicit ordering on elements.
   *
   *  @tparam A the element type of the set, which must have an implicit `Ordering`
   */
  def empty[A: Ordering]: TreeSet[A] = new TreeSet[A]

  /** Returns a tree set containing the elements of `it`, ordered by `ordering`.
   *
   *  If `it` is already a `TreeSet` with the same ordering, `it` itself is returned. If it is
   *  another sorted set with the same ordering, or a `Range` ordered compatibly with
   *  `ordering`, the tree is built directly from its elements in linear time. Otherwise the
   *  elements are inserted one by one, ignoring duplicates.
   *
   *  @tparam E the element type of the set
   *  @param it the collection of elements
   *  @param ordering the ordering used to compare elements
   *  @return a tree set with the elements of `it`
   */
  def from[E](it: scala.collection.IterableOnce[E]^)(implicit ordering: Ordering[E]): TreeSet[E] =
    (it: @unchecked) match {
      case ts: TreeSet[E] if ordering == ts.ordering => ts
      case ss: scala.collection.SortedSet[E] if ordering == ss.ordering =>
        new TreeSet[E](RB.fromOrderedKeys(ss.iterator, ss.size))
      case r: Range if (ordering eq Ordering.Int) || (Ordering.Int isReverseOf ordering) =>
        val it = if((ordering eq Ordering.Int) == (r.step > 0)) r.iterator else r.reverseIterator
        val tree = RB.fromOrderedKeys(it.asInstanceOf[Iterator[E]], r.size)
          // The cast is needed to compile with Dotty:
          // Dotty doesn't infer that E =:= Int, since instantiation of covariant GADTs is unsound
        new TreeSet[E](tree)
      case _ =>
        var t: RB.Tree[E, Null] | Null = null
        val i = it.iterator
        while (i.hasNext) t = RB.update(t, i.next(), null, overwrite = false)
        new TreeSet[E](t)
    }

  /** Returns a new builder for tree sets ordered by `ordering`.
   *
   *  The builder can be reused after calling `result()`.
   *
   *  @tparam A the element type of the sets built
   *  @param ordering the ordering used to compare elements
   *  @return a reusable builder producing a `TreeSet[A]`
   */
  def newBuilder[A](implicit ordering: Ordering[A]): ReusableBuilder[A, TreeSet[A]] = new TreeSetBuilder[A]
  private class TreeSetBuilder[A](implicit ordering: Ordering[A])
    extends RB.SetHelper[A]
      with ReusableBuilder[A, TreeSet[A]] {
    type Tree = RB.Tree[A, Any]
    private var tree:RB.Tree[A, Any] | Null = null

    override def addOne(elem: A): this.type = {
      tree = mutableUpd(tree, elem)
      this
    }

    override def addAll(xs: IterableOnce[A]^): this.type = {
      (xs: @unchecked) match {
        // TODO consider writing a mutable-safe union for TreeSet/TreeMap builder ++=
        // for the moment we have to force immutability before the union
        // which will waste some time and space
        // calling `beforePublish` makes `tree` immutable
        case ts: TreeSet[A] if ts.ordering == ordering =>
          if (tree eq null) tree = ts.tree
          else tree = RB.union(beforePublish(tree), ts.tree)(using ordering)
        case ts: TreeMap[A @unchecked, ?] if ts.ordering == ordering =>
          if (tree eq null) tree = ts.tree0
          else tree = RB.union(beforePublish(tree), ts.tree0)(using ordering)
        case _ =>
          super.addAll(xs)
      }
      this
    }

    override def clear(): Unit = {
      tree = null
    }

    override def result(): TreeSet[A] = new TreeSet[A](beforePublish(tree))(using ordering)
  }
}
