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

import scala.collection.immutable.Set.Set4
import scala.collection.mutable.{Builder, ReusableBuilder}

/** Base trait for immutable set collections.
 *
 *  @tparam A the element type of the set
 */
trait Set[A] extends Iterable[A]
    with collection.Set[A]
    with SetOps[A, Set, Set[A]]
    with IterableFactoryDefaults[A, Set] {
  /** The factory used to build immutable sets, the [[Set$ `Set`]] companion object, which
   *  creates the specialized small sets for up to four elements and a [[HashSet]] beyond that.
   */
  override def iterableFactory: IterableFactory[Set] = Set
}

/** Base trait for immutable set operations
 *
 *  @define coll immutable set
 *  @define Coll `immutable.Set`
 *
 *  @tparam A the element type of the set
 *  @tparam CC the type constructor for the resulting set (e.g., `Set`)
 *  @tparam C the concrete type of this set, returned by transformation operations
 */
transparent trait SetOps[A, +CC[X], +C <: SetOps[A, CC, C]]
  extends collection.SetOps[A, CC, C] {

  /** Creates a new set with an additional element, unless the element is
   *  already present.
   *
   *  @param elem the element to be added
   *  @return a new set that contains all elements of this set and that also
   *          contains `elem`.
   */
  def incl(elem: A): C

  /** Alias for `incl`.
   *
   *  @param elem the element to add
   */
  override final def + (elem: A): C = incl(elem) // like in collection.Set but not deprecated

  /** Creates a new set with a given element removed from this set.
   *
   *  @param elem the element to be removed
   *  @return a new set that contains all elements of this set but that does not
   *          contain `elem`.
   */
  def excl(elem: A): C

  /** Alias for `excl`.
   *
   *  @param elem the element to remove
   */
  @`inline` final override def - (elem: A): C = excl(elem)

  /** Returns a set with the elements of this set that are not in `that`.
   *
   *  The result is built up from the empty set one element at a time, so the concrete
   *  types of the sets involved need not agree.
   *
   *  @param that the set of elements to remove
   *  @return a set with the elements of this set that `that` does not contain
   */
  def diff(that: collection.Set[A]): C =
    foldLeft(empty)((result, elem) => if (that contains elem) result else result + elem)

  /** Creates a new $coll from this $coll by removing all elements of another
   *  collection.
   *
   *  @param that the collection containing the elements to remove.
   *  @return a new $coll with the given elements removed, omitting duplicates.
   */
  def removedAll(that: IterableOnce[A]^): C = that.iterator.foldLeft[C](coll)(_ - _)

  /** Alias for removedAll.
   *
   *  @param that the collection of elements to remove
   */
  override final def -- (that: IterableOnce[A]^): C = removedAll(that)
}

transparent trait StrictOptimizedSetOps[A, +CC[X], +C <: SetOps[A, CC, C]]
  extends SetOps[A, CC, C]
    with collection.StrictOptimizedSetOps[A, CC, C]
    with StrictOptimizedIterableOps[A, CC, C] {

  /** Returns a set with the elements of this set and those of `that`.
   *
   *  The elements of `that` are added to this set one at a time rather than through a
   *  builder, so nothing is copied when `that` is empty.
   *
   *  @param that the elements to add
   *  @return a set containing the elements of both collections
   */
  override def concat(that: collection.IterableOnce[A]^): C = {
    var result: C = coll
    val it = that.iterator
    while (it.hasNext) result = result + it.next()
    result
  }
}

/** $factoryInfo
 *  @define coll immutable set
 *  @define Coll `immutable.Set`
 */
@SerialVersionUID(3L)
object Set extends IterableFactory[Set] {

  /** Returns the empty immutable set.
   *
   *  All calls return the same instance, cast to the requested element type.
   *
   *  @tparam A the element type of the set
   */
  def empty[A]: Set[A] = EmptySet.asInstanceOf[Set[A]]

  /** Returns an immutable set containing the elements of `it`, with duplicates left out.
   *
   *  A collection that is already one of the immutable set implementations reached by
   *  this factory is returned unchanged. Those are the unordered sets and the key sets
   *  of immutable maps, so a key set of an insertion-ordered map such as `ListMap` or
   *  `VectorMap` keeps that map's iteration order. Anything else, including a sorted
   *  set, is copied into a new set, so that the result cannot keep an ordering that its
   *  static element type no longer justifies.
   *
   *  @tparam E the element type
   *  @param it the collection whose elements are to be contained
   */
  def from[E](it: collection.IterableOnce[E]^): Set[E] =
    (it: @unchecked) match {
      case _ if it.knownSize == 0 => empty[E]
      // Since IterableOnce[E] launders the variance of E,
      // identify only our implementations which can be soundly substituted.
      // It's not sufficient to match `SortedSet[E]` to rebuild and `Set[E]` to retain.
      case s: HashSet[E] => s
      case s: ListSet[E] => s
      case s: Set1[E]    => s
      case s: Set2[E]    => s
      case s: Set3[E]    => s
      case s: Set4[E]    => s
      case s: HashMap[E @unchecked, ?]#HashKeySet => s
      case s: MapOps[E, Any, Map, Map[E, Any]]#LazyImmutableKeySet @unchecked => s
      // We also want `SortedSet` (and subclasses, such as `BitSet`)
      // to rebuild themselves, to avoid element type widening issues.
      case _ => newBuilder[E].addAll(it).result()
    }

  /** Returns a new builder for an immutable set.
   *
   *  @tparam A the element type of the set being built
   *  @return a builder that produces the specialized small sets for up to four distinct
   *          elements, and a `HashSet` beyond that
   */
  def newBuilder[A]: Builder[A, Set[A]] = new SetBuilderImpl[A]

  /** An optimized representation for immutable empty sets. */
  @SerialVersionUID(3L)
  private object EmptySet extends AbstractSet[Any] with Serializable {
    /** Returns `0`: this set has no elements. */
    override def size: Int = 0
    /** Returns `true`: this is the empty set. */
    override def isEmpty = true
    /** Returns `0`: the size is always known. */
    override def knownSize: Int = size
    /** Returns this set itself: there is no element for `pred` to keep.
     *
     *  @param pred the predicate to test; never applied
     */
    override def filter(pred: Any => Boolean): Set[Any] = this
    /** Returns this set itself: there is no element for `pred` to drop.
     *
     *  @param pred the predicate to test; never applied
     */
    override def filterNot(pred: Any => Boolean): Set[Any] = this
    /** Returns this set itself: the empty set has nothing to remove.
     *
     *  @param that the elements to remove; never iterated
     */
    override def removedAll(that: IterableOnce[Any]^): Set[Any] = this
    /** Returns this set itself: the empty set has nothing to remove.
     *
     *  @param that the set of elements to remove; never used
     */
    override def diff(that: collection.Set[Any]): Set[Any] = this
    /** Returns `true`: the empty set is a subset of every set.
     *
     *  @param that the set to test against; never used
     */
    override def subsetOf(that: collection.Set[Any]): Boolean = true
    /** Returns this set itself: nothing can be in common with the empty set.
     *
     *  @param that the set to intersect with; never used
     */
    override def intersect(that: collection.Set[Any]): Set[Any] = this
    /** Returns the empty view. */
    override def view: View[Any] = View.empty
    /** Returns `false`: the empty set contains nothing.
     *
     *  @param elem the element to test; never used
     */
    def contains(elem: Any): Boolean = false
    /** Returns a new `Set1` containing `elem` alone.
     *
     *  @param elem the element to add
     */
    def incl(elem: Any): Set[Any] = new Set1(elem)
    /** Returns this set itself: the empty set has no element to remove.
     *
     *  @param elem the element to remove; never used
     */
    def excl(elem: Any): Set[Any] = this
    /** Returns the empty iterator: this set has no elements. */
    def iterator: Iterator[Any] = Iterator.empty
    /** Does nothing: there is no element to apply `f` to.
     *
     *  @tparam U the result type of `f`, used only for its side effects
     *  @param f the function to apply; never called
     */
    override def foreach[U](f: Any => U): Unit = ()
  }
  private[collection] def emptyInstance: Set[Any] = EmptySet

  @SerialVersionUID(3L)
  private abstract class SetNIterator[A](n: Int) extends AbstractIterator[A] with Serializable {
    private var current = 0
    private var remainder = n
    /** Returns the number of elements this iterator has left to produce. */
    override def knownSize: Int = remainder
    /** Returns `true` while this iterator has elements left to produce. */
    def hasNext = remainder > 0
    /** Returns the element of the underlying set at position `i`.
     *
     *  @param i the position of the element, counting from `0`
     *  @return the element the underlying set holds at position `i`
     */
    def apply(i: Int): A
    /** Returns the next element of the underlying set and advances this iterator.
     *
     *  @throws NoSuchElementException if this iterator has no elements left
     */
    def next(): A =
      if (hasNext) {
        val r = apply(current)
        current += 1
        remainder -= 1
        r
      } else Iterator.empty.next()

    /** Advances this iterator past the next `n` elements and returns this same iterator,
     *  without creating an intermediate one.
     *
     *  A non-positive `n` skips nothing, and skipping past the end leaves an exhausted
     *  iterator rather than a negative count.
     *
     *  @param n the number of elements to skip
     *  @return this iterator
     */
    override def drop(n: Int): Iterator[A] = {
      if (n > 0) {
        current += n
        remainder = Math.max(0, remainder - n)
      }
      this
    }
  }

  /** An optimized representation for immutable sets of size 1. */
  @SerialVersionUID(3L)
  final class Set1[A] private[collection] (elem1: A) extends AbstractSet[A] with StrictOptimizedIterableOps[A, Set, Set[A]] with Serializable {
    /** Returns `1`: this set has exactly one element. */
    override def size: Int = 1
    /** Returns `false`: this set always has one element. */
    override def isEmpty = false
    /** Returns `1`: the size is always known. */
    override def knownSize: Int = size
    /** Returns `true` if `elem` is the element of this set, comparing with `==`.
     *
     *  @param elem the element to look for
     */
    def contains(elem: A): Boolean = elem == elem1
    /** Returns a set containing `elem` and the element of this set.
     *
     *  Returns this set itself if it already contains `elem`; otherwise a `Set2` with
     *  `elem` after the element already present.
     *
     *  @param elem the element to add
     */
    def incl(elem: A): Set[A] =
      if (contains(elem)) this
      else new Set2(elem1, elem)
    /** Returns a set containing the element of this set except `elem`.
     *
     *  Returns the empty set if `elem` is the element of this set, and this set itself
     *  otherwise.
     *
     *  @param elem the element to remove
     */
    def excl(elem: A): Set[A] =
      if (elem == elem1) Set.empty
      else this
    /** Returns an iterator over the single element of this set. */
    def iterator: Iterator[A] = Iterator.single(elem1)
    /** Applies `f` to each element of this set, in the order they are held.
     *
     *  @tparam U the result type of `f`, used only for its side effects
     *  @param f the function to apply to each element
     */
    override def foreach[U](f: A => U): Unit = f(elem1)
    /** Returns `true` if `p` holds for at least one element of this set.
     *
     *  The elements are tested in the order they are held, and testing stops at the
     *  first one that satisfies `p`.
     *
     *  @param p the predicate used to test elements
     */
    override def exists(p: A => Boolean): Boolean = p(elem1)
    /** Returns `true` if `p` holds for every element of this set.
     *
     *  The elements are tested in the order they are held, and testing stops at the
     *  first one that fails `p`.
     *
     *  @param p the predicate used to test elements
     */
    override def forall(p: A => Boolean): Boolean = p(elem1)
    /** Returns a set of the elements of this set that satisfy `pred`, or that fail to
     *  satisfy it when `isFlipped` is `true`.
     *
     *  Returns this set itself if its element passes the test, and the empty set
     *  otherwise.
     *
     *  @param pred the predicate used to test elements
     *  @param isFlipped if `true`, keeps the elements that do not satisfy `pred`
     *  @return a set of the elements that pass the test
     */
    override protected[collection] def filterImpl(pred: A => Boolean, isFlipped: Boolean): Set[A] =
      if (pred(elem1) != isFlipped) this else Set.empty

    /** Returns the first element satisfying `p` wrapped in a `Some`, or `None` if there
     *  is none.
     *
     *  The elements are tested in the order they are held, and testing stops at the
     *  first match.
     *
     *  @param p the predicate used to test elements
     */
    override def find(p: A => Boolean): Option[A] =
      if (p(elem1)) Some(elem1)
      else None
    /** Returns the first element this set holds, which is the first its iterator gives out. */
    override def head: A = elem1
    /** Returns this set without its first element, the empty set. */
    override def tail: Set[A] = Set.empty
  }

  /** An optimized representation for immutable sets of size 2. */
  @SerialVersionUID(3L)
  final class Set2[A] private[collection] (elem1: A, elem2: A) extends AbstractSet[A] with StrictOptimizedIterableOps[A, Set, Set[A]] with Serializable {
    /** Returns `2`: this set has exactly two elements. */
    override def size: Int = 2
    /** Returns `false`: this set always has two elements. */
    override def isEmpty = false
    /** Returns `2`: the size is always known. */
    override def knownSize: Int = size
    /** Returns `true` if `elem` is one of the elements of this set, comparing with `==`.
     *
     *  @param elem the element to look for
     */
    def contains(elem: A): Boolean = elem == elem1 || elem == elem2
    /** Returns a set containing `elem` and the elements of this set.
     *
     *  Returns this set itself if it already contains `elem`; otherwise a `Set3` with
     *  `elem` after the elements already present.
     *
     *  @param elem the element to add
     */
    def incl(elem: A): Set[A] =
      if (contains(elem)) this
      else new Set3(elem1, elem2, elem)
    /** Returns a set containing the elements of this set except `elem`.
     *
     *  Returns a `Set1` with the remaining elements in their current order if `elem` is
     *  one of them, and this set itself otherwise.
     *
     *  @param elem the element to remove
     */
    def excl(elem: A): Set[A] =
      if (elem == elem1) new Set1(elem2)
      else if (elem == elem2) new Set1(elem1)
      else this
    /** Returns an iterator over the two elements of this set, in the order they are held. */
    def iterator: Iterator[A] = new SetNIterator[A](size) {
      def apply(i: Int) = getElem(i)
    }
    private def getElem(i: Int) = i match { case 0 => elem1 case 1 => elem2 }

    /** Applies `f` to each element of this set, in the order they are held.
     *
     *  @tparam U the result type of `f`, used only for its side effects
     *  @param f the function to apply to each element
     */
    override def foreach[U](f: A => U): Unit = {
      f(elem1); f(elem2)
    }
    /** Returns `true` if `p` holds for at least one element of this set.
     *
     *  The elements are tested in the order they are held, and testing stops at the
     *  first one that satisfies `p`.
     *
     *  @param p the predicate used to test elements
     */
    override def exists(p: A => Boolean): Boolean = {
      p(elem1) || p(elem2)
    }
    /** Returns `true` if `p` holds for every element of this set.
     *
     *  The elements are tested in the order they are held, and testing stops at the
     *  first one that fails `p`.
     *
     *  @param p the predicate used to test elements
     */
    override def forall(p: A => Boolean): Boolean = {
      p(elem1) && p(elem2)
    }
    /** Returns a set of the elements of this set that satisfy `pred`, or that fail to
     *  satisfy it when `isFlipped` is `true`.
     *
     *  The elements that pass keep their current relative order, and the result is this
     *  set itself when they all pass, the empty set when none does, and the specialized
     *  set of the matching size in between.
     *
     *  @param pred the predicate used to test elements
     *  @param isFlipped if `true`, keeps the elements that do not satisfy `pred`
     *  @return a set of the elements that pass the test
     */
    override protected[collection] def filterImpl(pred: A => Boolean, isFlipped: Boolean): Set[A] = {
      var r1: A = null.asInstanceOf[A]
      var n = 0
      if (pred(elem1) != isFlipped) {             r1 = elem1; n += 1}
      if (pred(elem2) != isFlipped) { if (n == 0) r1 = elem2; n += 1}

      n match {
        case 0 => Set.empty
        case 1 => new Set1(r1)
        case 2 => this
      }
    }
    /** Returns the first element satisfying `p` wrapped in a `Some`, or `None` if there
     *  is none.
     *
     *  The elements are tested in the order they are held, and testing stops at the
     *  first match.
     *
     *  @param p the predicate used to test elements
     */
    override def find(p: A => Boolean): Option[A] = {
      if (p(elem1)) Some(elem1)
      else if (p(elem2)) Some(elem2)
      else None
    }
    /** Returns the first element this set holds, which is the first its iterator gives out. */
    override def head: A = elem1
    /** Returns this set without its first element, a `Set1` of the remaining element. */
    override def tail: Set[A] = new Set1(elem2)
  }

  /** An optimized representation for immutable sets of size 3. */
  @SerialVersionUID(3L)
  final class Set3[A] private[collection] (elem1: A, elem2: A, elem3: A) extends AbstractSet[A] with StrictOptimizedIterableOps[A, Set, Set[A]] with Serializable {
    /** Returns `3`: this set has exactly three elements. */
    override def size: Int = 3
    /** Returns `false`: this set always has three elements. */
    override def isEmpty = false
    /** Returns `3`: the size is always known. */
    override def knownSize: Int = size
    /** Returns `true` if `elem` is one of the elements of this set, comparing with `==`.
     *
     *  @param elem the element to look for
     */
    def contains(elem: A): Boolean =
      elem == elem1 || elem == elem2 || elem == elem3
    /** Returns a set containing `elem` and the elements of this set.
     *
     *  Returns this set itself if it already contains `elem`; otherwise a `Set4` with
     *  `elem` after the elements already present.
     *
     *  @param elem the element to add
     */
    def incl(elem: A): Set[A] =
      if (contains(elem)) this
      else new Set4(elem1, elem2, elem3, elem)
    /** Returns a set containing the elements of this set except `elem`.
     *
     *  Returns a `Set2` with the remaining elements in their current order if `elem` is
     *  one of them, and this set itself otherwise.
     *
     *  @param elem the element to remove
     */
    def excl(elem: A): Set[A] =
      if (elem == elem1) new Set2(elem2, elem3)
      else if (elem == elem2) new Set2(elem1, elem3)
      else if (elem == elem3) new Set2(elem1, elem2)
      else this
    /** Returns an iterator over the three elements of this set, in the order they are held. */
    def iterator: Iterator[A] = new SetNIterator[A](size) {
      def apply(i: Int) = getElem(i)
    }
    private def getElem(i: Int) = i match { case 0 => elem1 case 1 => elem2 case 2 => elem3 }

    /** Applies `f` to each element of this set, in the order they are held.
     *
     *  @tparam U the result type of `f`, used only for its side effects
     *  @param f the function to apply to each element
     */
    override def foreach[U](f: A => U): Unit = {
      f(elem1); f(elem2); f(elem3)
    }
    /** Returns `true` if `p` holds for at least one element of this set.
     *
     *  The elements are tested in the order they are held, and testing stops at the
     *  first one that satisfies `p`.
     *
     *  @param p the predicate used to test elements
     */
    override def exists(p: A => Boolean): Boolean = {
      p(elem1) || p(elem2) || p(elem3)
    }
    /** Returns `true` if `p` holds for every element of this set.
     *
     *  The elements are tested in the order they are held, and testing stops at the
     *  first one that fails `p`.
     *
     *  @param p the predicate used to test elements
     */
    override def forall(p: A => Boolean): Boolean = {
      p(elem1) && p(elem2) && p(elem3)
    }
    /** Returns a set of the elements of this set that satisfy `pred`, or that fail to
     *  satisfy it when `isFlipped` is `true`.
     *
     *  The elements that pass keep their current relative order, and the result is this
     *  set itself when they all pass, the empty set when none does, and the specialized
     *  set of the matching size in between.
     *
     *  @param pred the predicate used to test elements
     *  @param isFlipped if `true`, keeps the elements that do not satisfy `pred`
     *  @return a set of the elements that pass the test
     */
    override protected[collection] def filterImpl(pred: A => Boolean, isFlipped: Boolean): Set[A] = {
      var r1, r2: A = null.asInstanceOf[A]
      var n = 0
      if (pred(elem1) != isFlipped) {             r1 = elem1;                             n += 1}
      if (pred(elem2) != isFlipped) { if (n == 0) r1 = elem2 else             r2 = elem2; n += 1}
      if (pred(elem3) != isFlipped) { if (n == 0) r1 = elem3 else if (n == 1) r2 = elem3; n += 1}

      n match {
        case 0 => Set.empty
        case 1 => new Set1(r1)
        case 2 => new Set2(r1, r2)
        case 3 => this
      }
    }
    /** Returns the first element satisfying `p` wrapped in a `Some`, or `None` if there
     *  is none.
     *
     *  The elements are tested in the order they are held, and testing stops at the
     *  first match.
     *
     *  @param p the predicate used to test elements
     */
    override def find(p: A => Boolean): Option[A] = {
      if (p(elem1)) Some(elem1)
      else if (p(elem2)) Some(elem2)
      else if (p(elem3)) Some(elem3)
      else None
    }
    /** Returns the first element this set holds, which is the first its iterator gives out. */
    override def head: A = elem1
    /** Returns this set without its first element, a `Set2` of the remaining elements. */
    override def tail: Set[A] = new Set2(elem2, elem3)
  }

  /** An optimized representation for immutable sets of size 4. */
  @SerialVersionUID(3L)
  final class Set4[A] private[collection] (elem1: A, elem2: A, elem3: A, elem4: A) extends AbstractSet[A] with StrictOptimizedIterableOps[A, Set, Set[A]] with Serializable {
    /** Returns `4`: this set has exactly four elements. */
    override def size: Int = 4
    /** Returns `false`: this set always has four elements. */
    override def isEmpty = false
    /** Returns `4`: the size is always known. */
    override def knownSize: Int = size
    /** Returns `true` if `elem` is one of the elements of this set, comparing with `==`.
     *
     *  @param elem the element to look for
     */
    def contains(elem: A): Boolean =
      elem == elem1 || elem == elem2 || elem == elem3 || elem == elem4
    /** Returns a set containing `elem` and the elements of this set.
     *
     *  Returns this set itself if it already contains `elem`; otherwise a [[HashSet]],
     *  since the specialized small sets stop at four elements. The order in which the
     *  elements are iterated is then the one the hash set gives them, not this one.
     *
     *  @param elem the element to add
     */
    def incl(elem: A): Set[A] =
      if (contains(elem)) this
      else HashSet.empty[A] + elem1 + elem2 + elem3 + elem4 + elem
    /** Returns a set containing the elements of this set except `elem`.
     *
     *  Returns a `Set3` with the remaining elements in their current order if `elem` is
     *  one of them, and this set itself otherwise.
     *
     *  @param elem the element to remove
     */
    def excl(elem: A): Set[A] =
      if (elem == elem1) new Set3(elem2, elem3, elem4)
      else if (elem == elem2) new Set3(elem1, elem3, elem4)
      else if (elem == elem3) new Set3(elem1, elem2, elem4)
      else if (elem == elem4) new Set3(elem1, elem2, elem3)
      else this
    /** Returns an iterator over the four elements of this set, in the order they are held. */
    def iterator: Iterator[A] = new SetNIterator[A](size) {
      def apply(i: Int) = getElem(i)
    }
    private def getElem(i: Int) = i match { case 0 => elem1 case 1 => elem2 case 2 => elem3 case 3 => elem4 }

    /** Applies `f` to each element of this set, in the order they are held.
     *
     *  @tparam U the result type of `f`, used only for its side effects
     *  @param f the function to apply to each element
     */
    override def foreach[U](f: A => U): Unit = {
      f(elem1); f(elem2); f(elem3); f(elem4)
    }
    /** Returns `true` if `p` holds for at least one element of this set.
     *
     *  The elements are tested in the order they are held, and testing stops at the
     *  first one that satisfies `p`.
     *
     *  @param p the predicate used to test elements
     */
    override def exists(p: A => Boolean): Boolean = {
      p(elem1) || p(elem2) || p(elem3) || p(elem4)
    }
    /** Returns `true` if `p` holds for every element of this set.
     *
     *  The elements are tested in the order they are held, and testing stops at the
     *  first one that fails `p`.
     *
     *  @param p the predicate used to test elements
     */
    override def forall(p: A => Boolean): Boolean = {
      p(elem1) && p(elem2) && p(elem3) && p(elem4)
    }
    /** Returns a set of the elements of this set that satisfy `pred`, or that fail to
     *  satisfy it when `isFlipped` is `true`.
     *
     *  The elements that pass keep their current relative order, and the result is this
     *  set itself when they all pass, the empty set when none does, and the specialized
     *  set of the matching size in between.
     *
     *  @param pred the predicate used to test elements
     *  @param isFlipped if `true`, keeps the elements that do not satisfy `pred`
     *  @return a set of the elements that pass the test
     */
    override protected[collection] def filterImpl(pred: A => Boolean, isFlipped: Boolean): Set[A] = {
      var r1, r2, r3: A = null.asInstanceOf[A]
      var n = 0
      if (pred(elem1) != isFlipped) {             r1 = elem1;                                                         n += 1}
      if (pred(elem2) != isFlipped) { if (n == 0) r1 = elem2 else             r2 = elem2;                             n += 1}
      if (pred(elem3) != isFlipped) { if (n == 0) r1 = elem3 else if (n == 1) r2 = elem3 else             r3 = elem3; n += 1}
      if (pred(elem4) != isFlipped) { if (n == 0) r1 = elem4 else if (n == 1) r2 = elem4 else if (n == 2) r3 = elem4; n += 1}

      n match {
        case 0 => Set.empty
        case 1 => new Set1(r1)
        case 2 => new Set2(r1, r2)
        case 3 => new Set3(r1, r2, r3)
        case 4 => this
      }
    }

    /** Returns the first element satisfying `p` wrapped in a `Some`, or `None` if there
     *  is none.
     *
     *  The elements are tested in the order they are held, and testing stops at the
     *  first match.
     *
     *  @param p the predicate used to test elements
     */
    override def find(p: A => Boolean): Option[A] = {
      if (p(elem1)) Some(elem1)
      else if (p(elem2)) Some(elem2)
      else if (p(elem3)) Some(elem3)
      else if (p(elem4)) Some(elem4)
      else None
    }
    /** Returns the first element this set holds, which is the first its iterator gives out. */
    override def head: A = elem1
    /** Returns this set without its first element, a `Set3` of the remaining elements. */
    override def tail: Set[A] = new Set3(elem2, elem3, elem4)

    private[immutable] def buildTo(builder: Builder[A, Set[A]]): builder.type =
      builder.addOne(elem1).addOne(elem2).addOne(elem3).addOne(elem4)
  }
}

/** Explicit instantiation of the `Set` trait to reduce class file size in subclasses.
 *
 *  @tparam A the element type of the set
 */
abstract class AbstractSet[A] extends scala.collection.AbstractSet[A] with Set[A]

/** Builder for Set.
 *  $multipleResults
 *
 *  @tparam A the element type of the set being built
 */
private final class SetBuilderImpl[A] extends ReusableBuilder[A, Set[A]] {
  private var elems: Set[A] = Set.empty
  private var switchedToHashSetBuilder: Boolean = false
  private var hashSetBuilder: HashSetBuilder[A] = compiletime.uninitialized

  /** Discards everything added so far, so that this builder starts again from the empty
   *  set, keeping any hash set builder it has already allocated for reuse.
   */
  override def clear(): Unit = {
    elems = Set.empty
    if (hashSetBuilder != null) {
      hashSetBuilder.clear()
    }
    switchedToHashSetBuilder = false
  }

  /** Returns the set built so far: one of the specialized small sets while at most four
   *  distinct elements have been added, and a `HashSet` once more have.
   */
  override def result(): Set[A] =
    if (switchedToHashSetBuilder) hashSetBuilder.result() else elems

  /** Adds `elem` to this builder, leaving the set unchanged if it is already present.
   *
   *  The fifth distinct element makes this builder move the four it holds into a hash
   *  set builder and keep going there.
   *
   *  @param elem the element to add
   *  @return this builder
   */
  def addOne(elem: A) = {
    if (switchedToHashSetBuilder) {
      hashSetBuilder.addOne(elem)
    } else if (elems.size < 4) {
      elems = elems + elem
    } else {
      // assert(elems.size == 4)
      if (elems.contains(elem)) {
        () // do nothing
      } else {
        switchedToHashSetBuilder = true
        if (hashSetBuilder == null) {
          hashSetBuilder = new HashSetBuilder
        }
        elems.asInstanceOf[Set4[A]].buildTo(hashSetBuilder)
        hashSetBuilder.addOne(elem)
      }
    }

    this
  }

  /** Adds all elements of `xs` to this builder, leaving out the ones already present.
   *
   *  Once this builder has moved to a hash set builder, `xs` is handed to it in one go
   *  rather than element by element.
   *
   *  @param xs the elements to add
   *  @return this builder
   */
  override def addAll(xs: IterableOnce[A]^): this.type =
    if (switchedToHashSetBuilder) {
      hashSetBuilder.addAll(xs)
      this
    } else {
      super.addAll(xs)
    }
}
