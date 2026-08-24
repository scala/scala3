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

import scala.language.`2.13`
import language.experimental.captureChecking

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable.{ArrayBuffer, Builder}
import scala.collection.immutable.LazyList
import scala.runtime.ScalaRunTime.nullForGC
import caps.unsafe.unsafeAssumePure

/** Views are collections whose transformation operations are non strict: the resulting elements
 *  are evaluated only when the view is effectively traversed (e.g. using `foreach` or `foldLeft`),
 *  or when the view is converted to a strict collection type (using the `to` operation).
 *  @define coll view
 *  @define Coll `View`
 *
 *  @tparam A the element type of the view
 */
trait View[+A] extends Iterable[A] with IterableOps[A, View, View[A]] with IterableFactoryDefaults[A, View] with Serializable {

  /** Returns this view unchanged: a view is already a view. */
  override def view: View[A]^{this} = this

  /** Returns the companion object `View`, used as the factory for views of any element type. */
  override def iterableFactory: IterableFactory[View] = View

  /** Returns an empty view. */
  override def empty: scala.collection.View[A] = iterableFactory.empty

  /** Returns a string consisting of this view's class name followed by `(<not computed>)`.
   *
   *  The elements of this view are never evaluated by `toString`.
   */
  override def toString(): String  = className + "(<not computed>)"

  /** Returns `"View"`, the prefix used in the string representation of this view. */
  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected def stringPrefix: String = "View"

  @deprecated("Views no longer know about their underlying collection type; .force always returns an IndexedSeq", "2.13.0")
  @`inline` def force: IndexedSeq[A] = toIndexedSeq
}

/** This object reifies operations on views as case classes
 *
 *  @define Coll View
 *  @define coll view
 */
@SerialVersionUID(3L)
object View extends IterableFactory[View] {

  /**
   *  @tparam A View element type
   *
   *  @param it Function creating the iterator to be used by the view. This function must always return
   *           a fresh `Iterator`, otherwise the resulting view will be effectively iterable only once.
   *
   *  @return A `View[A]` whose underlying iterator is provided by the `it` parameter-less function.
   */
  def fromIteratorProvider[A](it: () => Iterator[A]^): View[A]^{it} = new AbstractView[A] {
    def iterator = it()
  }

  /**
   *  @tparam E View element type
   *
   *  @param it The `IterableOnce` to view. A proper `Iterable` is used directly. If it is really only
   *           `IterableOnce` it gets memoized on the first traversal.
   *
   *  @return A view iterating over the given `Iterable`
   */
  def from[E](it: IterableOnce[E]^): View[E]^{it} = it match {
    case it: (View[E @unchecked]^{it})     => it
    case it: (Iterable[E @unchecked]^{it}) => View.fromIteratorProvider(() => it.iterator)
    case _                      => LazyList.from(it).view
  }

  /** Returns the empty view, [[View.Empty]].
   *
   *  @tparam A the element type of the view; no elements are ever produced
   */
  def empty[A]: View[A] = Empty

  /** Returns a new builder for a view.
   *
   *  The builder is strict: elements added to it are buffered eagerly, and
   *  `result()` returns a view over the buffered elements.
   *
   *  @tparam A the element type of the view to build
   *  @return a builder that buffers the added elements and returns a view of them
   */
  def newBuilder[A]: Builder[A, View[A]] = ArrayBuffer.newBuilder[A].mapResult(from)

  /** Returns a view of the given elements.
   *
   *  @tparam A the element type of the view
   *  @param xs the elements of the view
   *  @return a view iterating over `xs`
   */
  override def apply[A](xs: A*): View[A] = new Elems(xs*)

  /** The empty view. */
  @SerialVersionUID(3L)
  case object Empty extends AbstractView[Nothing] {
    /** Returns the empty iterator. */
    def iterator = Iterator.empty
    /** Returns 0: this view never has elements. */
    override def knownSize = 0
    /** Returns `true`: this view is always empty. */
    override def isEmpty: Boolean = true
  }

  /** A view with exactly one element. */
  @SerialVersionUID(3L)
  class Single[A](a: A) extends AbstractView[A] {
    /** Returns an iterator producing the single element `a`. */
    def iterator: Iterator[A] = Iterator.single(a)
    /** Returns 1: this view always has exactly one element. */
    override def knownSize: Int = 1
    /** Returns `false`: this view always has one element. */
    override def isEmpty: Boolean = false
  }

  /** A view with given elements. */
  @SerialVersionUID(3L)
  class Elems[A](xs: A*) extends AbstractView[A] {
    /** Returns an iterator over the given elements. */
    def iterator = xs.iterator
    /** Returns the number of given elements, or -1 if the argument sequence does not know its size. */
    override def knownSize = xs.knownSize
    /** Returns `true` if no elements were given. */
    override def isEmpty: Boolean = xs.isEmpty
  }

  /** A view containing the results of some element computation a number of times. */
  @SerialVersionUID(3L)
  class Fill[A](n: Int)(elem: => A) extends AbstractView[A] {
    /** Returns an iterator that evaluates `elem` anew for each of the `n` elements it produces. */
    def iterator = Iterator.fill(n)(elem)
    /** Returns `n`, or 0 if `n` is negative. */
    override def knownSize: Int = 0 max n
    /** Returns `true` if `n <= 0`; `elem` is not evaluated. */
    override def isEmpty: Boolean = n <= 0
  }

  /** A view containing values of a given function over a range of integer values starting from 0. */
  @SerialVersionUID(3L)
  class Tabulate[A](n: Int)(f: Int => A) extends AbstractView[A] {
    /** Returns an iterator that applies `f` on demand to each index from 0 until `n`. */
    def iterator: Iterator[A]^{f} = Iterator.tabulate(n)(f)
    /** Returns `n`, or 0 if `n` is negative. */
    override def knownSize: Int = 0 max n
    /** Returns `true` if `n <= 0`; `f` is not called. */
    override def isEmpty: Boolean = n <= 0
  }

  /** A view containing repeated applications of a function to a start value. */
  @SerialVersionUID(3L)
  class Iterate[A](start: A, len: Int)(f: A => A) extends AbstractView[A] {
    /** Returns an iterator producing on demand `start`, `f(start)`, `f(f(start))`, ..., limited to `len` elements. */
    def iterator: Iterator[A]^{f} = Iterator.iterate(start)(f).take(len)
    /** Returns `len`, or 0 if `len` is negative. */
    override def knownSize: Int = 0 max len
    /** Returns `true` if `len <= 0`; `f` is not called. */
    override def isEmpty: Boolean = len <= 0
  }

  /** A view that uses a function `f` to produce elements of type `A` and update
   *  an internal state `S`.
   */
  @SerialVersionUID(3L)
  class Unfold[A, S](initial: S)(f: S => Option[(A, S)]) extends AbstractView[A] {
    /** Returns an iterator that, on demand, applies `f` to the current state, yielding the
     *  element and next state of each `Some` result and ending when `f` returns `None`.
     */
    def iterator: Iterator[A]^{f} = Iterator.unfold(initial)(f)
  }

  /** An `IterableOps` whose collection type and collection type constructor are unknown. */
  type SomeIterableOps[A] = IterableOps[A, AnyConstr, ?]

  /** A view that filters an underlying collection. */
  @SerialVersionUID(3L)
  class Filter[A](val underlying: SomeIterableOps[A]^, val p: A => Boolean, val isFlipped: Boolean) extends AbstractView[A] {
    /** Returns an iterator that evaluates `p` on demand and produces the underlying elements
     *  that satisfy `p`, or that fail `p` if `isFlipped` is `true`.
     */
    def iterator = underlying.iterator.filterImpl(p, isFlipped)
    /** Returns 0 if the underlying collection is known to be empty, -1 otherwise
     *  (how many elements pass the filter cannot be known without evaluating `p`).
     */
    override def knownSize: Int = if (underlying.knownSize == 0) 0 else super.knownSize
    /** Returns `true` if no element passes the filter, evaluating `p` on the underlying
     *  elements until a first match is found.
     */
    override def isEmpty: Boolean = iterator.isEmpty
  }

  object Filter {
    /** Returns a view that filters `underlying` with `p`.
     *
     *  If `underlying` is itself a `Filter` with the same `isFlipped` polarity, the two
     *  predicates are combined by conjunction into a single `Filter` over the original
     *  collection instead of stacking two view layers.
     *
     *  @tparam A the element type of the view
     *  @param underlying the collection to filter
     *  @param p the predicate to apply to each element
     *  @param isFlipped if `true`, keeps the elements that fail `p` instead of those that satisfy it
     *  @return a view of the underlying elements selected by `p` and `isFlipped`
     */
    def apply[A](underlying: Iterable[A]^, p: A => Boolean, isFlipped: Boolean): Filter[A]^{underlying, p} =
      (underlying: @unchecked) match {
        case filter : Filter[A] if filter.isFlipped == isFlipped => {
          val f: Filter[A]^{underlying} = filter.unsafeAssumePure // TODO remove when pattern matching works
          val f2: Filter[A]^{f, p} = new Filter(f.underlying, a => f.p(a) && p(a), isFlipped)
          f2
        }
        case _ => new Filter(underlying, p, isFlipped)
      }
  }

  /** A view that removes the duplicated elements as determined by the transformation function `f`. */
  @SerialVersionUID(3L)
  class DistinctBy[A, B](underlying: SomeIterableOps[A]^, f: A -> B) extends AbstractView[A] {
    /** Returns an iterator over the underlying elements, skipping any element whose image
     *  under `f` equals that of an already produced element.
     */
    def iterator: Iterator[A]^{underlying} = underlying.iterator.distinctBy(f)
    /** Returns 0 if the underlying collection is known to be empty, -1 otherwise
     *  (the number of distinct elements cannot be known without evaluating `f`).
     */
    override def knownSize: Int = if (underlying.knownSize == 0) 0 else super.knownSize
    /** Returns `true` if the underlying collection is empty; `f` is not called. */
    override def isEmpty: Boolean = underlying.isEmpty
  }

  /** A view of the `Left` values produced by applying `f` to the elements of an
   *  underlying collection; elements for which `f` yields a `Right` are skipped.
   *
   *  @tparam A the element type of the underlying collection
   *  @tparam A1 the `Left` type of the results of `f`, and the element type of this view
   *  @tparam A2 the `Right` type of the results of `f`; values of this type are discarded
   *  @param underlying the collection whose elements are partitioned
   *  @param f the function applied on demand to each underlying element
   */
  @SerialVersionUID(3L)
  class LeftPartitionMapped[A, A1, A2](underlying: SomeIterableOps[A]^, f: A => Either[A1, A2]) extends AbstractView[A1] {
    /** Returns an iterator that applies `f` on demand to the underlying elements and
     *  produces the value inside each `Left` result, skipping `Right` results.
     */
    def iterator: AbstractIterator[A1]^{this} = new AbstractIterator[A1] {
      private val self = underlying.iterator
      private var hd: A1 = compiletime.uninitialized
      private var hdDefined: Boolean = false
      def hasNext = hdDefined || {
        @tailrec
        def findNext(): Boolean =
          if (self.hasNext) {
            f(self.next()) match {
              case Left(a1) => hd = a1; hdDefined = true; true
              case Right(_) => findNext()
            }
          } else false
        findNext()
      }
      def next() =
        if (hasNext) {
          hdDefined = false
          hd
        } else Iterator.empty.next()
    }
  }

  /** A view of the `Right` values produced by applying `f` to the elements of an
   *  underlying collection; elements for which `f` yields a `Left` are skipped.
   *
   *  @tparam A the element type of the underlying collection
   *  @tparam A1 the `Left` type of the results of `f`; values of this type are discarded
   *  @tparam A2 the `Right` type of the results of `f`, and the element type of this view
   *  @param underlying the collection whose elements are partitioned
   *  @param f the function applied on demand to each underlying element
   */
  @SerialVersionUID(3L)
  class RightPartitionMapped[A, A1, A2](underlying: SomeIterableOps[A]^, f: A => Either[A1, A2]) extends AbstractView[A2] {
      /** Returns an iterator that applies `f` on demand to the underlying elements and
       *  produces the value inside each `Right` result, skipping `Left` results.
       */
      def iterator: AbstractIterator[A2]^{this} = new AbstractIterator[A2] {
        private val self = underlying.iterator
        private var hd: A2 = compiletime.uninitialized
        private var hdDefined: Boolean = false
        def hasNext = hdDefined || {
          @tailrec
          def findNext(): Boolean =
            if (self.hasNext) {
              f(self.next()) match {
                case Left(_) => findNext()
                case Right(a2) => hd = a2; hdDefined = true; true
              }
            } else false
          findNext()
        }
        def next() =
          if (hasNext) {
            hdDefined = false
            hd
          } else Iterator.empty.next()
      }
  }

  /** A view that drops leading elements of the underlying collection. */
  @SerialVersionUID(3L)
  class Drop[A](underlying: SomeIterableOps[A]^, n: Int) extends AbstractView[A] {
    /** Returns an iterator over the underlying elements, skipping the first `n` of them. */
    def iterator = underlying.iterator.drop(n)
    /** The number of elements to drop, clamped to be non-negative. */
    protected val normN = n max 0
    /** Returns the underlying collection's known size minus the drop count (at least 0),
     *  or -1 if the underlying size is not known.
     */
    override def knownSize = {
      val size = underlying.knownSize
      if (size >= 0) (size - normN) max 0 else -1
    }
    /** Returns `true` if no element remains after the drop, determined by iterating
     *  past the dropped prefix.
     */
    override def isEmpty: Boolean = iterator.isEmpty
  }

  /** A view that drops trailing elements of the underlying collection. */
  @SerialVersionUID(3L)
  class DropRight[A](underlying: SomeIterableOps[A]^, n: Int) extends AbstractView[A] {
    /** Returns an iterator over the underlying elements except the last `n`.
     *
     *  If the underlying size is not known, the iterator maintains an `n`-element
     *  lookahead buffer, so it reads `n` elements ahead of the one it produces.
     */
    def iterator = dropRightIterator(underlying.iterator, n)
    /** The number of elements to drop, clamped to be non-negative. */
    protected val normN = n max 0
    /** Returns the underlying collection's known size minus the drop count (at least 0),
     *  or -1 if the underlying size is not known.
     */
    override def knownSize = {
      val size = underlying.knownSize
      if (size >= 0) (size - normN) max 0 else -1
    }
    /** Returns `true` if this view has no elements, using `knownSize` when available and
     *  otherwise iterating, which reads up to `n + 1` underlying elements.
     */
    override def isEmpty: Boolean =
      if(knownSize >= 0) knownSize == 0
      else iterator.isEmpty
  }

  /** A view of the elements of an underlying collection that follow its longest prefix
   *  of elements satisfying a predicate.
   *
   *  @tparam A the element type of the view
   *  @param underlying the collection whose leading elements are dropped
   *  @param p the predicate that selects the prefix to drop, evaluated on demand
   */
  @SerialVersionUID(3L)
  class DropWhile[A](underlying: SomeIterableOps[A]^, p: A => Boolean) extends AbstractView[A] {
    /** Returns an iterator over the underlying elements, skipping the longest prefix
     *  whose elements all satisfy `p`.
     */
    def iterator = underlying.iterator.dropWhile(p)
    /** Returns 0 if the underlying collection is known to be empty, -1 otherwise
     *  (the length of the dropped prefix cannot be known without evaluating `p`).
     */
    override def knownSize: Int = if (underlying.knownSize == 0) 0 else super.knownSize
    /** Returns `true` if every underlying element satisfies `p`, determined by iterating
     *  past the dropped prefix.
     */
    override def isEmpty: Boolean = iterator.isEmpty
  }

  /** A view that takes leading elements of the underlying collection. */
  @SerialVersionUID(3L)
  class Take[+A](underlying: SomeIterableOps[A]^, n: Int) extends AbstractView[A] {
    /** Returns an iterator over the first `n` underlying elements, or all of them
     *  if there are fewer than `n`.
     */
    def iterator = underlying.iterator.take(n)
    /** The number of elements to take, clamped to be non-negative. */
    protected val normN = n max 0
    /** Returns the smaller of the underlying collection's known size and the take count,
     *  or -1 if the underlying size is not known.
     */
    override def knownSize = {
      val size = underlying.knownSize
      if (size >= 0) size min normN else -1
    }
    /** Returns `true` if this view has no elements, determined by iterating. */
    override def isEmpty: Boolean = iterator.isEmpty
  }

  /** A view that takes trailing elements of the underlying collection. */
  @SerialVersionUID(3L)
  class TakeRight[+A](underlying: SomeIterableOps[A]^, n: Int) extends AbstractView[A] {
    /** Returns an iterator over the last `n` underlying elements, or all of them
     *  if there are fewer than `n`.
     *
     *  If the underlying size is not known, the first query of the iterator consumes
     *  the entire underlying iterator, buffering at most `n` elements.
     */
    def iterator = takeRightIterator(underlying.iterator, n)
    /** The number of elements to take, clamped to be non-negative. */
    protected val normN = n max 0
    /** Returns the smaller of the underlying collection's known size and the take count,
     *  or -1 if the underlying size is not known.
     */
    override def knownSize = {
      val size = underlying.knownSize
      if (size >= 0) size min normN else -1
    }
    /** Returns `true` if this view has no elements, using `knownSize` when available and
     *  otherwise iterating, which traverses the whole underlying collection.
     */
    override def isEmpty: Boolean =
      if(knownSize >= 0) knownSize == 0
      else iterator.isEmpty
  }

  /** A view of the longest prefix of an underlying collection whose elements all
   *  satisfy a predicate.
   *
   *  @tparam A the element type of the view
   *  @param underlying the collection whose prefix is taken
   *  @param p the predicate that selects the prefix, evaluated on demand
   */
  @SerialVersionUID(3L)
  class TakeWhile[A](underlying: SomeIterableOps[A]^, p: A => Boolean) extends AbstractView[A] {
    /** Returns an iterator over the underlying elements that stops before the first
     *  element failing `p`.
     */
    def iterator = underlying.iterator.takeWhile(p)
    /** Returns 0 if the underlying collection is known to be empty, -1 otherwise
     *  (the length of the prefix cannot be known without evaluating `p`).
     */
    override def knownSize: Int = if (underlying.knownSize == 0) 0 else super.knownSize
    /** Returns `true` if the underlying collection is empty or its first element fails `p`. */
    override def isEmpty: Boolean = iterator.isEmpty
  }

  /** A view of the cumulative results of applying an operator going left to right over
   *  an underlying collection, starting with an initial value.
   *
   *  @tparam A the element type of the underlying collection
   *  @tparam B the element type of the view
   *  @param underlying the collection to scan
   *  @param z the initial value, the first element of the view
   *  @param op the operator applied on demand to the previous result and the next underlying element
   */
  @SerialVersionUID(3L)
  class ScanLeft[+A, +B](underlying: SomeIterableOps[A]^, z: B, op: (B, A) => B) extends AbstractView[B] {
    /** Returns an iterator producing `z`, then on demand each successive result of
     *  applying `op` to the previous result and the next underlying element.
     */
    def iterator = underlying.iterator.scanLeft(z)(op)
    /** Returns the underlying collection's known size plus 1 (for the initial value `z`),
     *  or -1 if the underlying size is not known.
     */
    override def knownSize: Int = {
      val size = underlying.knownSize
      if (size >= 0) size + 1 else -1
    }
    /** Returns `false`: this view always contains at least the initial value `z`. */
    override def isEmpty: Boolean = iterator.isEmpty
  }

  /** A view that maps elements of the underlying collection. */
  @SerialVersionUID(3L)
  class Map[+A, +B](underlying: SomeIterableOps[A]^, f: A => B) extends AbstractView[B] {
    /** Returns an iterator that applies `f` on demand to each underlying element. */
    def iterator = underlying.iterator.map(f)
    /** Returns the underlying collection's known size, or -1 if it is not known;
     *  mapping does not change the number of elements.
     */
    override def knownSize = underlying.knownSize
    /** Returns `true` if the underlying collection is empty; `f` is not called. */
    override def isEmpty: Boolean = underlying.isEmpty
  }

  /** A view that flatmaps elements of the underlying collection. */
  @SerialVersionUID(3L)
  class FlatMap[A, B](underlying: SomeIterableOps[A]^, f: A => IterableOnce[B]^) extends AbstractView[B] {
    /** Returns an iterator that applies `f` on demand to each underlying element and
     *  produces the elements of each result in turn.
     */
    def iterator = underlying.iterator.flatMap(f)
    /** Returns 0 if the underlying collection is known to be empty, -1 otherwise
     *  (the total number of elements cannot be known without evaluating `f`).
     */
    override def knownSize: Int = if (underlying.knownSize == 0) 0 else super.knownSize
    /** Returns `true` if `f` yields an empty collection for every underlying element,
     *  evaluating `f` on the underlying elements until a first non-empty result.
     */
    override def isEmpty: Boolean = iterator.isEmpty
  }

  /** A view that collects elements of the underlying collection. */
  @SerialVersionUID(3L)
  class Collect[+A, B](underlying: SomeIterableOps[A]^, pf: PartialFunction[A, B]^) extends AbstractView[B] {
    /** Returns an iterator that, on demand, applies `pf` to each underlying element on
     *  which it is defined, skipping the elements on which it is not.
     */
    def iterator = underlying.iterator.collect(pf)
  }

  /** A view that concatenates elements of the prefix collection or iterator with the elements
   *  of the suffix collection or iterator.
   */
  @SerialVersionUID(3L)
  class Concat[A](prefix: SomeIterableOps[A]^, suffix: SomeIterableOps[A]^) extends AbstractView[A] {
    /** Returns an iterator over the prefix elements followed by the suffix elements. */
    def iterator = prefix.iterator ++ suffix.iterator
    /** Returns the sum of the two known sizes, or -1 if either is not known. */
    override def knownSize = {
      val prefixSize = prefix.knownSize
      if (prefixSize >= 0) {
        val suffixSize = suffix.knownSize
        if (suffixSize >= 0) prefixSize + suffixSize
        else -1
      }
      else -1
    }
    /** Returns `true` if both the prefix and the suffix are empty. */
    override def isEmpty: Boolean = prefix.isEmpty && suffix.isEmpty
  }

  /** A view that zips elements of the underlying collection with the elements
   *  of another collection.
   */
  @SerialVersionUID(3L)
  class Zip[A, B](underlying: SomeIterableOps[A]^, other: Iterable[B]^) extends AbstractView[(A, B)] {
    /** Returns an iterator producing pairs of corresponding elements, ending when either
     *  collection runs out of elements.
     */
    def iterator = underlying.iterator.zip(other)
    /** Returns the smaller of the two known sizes; 0 if either collection is known to be
     *  empty, and -1 if neither is known to be empty and either size is not known.
     */
    override def knownSize = {
      val s1 = underlying.knownSize
      if (s1 == 0) 0 else {
        val s2 = other.knownSize
        if (s2 == 0) 0 else s1 min s2
      }
    }
    /** Returns `true` if either collection is empty. */
    override def isEmpty: Boolean = underlying.isEmpty || other.isEmpty
  }

  /** A view that zips elements of the underlying collection with the elements
   *  of another collection. If one of the two collections is shorter than the other,
   *  placeholder elements are used to extend the shorter collection to the length of the longer.
   */
  @SerialVersionUID(3L)
  class ZipAll[A, B](underlying: SomeIterableOps[A]^, other: Iterable[B]^, thisElem: A, thatElem: B) extends AbstractView[(A, B)] {
    /** Returns an iterator producing pairs of corresponding elements, substituting
     *  `thisElem` or `thatElem` once the shorter collection runs out of elements.
     */
    def iterator = underlying.iterator.zipAll(other, thisElem, thatElem)
    /** Returns the larger of the two known sizes, or -1 if either is not known. */
    override def knownSize = {
      val s1 = underlying.knownSize
      if(s1 == -1) -1 else {
        val s2 = other.knownSize
        if(s2 == -1) -1 else s1 max s2
      }
    }
    /** Returns `true` if both collections are empty. */
    override def isEmpty: Boolean = underlying.isEmpty && other.isEmpty
  }

  /** A view that appends an element to its elements. */
  @SerialVersionUID(3L)
  class Appended[+A](underlying: SomeIterableOps[A]^, elem: A) extends AbstractView[A] {
    /** Returns an iterator over the underlying elements followed by `elem`. */
    def iterator = new Concat(underlying, new View.Single(elem)).iterator
    /** Returns the underlying collection's known size plus 1, or -1 if it is not known. */
    override def knownSize: Int = {
      val size = underlying.knownSize
      if (size >= 0) size + 1 else -1
    }
    /** Returns `false`: this view always contains at least the appended element. */
    override def isEmpty: Boolean = false
  }

  /** A view that prepends an element to its elements. */
  @SerialVersionUID(3L)
  class Prepended[+A](elem: A, underlying: SomeIterableOps[A]^) extends AbstractView[A] {
    /** Returns an iterator producing `elem` followed by the underlying elements. */
    def iterator = new Concat(new View.Single(elem), underlying).iterator
    /** Returns the underlying collection's known size plus 1, or -1 if it is not known. */
    override def knownSize: Int = {
      val size = underlying.knownSize
      if (size >= 0) size + 1 else -1
    }
    /** Returns `false`: this view always contains at least the prepended element. */
    override def isEmpty: Boolean = false
  }

  /** A view of the elements of an underlying collection with the element at one position
   *  replaced by another value.
   *
   *  If `index` is greater than or equal to the number of underlying elements, iterating
   *  past the end throws an `IndexOutOfBoundsException`; if `index` is negative, no
   *  element is replaced and the view contains the underlying elements unchanged.
   *
   *  @tparam A the element type of the view
   *  @param underlying the collection providing the elements
   *  @param index the position at which to substitute `elem`
   *  @param elem the replacement element
   */
  @SerialVersionUID(3L)
  class Updated[A](underlying: SomeIterableOps[A]^, index: Int, elem: A) extends AbstractView[A] {
    /** Returns an iterator over the underlying elements that produces `elem` instead of
     *  the element at position `index`.
     *
     *  @throws IndexOutOfBoundsException if the underlying collection is exhausted
     *          before position `index` is reached
     */
    def iterator: Iterator[A]^{underlying} = new AbstractIterator[A] {
      private val it = underlying.iterator
      private var i = 0
      def next(): A = {
        val value = if (i == index) { it.next(); elem } else it.next()
        i += 1
        value
      }
      def hasNext: Boolean =
        if(it.hasNext) true
        else if(index >= i) throw new IndexOutOfBoundsException(index.toString)
        else false
    }
    /** Returns the underlying collection's known size, or -1 if it is not known;
     *  updating does not change the number of elements.
     */
    override def knownSize: Int = underlying.knownSize
    /** Returns `false` if the underlying collection has elements, determined by iterating.
     *
     *  @throws IndexOutOfBoundsException if the underlying collection is empty and
     *          `index` is non-negative; `true` is returned only for a negative `index`
     *          on an empty underlying collection
     */
    override def isEmpty: Boolean = iterator.isEmpty
  }

  @SerialVersionUID(3L)
  private[collection] class Patched[A](underlying: SomeIterableOps[A]^, from: Int, other: IterableOnce[A]^, replaced: Int) extends AbstractView[A] {
    // we may be unable to traverse `other` more than once, so we need to cache it if that's the case
    private val _other: Iterable[A]^{other} = other match {
      case other: Iterable[A @unchecked] => other
      case other              => LazyList.from(other)
    }

    /** Returns an iterator over the underlying elements in which, starting at position
     *  `from`, `replaced` elements are dropped and the elements of `other` are inserted.
     */
    def iterator: Iterator[A]^{this} = underlying.iterator.patch(from, _other.iterator, replaced)
    /** Returns 0 if both the underlying collection and the patch are known to be empty,
     *  -1 otherwise.
     */
    override def knownSize: Int = if (underlying.knownSize == 0 && _other.knownSize == 0) 0 else super.knownSize
    /** Returns `true` if this view has no elements, using `knownSize` when it is 0 and
     *  otherwise iterating.
     */
    override def isEmpty: Boolean = if (knownSize == 0) true else iterator.isEmpty
  }

  /** A view that pairs each element of an underlying collection with its index,
   *  counting from 0.
   *
   *  @tparam A the element type of the underlying collection
   *  @param underlying the collection whose elements are paired with their indices
   */
  @SerialVersionUID(3L)
  class ZipWithIndex[A](underlying: SomeIterableOps[A]^) extends AbstractView[(A, Int)] {
    /** Returns an iterator producing each underlying element paired with its index,
     *  starting from `(firstElem, 0)`.
     */
    def iterator: Iterator[(A, Int)]^{this} = underlying.iterator.zipWithIndex
    /** Returns the underlying collection's known size, or -1 if it is not known. */
    override def knownSize: Int = underlying.knownSize
    /** Returns `true` if the underlying collection is empty. */
    override def isEmpty: Boolean = underlying.isEmpty
  }

  /** A view of the elements of an underlying collection, followed by as many copies of a
   *  padding element as are needed to reach a given length.
   *
   *  If the underlying collection already has `len` or more elements, no padding is added.
   *
   *  @tparam A the element type of the view
   *  @param underlying the collection providing the leading elements
   *  @param len the minimum length of the view
   *  @param elem the padding element
   */
  @SerialVersionUID(3L)
  class PadTo[A](underlying: SomeIterableOps[A]^, len: Int, elem: A) extends AbstractView[A] {
    /** Returns an iterator over the underlying elements, then over copies of `elem` until
     *  `len` elements in total have been produced.
     */
    def iterator: Iterator[A]^{this} = underlying.iterator.padTo(len, elem)

    /** Returns the larger of the underlying collection's known size and `len`, or -1 if
     *  the underlying size is not known.
     */
    override def knownSize: Int = {
      val size = underlying.knownSize
      if (size >= 0) size max len else -1
    }
    /** Returns `true` if the underlying collection is empty and `len <= 0`. */
    override def isEmpty: Boolean = underlying.isEmpty && len <= 0
  }

  private[collection] def takeRightIterator[A](it: Iterator[A]^, n: Int): Iterator[A]^{it} = {
    val k = it.knownSize
    if(k == 0 || n <= 0) Iterator.empty
    else if(n == Int.MaxValue) it
    else if(k > 0) it.drop((k-n) max 0)
    else new TakeRightIterator[A](it, n)
  }

  private final class TakeRightIterator[A](private var underlying: Iterator[A]^, maxlen: Int) extends AbstractIterator[A] {
    private var len: Int = -1
    private var pos: Int = 0
    @annotation.stableNull
    private var buf: ArrayBuffer[AnyRef] | Null = compiletime.uninitialized
    /** Consumes the entire underlying iterator into a circular buffer holding its last
     *  `maxlen` elements, then releases the underlying iterator; does nothing after the
     *  first call.
     */
    def init(): Unit = if(buf eq null) {
      buf = new ArrayBuffer[AnyRef](maxlen min 256)
      len = 0
      while(underlying.hasNext) {
        val n = underlying.next().asInstanceOf[AnyRef]
        if(pos >= buf.length) buf.addOne(n)
        else buf(pos) = n
        pos += 1
        if(pos == maxlen) pos = 0
        len += 1
      }
      underlying = nullForGC[Iterator[A]]
      if(len > maxlen) len = maxlen
      pos = pos - len
      if(pos < 0) pos += maxlen
    }
    /** Returns the number of remaining elements, or -1 before the first `hasNext`,
     *  `next()`, or `drop` call.
     */
    override def knownSize = len
    /** Returns `true` if buffered elements remain, consuming the underlying iterator on
     *  the first call.
     */
    def hasNext: Boolean = {
      init()
      len > 0
    }
    /** Returns the next buffered element, consuming the underlying iterator on the first
     *  call.
     *
     *  @throws NoSuchElementException if no elements remain
     */
    def next(): A = {
      init()
      if(len == 0) Iterator.empty.next()
      else {
        val x = buf.nn(pos).asInstanceOf[A]
        pos += 1
        if(pos == maxlen) pos = 0
        len -= 1
        x
      }
    }
    /** Advances past the next `n` buffered elements in constant time by moving the buffer
     *  position, consuming the underlying iterator on the first call.
     *
     *  @param n the number of elements to skip; non-positive values skip nothing
     *  @return this iterator
     */
    override def drop(n: Int): Iterator[A]^{this} = {
      init()
      if (n > 0) {
        len = (len - n) max 0
        pos = (pos + n) % maxlen
      }
      this
    }
  }

  private[collection] def dropRightIterator[A](it: Iterator[A]^, n: Int): Iterator[A]^{it} = {
    if(n <= 0) it
    else {
      val k = it.knownSize
      if(k >= 0) it.take(k - n)
      else new DropRightIterator[A](it, n)
    }
  }

  private final class DropRightIterator[A](private var underlying: Iterator[A]^, maxlen: Int) extends AbstractIterator[A] {
    private var len: Int = -1 // known size or -1 if the end of `underlying` has not been seen yet
    private var pos: Int = 0
    @annotation.stableNull
    private var buf: ArrayBuffer[AnyRef] | Null = compiletime.uninitialized
    /** Fills the lookahead buffer with up to `maxlen` elements from the underlying
     *  iterator; does nothing after the first call.
     */
    def init(): Unit = if(buf eq null) {
      buf = new ArrayBuffer[AnyRef](maxlen min 256)
      while(pos < maxlen && underlying.hasNext) {
        buf.addOne(underlying.next().asInstanceOf[AnyRef])
        pos += 1
      }
      if(!underlying.hasNext) len = 0
      pos = 0
    }
    /** Returns 0 once the end of the underlying iterator has been seen, -1 before that. */
    override def knownSize = len
    /** Returns `true` if the underlying iterator holds more elements than remain buffered
     *  to be dropped, filling the lookahead buffer on the first call.
     */
    def hasNext: Boolean = {
      init()
      len != 0
    }
    /** Returns the oldest buffered element, refilling its buffer slot from the underlying
     *  iterator.
     *
     *  @throws NoSuchElementException if only the `maxlen` elements to drop remain
     */
    def next(): A = {
      if(!hasNext) Iterator.empty.next()
      else {
        val x = buf.nn(pos).asInstanceOf[A]
        if(len == -1) {
          buf.nn(pos) = underlying.next().asInstanceOf[AnyRef]
          if(!underlying.hasNext) len = 0
        } else len -= 1
        pos += 1
        if(pos == maxlen) pos = 0
        x
      }
    }
  }
}

/** Explicit instantiation of the `View` trait to reduce class file size in subclasses. */
@SerialVersionUID(3L)
abstract class AbstractView[+A] extends scala.collection.AbstractIterable[A] with View[A]
