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
import java.io.{ObjectInputStream, ObjectOutputStream}
import java.lang.{StringBuilder => JStringBuilder}

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.generic.SerializeEnd
import scala.collection.mutable.{ArrayBuffer, StringBuilder}
import scala.language.implicitConversions
import scala.runtime.ScalaRunTime.nullForGC

/** A sequence whose elements are computed as they are needed, one cons cell at a time.
 *
 *  A non-empty stream holds its head as an already computed value and its tail as an
 *  expression that is evaluated the first time the tail is asked for and remembered
 *  afterwards, so no element is ever computed twice. Building a cons cell therefore
 *  computes one element and nothing beyond it, which lets a stream stand for a series
 *  that has no end:
 *
 *  ```scala sc:compile
 *  val naturals: Stream[Int] = Stream.from(0)
 *  naturals.take(3).force // Stream(0, 1, 2)
 *  ```
 *
 *  Because the head is strict, an operation that produces a stream computes the first
 *  element of its result right away; only the rest is deferred. Because the elements
 *  are memoized, a stream that is still referenced from its head keeps every element
 *  it has produced, which is what makes traversing one twice cheap and what makes a
 *  long one expensive to hold on to.
 *
 *  @tparam A the type of the elements contained in this stream
 *
 *  @define coll stream
 *  @define Coll `Stream`
 */
@deprecated("Use LazyListIterable (which is fully lazy) instead of Stream (which has a lazy tail only)", "2.13.0")
@SerialVersionUID(3L)
sealed abstract class Stream[+A] extends AbstractSeq[A]
  with LinearSeq[A]
  with LinearSeqOps[A, Stream, Stream[A]]
  with IterableFactoryDefaults[A, Stream]
  with Serializable {
  /** Returns a stream of all elements of this stream except the first.
   *
   *  The tail is computed the first time it is asked for and remembered afterwards.
   *
   *  @throws UnsupportedOperationException if this stream is empty
   */
  def tail: Stream[A]

  /** Forces evaluation of the whole `Stream` and returns it.
   *
   *  @note Often we use `Stream`s to represent an infinite set or series.  If
   *  that's the case for your particular `Stream` then this function will never
   *  return and will probably crash the VM with an `OutOfMemory` exception.
   *  This function will not hang on a finite cycle, however.
   *
   *  @return The fully realized `Stream`.
   */
  def force: this.type

  /** The factory used to build streams, the [[Stream$ `Stream`]] companion object. */
  override def iterableFactory: SeqFactory[Stream] = Stream

  /** The prefix of this stream's string representation: `"Stream"`. */
  override protected def className: String = "Stream"

  /** Applies the given function `f` to each element of this linear sequence
   *  (while respecting the order of the elements).
   *
   *  @tparam U the result type of `f`, discarded by `foreach`
   *  @param f The treatment to apply to each element.
   *  @note  Overridden here as final to trigger tail-call optimization, which
   *  replaces 'this' with 'tail' at each iteration. This is absolutely
   *  necessary for allowing the GC to collect the underlying Stream as elements
   *  are consumed.
   *  @note  This function will force the realization of the entire Stream
   *  unless the `f` throws an exception.
   */
  @tailrec
  override final def foreach[U](f: A => U): Unit = {
    if (!this.isEmpty) {
      f(head)
      tail.foreach(f)
    }
  }

  /** Returns the first element satisfying `p` wrapped in a `Some`, or `None` if there
   *  is none.
   *
   *  The elements up to and including the first match are computed, and no others.
   *  Overridden here as final to trigger tail-call optimization, so that the elements
   *  already tested can be collected while the search goes on.
   *
   *  @param p the predicate used to test elements
   */
  @tailrec
  override final def find(p: A => Boolean): Option[A] = {
    if(isEmpty) None
    else if(p(head)) Some(head)
    else tail.find(p)
  }

  /** Returns a stream of the first `n` elements of this stream, or of all its elements
   *  if it has fewer than `n`.
   *
   *  Only the first element of the result is computed; the rest is computed as the
   *  result is forced. A non-positive `n` gives the empty stream.
   *
   *  @param n the number of elements to take
   */
  override def take(n: Int): Stream[A] = {
    if (n <= 0 || isEmpty) Stream.empty
    else if (n == 1) new Stream.Cons(head, Stream.empty)
    else new Stream.Cons(head, tail.take(n - 1))
  }

  /** Stream specialization of foldLeft which allows GC to collect along the
   *  way.
   *
   *  @tparam B The type of value being accumulated.
   *  @param z The initial value seeded into the function `op`.
   *  @param op The operation to perform on successive elements of the `Stream`.
   *  @return The accumulated value from successive applications of `op`.
   */
  @tailrec
  override final def foldLeft[B](z: B)(op: (B, A) => B): B = {
    if (this.isEmpty) z
    else tail.foldLeft(op(z, head))(op)
  }

  /** The stream resulting from the concatenation of this stream with the argument stream.
   *  @param rest   The collection that gets appended to this stream
   *  @return       The stream containing elements of this stream and the iterable object.
   */
  @deprecated("The `append` operation has been renamed `lazyAppendedAll`", "2.13.0")
  @inline final def append[B >: A](rest: => IterableOnce[B]): Stream[B] = lazyAppendedAll(rest)

  /** Replaces this stream with a serialization proxy during Java serialization, but only
   *  if it is non-empty and its tail has already been computed.
   *
   *  The proxy writes out the computed prefix compactly; a stream whose tail is not yet
   *  computed is serialized as itself, so that its unevaluated thunk is stored by
   *  standard Java serialization and is not forced by serializing.
   */
  protected def writeReplace(): AnyRef =
    if(nonEmpty && tailDefined) new Stream.SerializationProxy[A](this) else this

  /** Prints elements of this stream one by one, separated by commas. */
  @deprecated(message = """Use print(stream.force.mkString(", ")) instead""", since = "2.13.0")
  @inline def print(): Unit = Console.print(this.force.mkString(", "))

  /** Prints elements of this stream one by one, separated by `sep`.
   *  @param sep   The separator string printed between consecutive elements.
   */
  @deprecated(message = "Use print(stream.force.mkString(sep)) instead", since = "2.13.0")
  @inline def print(sep: String): Unit = Console.print(this.force.mkString(sep))

  /** The stream resulting from the concatenation of this stream with the argument stream.
   *
   *  @tparam B the element type of the returned stream, which must be a supertype of `A`
   *  @param suffix The collection that gets appended to this stream
   *  @return The stream containing elements of this stream and the iterable object.
   */
  def lazyAppendedAll[B >: A](suffix: => collection.IterableOnce[B]): Stream[B] =
    if (isEmpty) iterableFactory.from(suffix) else Stream.cons[B](head, tail.lazyAppendedAll(suffix))

  /** Returns a stream of the intermediate results of folding this stream from the left
   *  with `op`, starting with `z`.
   *
   *  The result begins with `z`; each further element is computed only as the result is
   *  forced that far.
   *
   *  @tparam B the type of the accumulated value
   *  @param z the start value, which is the first element of the result
   *  @param op the operation applied to the accumulated value and the next element
   *  @return a stream of the successive results of `op`, of length one more than this stream
   */
  override def scanLeft[B](z: B)(op: (B, A) => B): Stream[B] =
    if (isEmpty) z +: iterableFactory.empty
    else Stream.cons(z, tail.scanLeft(op(z, head))(op))

  /** Stream specialization of reduceLeft which allows GC to collect
   *  along the way.
   *
   *  @tparam B The type of value being accumulated.
   *  @param f The operation to perform on successive elements of the `Stream`.
   *  @return The accumulated value from successive applications of `f`.
   */
  override final def reduceLeft[B >: A](f: (B, A) => B): B = {
    if (this.isEmpty) throw new UnsupportedOperationException("empty.reduceLeft")
    else {
      var reducedRes: B = this.head
      var left: Stream[A] = this.tail
      while (!left.isEmpty) {
        reducedRes = f(reducedRes, left.head)
        left = left.tail
      }
      reducedRes
    }
  }

  /** Returns a pair of streams, the first holding the elements satisfying `p` and the
   *  second the elements that do not.
   *
   *  This traverses the elements twice, once for each side of the pair, and applies `p`
   *  to every element it passes; both traversals stop at the first element they keep.
   *
   *  @param p the predicate used to test elements
   *  @return a pair of the elements satisfying `p` and the elements not satisfying it
   */
  override def partition(p: A => Boolean): (Stream[A], Stream[A]) = (filter(p(_)), filterNot(p(_)))

  /** Returns a stream of the elements of this stream that satisfy `pred`.
   *
   *  The leading elements that do not satisfy `pred` are computed and dropped at once,
   *  so this returns only when a first matching element has been found or the stream has
   *  been exhausted; the remaining elements are filtered as the result is forced.
   *
   *  @param pred the predicate an element must satisfy to be kept
   */
  override def filter(pred: A => Boolean): Stream[A] = filterImpl(pred, isFlipped = false)

  /** Returns a stream of the elements of this stream that do not satisfy `pred`.
   *
   *  The leading elements that satisfy `pred` are computed and dropped at once, so this
   *  returns only when a first non-matching element has been found or the stream has
   *  been exhausted; the remaining elements are filtered as the result is forced.
   *
   *  @param pred the predicate an element must not satisfy to be kept
   */
  override def filterNot(pred: A => Boolean): Stream[A] = filterImpl(pred, isFlipped = true)

  private[immutable] def filterImpl(p: A => Boolean, isFlipped: Boolean): Stream[A] = {
    // optimization: drop leading prefix of elems for which f returns false
    // var rest = this dropWhile (!p(_)) - forget DRY principle - GC can't collect otherwise
    var rest: Stream[A] = coll
    while (rest.nonEmpty && p(rest.head) == isFlipped) rest = rest.tail
    // private utility func to avoid `this` on stack (would be needed for the lazy arg)
    if (rest.nonEmpty) Stream.filteredTail(rest, p, isFlipped)
    else iterableFactory.empty
  }

  /** A `collection.WithFilter` which allows GC of the head of stream during processing.
   *
   *  @param p the predicate used to test elements
   *  @return a `WithFilter` over this stream restricted to elements satisfying `p`, suitable for chained `map`, `flatMap`, `foreach`, and further `withFilter` calls
   */
  override final def withFilter(p: A => Boolean): collection.WithFilter[A, Stream] =
    Stream.withFilter(coll, p)

  /** Returns a stream consisting of `elem` followed by the elements of this stream.
   *
   *  Nothing of this stream is computed; it becomes the deferred tail of the result.
   *
   *  @tparam B the element type of the returned stream, a supertype of `A`
   *  @param elem the element to prepend
   */
  override final def prepended[B >: A](elem: B): Stream[B] = Stream.cons(elem, coll)

  /** Returns a stream of the results of applying `f` to each element of this stream.
   *
   *  `f` is applied to the head at once, and to each further element only as the result
   *  is forced that far.
   *
   *  @tparam B the element type of the returned stream
   *  @param f the function to apply to each element
   */
  override final def map[B](f: A => B): Stream[B] =
    if (isEmpty) iterableFactory.empty
    else Stream.cons(f(head), tail.map(f))

  /** Returns a stream of the results of applying `pf` to the elements on which it is
   *  defined, leaving the others out.
   *
   *  The leading elements on which `pf` is not defined are computed and dropped at once,
   *  so this returns only when a first element in the domain of `pf` has been found or
   *  the stream has been exhausted; `pf` is applied to each element at most once.
   *
   *  @tparam B the element type of the returned stream
   *  @param pf the partial function applied to the elements in its domain
   */
  @tailrec override final def collect[B](pf: PartialFunction[A, B]): Stream[B] =
    if(isEmpty) Stream.empty
    else {
      var newHead: B = null.asInstanceOf[B]
      val runWith = pf.runWith((b: B) => newHead = b)
      if(runWith(head)) Stream.collectedTail(newHead, this, pf)
      else tail.collect(pf)
    }

  /** Returns the result of applying `pf` to the first element in its domain wrapped in
   *  a `Some`, or `None` if there is no such element.
   *
   *  The elements up to and including the first match are computed, and no others; `pf`
   *  is applied to each of them at most once.
   *
   *  @tparam B the result type of `pf`
   *  @param pf the partial function applied to the elements in its domain
   */
  @tailrec override final def collectFirst[B](pf: PartialFunction[A, B]): Option[B] =
    if(isEmpty) None
    else {
      var newHead: B = null.asInstanceOf[B]
      val runWith = pf.runWith((b: B) => newHead = b)
      if(runWith(head)) Some(newHead)
      else tail.collectFirst(pf)
    }

  // optimisations are not for speed, but for functionality
  // see tickets #153, #498, #2147, and corresponding tests in run/ (as well as run/stream_flatmap_odds.scala)
  /** Returns a stream of the concatenated results of applying `f` to each element of
   *  this stream.
   *
   *  `f` is applied to leading elements until one of them yields a non-empty collection,
   *  so an element whose image is empty is passed over at once; the rest is computed as
   *  the result is forced.
   *
   *  @tparam B the element type of the returned stream
   *  @param f the function mapping each element to a collection of results
   */
  override final def flatMap[B](f: A => IterableOnce[B]): Stream[B] =
    if (isEmpty) iterableFactory.empty
    else {
      // establish !prefix.isEmpty || nonEmptyPrefix.isEmpty
      var nonEmptyPrefix: Stream[A] = coll
      var prefix = iterableFactory.from(f(nonEmptyPrefix.head))
      while (!nonEmptyPrefix.isEmpty && prefix.isEmpty) {
        nonEmptyPrefix = nonEmptyPrefix.tail
        if(!nonEmptyPrefix.isEmpty)
          prefix = iterableFactory.from(f(nonEmptyPrefix.head))
      }

      if (nonEmptyPrefix.isEmpty) iterableFactory.empty
      else prefix.lazyAppendedAll(nonEmptyPrefix.tail.flatMap(f))
    }

  /** Returns a stream of pairs of corresponding elements of this stream and `that`,
   *  as long as both have elements left.
   *
   *  Only the first pair is computed; the rest is computed as the result is forced. The
   *  length of the result is the smaller of the two lengths.
   *
   *  @tparam B the element type of `that`
   *  @param that the collection to zip with this stream
   */
  override final def zip[B](that: collection.IterableOnce[B]): Stream[(A, B)] =
    if (this.isEmpty || that.isEmpty) iterableFactory.empty
    else {
      val thatIterable = that match {
        case that: collection.Iterable[B @unchecked] => that
        case _ => LazyList.from(that)
      }
      Stream.cons[(A, B)]((this.head, thatIterable.head), this.tail.zip(thatIterable.tail))
    }

  /** Returns a stream of pairs of the elements of this stream and their indices,
   *  counting from `0`; only the first pair is computed.
   */
  override final def zipWithIndex: Stream[(A, Int)] = this.zip(LazyList.from(0))

  /** Returns `true` if the tail of this stream has already been computed. */
  protected def tailDefined: Boolean

  /** Appends all elements of this $coll to a string builder using start, end, and separator strings.
   *  The written text begins with the string `start` and ends with the string `end`.
   *  Inside, the string representations (w.r.t. the method `toString`)
   *  of all elements of this $coll are separated by the string `sep`.
   *
   *  Undefined elements are represented with `"_"`, an undefined tail is represented with `"&lt;not computed&gt;"`,
   *  and cycles are represented with `"&lt;cycle&gt;"`.
   *
   *  @param sb    the string builder to which elements are appended.
   *  @param start the starting string.
   *  @param sep   the separator string.
   *  @param end   the ending string.
   *  @return      the string builder `b` to which elements were appended.
   */
  override def addString(sb: StringBuilder, start: String, sep: String, end: String): sb.type = {
    force
    addStringNoForce(sb.underlying, start, sep, end)
    sb
  }

  private def addStringNoForce(b: JStringBuilder, start: String, sep: String, end: String): b.type = {
    b.append(start)
    if (nonEmpty) {
      b.append(head)
      var cursor = this
      def appendCursorElement(): Unit = b.append(sep).append(cursor.head)
      if (tailDefined) {  // If tailDefined, also !isEmpty
        var scout = tail
        if (cursor ne scout) {
          cursor = scout
          if (scout.tailDefined) {
            scout = scout.tail
            // Use 2x 1x iterator trick for cycle detection; slow iterator can add strings
            while ((cursor ne scout) && scout.tailDefined) {
              appendCursorElement()
              cursor = cursor.tail
              scout = scout.tail
              if (scout.tailDefined) scout = scout.tail
            }
          }
        }
        if (!scout.tailDefined) {  // Not a cycle, scout hit an end
          while (cursor ne scout) {
            appendCursorElement()
            cursor = cursor.tail
          }
          if (cursor.nonEmpty) {
            appendCursorElement()
          }
        }
        else {
          // Cycle.
          // If we have a prefix of length P followed by a cycle of length C,
          // the scout will be at position (P%C) in the cycle when the cursor
          // enters it at P.  They'll then collide when the scout advances another
          // C - (P%C) ahead of the cursor.
          // If we run the scout P farther, then it will be at the start of
          // the cycle: (C - (P%C) + (P%C)) == C == 0.  So if another runner
          // starts at the beginning of the prefix, they'll collide exactly at
          // the start of the loop.
          var runner = this
          var k = 0
          while (runner ne scout) {
            runner = runner.tail
            scout = scout.tail
            k += 1
          }
          // Now runner and scout are at the beginning of the cycle.  Advance
          // cursor, adding to string, until it hits; then we'll have covered
          // everything once.  If cursor is already at beginning, we'd better
          // advance one first unless runner didn't go anywhere (in which case
          // we've already looped once).
          if ((cursor eq scout) && (k > 0)) {
            appendCursorElement()
            cursor = cursor.tail
          }
          while (cursor ne scout) {
            appendCursorElement()
            cursor = cursor.tail
          }
        }
      }
      if (cursor.nonEmpty) {
        // Either undefined or cyclic; we can check with tailDefined
        if (!cursor.tailDefined) b.append(sep).append("<not computed>")
        else b.append(sep).append("<cycle>")
      }
    }
    b.append(end)
    b
  }

  /** Returns a string representation of this collection. Undefined elements are
   *         represented with `"_"`, an undefined tail is represented with `"&lt;not computed&gt;"`,
   *         and cycles are represented with `"&lt;cycle&gt;"`
   *
   *         Examples:
   *
   *           - `"Stream(_, &lt;not computed&gt;)"`, a non-empty stream, whose head has not been
   *             evaluated ;
   *           - `"Stream(_, 1, _, &lt;not computed&gt;)"`, a stream with at least three elements,
   *             the second one has been evaluated ;
   *           - `"Stream(1, 2, 3, &lt;cycle&gt;)"`, an infinite stream that contains
   *             a cycle at the fourth element.
   */
  override def toString() = addStringNoForce(new JStringBuilder(className), "(", ", ", ")").toString

  /** Returns `true` if this stream is known to be finite, which is decided without
   *  computing anything that has not been computed already.
   *
   *  An empty stream qualifies, and so does one whose tails have all been computed down
   *  to the empty stream. A stream that ends in an uncomputed tail, and a cyclic one,
   *  both give `false`, the latter because its tails never reach the empty stream.
   */
  @deprecated("Check .knownSize instead of .hasDefiniteSize for more actionable information (see scaladoc for details)", "2.13.0")
  override def hasDefiniteSize: Boolean = isEmpty || {
    if (!tailDefined) false
    else {
      // Two-iterator trick (2x & 1x speed) for cycle detection.
      var those = this
      var these = tail
      while (those ne these) {
        if (these.isEmpty) return true
        if (!these.tailDefined) return false
        these = these.tail
        if (these.isEmpty) return true
        if (!these.tailDefined) return false
        these = these.tail
        if (those eq these) return false
        those = those.tail
      }
      false  // Cycle detected
    }
  }
}

@deprecated("Use LazyList (which is fully lazy) instead of Stream (which has a lazy tail only)", "2.13.0")
@SerialVersionUID(3L)
object Stream extends SeqFactory[Stream] {

  /* !!! #11997 This `object cons` must be defined lexically *before* `class Cons` below.
   * Otherwise it prevents Scala.js from building on Windows.
   */
  /** An alternative way of building and matching Streams using Stream.cons(hd, tl). */
  object cons {
    /** A stream consisting of a given first element and remaining elements.
     *
     *  @tparam A the element type of the stream
     *  @param hd   The first element of the result stream
     *  @param tl   The remaining elements of the result stream
     *  @return a non-empty `Stream` whose head is `hd` and whose tail is lazily produced from `tl`
     */
    def apply[A](hd: A, tl: => Stream[A]): Stream[A] = new Cons(hd, tl)

    /** Maps a stream to its head and tail.
     *
     *  @tparam A the element type of the stream
     *  @param xs the stream to decompose
     *  @return `Some((head, tail))` if `xs` is non-empty, or `None` if `xs` is empty
     */
    def unapply[A](xs: Stream[A]): Option[(A, Stream[A])] = #::.unapply(xs)
  }

  //@SerialVersionUID(3L) //TODO Putting an annotation on Stream.empty causes a cyclic dependency in unpickling
  object Empty extends Stream[Nothing] {
    /** Returns `true`; this is the empty stream. */
    override def isEmpty: Boolean = true
    /** Returns nothing; the empty stream has no elements.
     *
     *  @throws NoSuchElementException always
     */
    override def head: Nothing = throw new NoSuchElementException("head of empty stream")
    /** Returns nothing; the empty stream has no tail.
     *
     *  @throws UnsupportedOperationException always
     */
    override def tail: Stream[Nothing] = throw new UnsupportedOperationException("tail of empty stream")
    /** Forces evaluation of the whole `Stream` and returns it.
     *
     *  @note Often we use `Stream`s to represent an infinite set or series.  If
     *  that's the case for your particular `Stream` then this function will never
     *  return and will probably crash the VM with an `OutOfMemory` exception.
     *  This function will not hang on a finite cycle, however.
     *
     *  @return The fully realized `Stream`.
     */
    def force: this.type = this
    /** Returns `0`; the size of the empty stream is known without computing anything. */
    override def knownSize: Int = 0
    /** Returns `false`; the empty stream has no tail to compute. */
    protected def tailDefined: Boolean = false
  }

  /** A non-empty stream, holding a computed head and a tail that is computed on first
   *  access and remembered afterwards.
   *
   *  @tparam A the element type of the stream
   *  @param head the first element of this stream
   *  @param tl the expression producing the rest of this stream, evaluated once if it
   *            returns normally and retried on the next access if it throws
   */
  @SerialVersionUID(3L)
  final class Cons[A](override val head: A, tl: => Stream[A]) extends Stream[A] {
    /** Returns `false`; a cons cell always holds at least its head. */
    override def isEmpty: Boolean = false
    @volatile private var tlVal: Stream[A] = compiletime.uninitialized
    @volatile private var tlGen: (() => Stream[A]) | Null = () => tl
    /** Returns `true` once the tail of this stream has been computed. */
    protected def tailDefined: Boolean = tlGen eq null
    /** Returns the rest of this stream, evaluating the tail expression on the first call
     *  and returning the remembered result on every call after that.
     *
     *  The evaluation is synchronized, so that concurrent callers agree on one tail and
     *  the expression is evaluated once. Nothing is remembered if it throws, so the next
     *  access evaluates it again.
     */
    override def tail: Stream[A] = {
      if (!tailDefined)
        synchronized {
          if (!tailDefined) {
            tlVal = tlGen.nn()
            tlGen = null
          }
        }
      tlVal
    }

    /** Forces evaluation of the whole `Stream` and returns it.
     *
     *  @note Often we use `Stream`s to represent an infinite set or series.  If
     *  that's the case for your particular `Stream` then this function will never
     *  return and will probably crash the VM with an `OutOfMemory` exception.
     *  This function will not hang on a finite cycle, however.
     *
     *  @return The fully realized `Stream`.
     */
    def force: this.type = {
      // Use standard 2x 1x iterator trick for cycle detection ("those" is slow one)
      var these, those: Stream[A] = this
      if (!these.isEmpty) these = these.tail
      while (those ne these) {
        if (these.isEmpty) return this
        these = these.tail
        if (these.isEmpty) return this
        these = these.tail
        if (these eq those) return this
        those = those.tail
      }
      this
    }

  }

  /** Provides the `#::` and `#:::` operators on a by-name stream, so that the right
   *  operand of a cons is not evaluated when the cons cell is built.
   *
   *  @tparam A the element type of the stream
   *  @param l the stream to defer, evaluated when the operator's result asks for its tail,
   *           or at once by `#:::` when its left operand turns out to be empty
   */
  implicit def toDeferrer[A](l: => Stream[A]): Deferrer[A] = new Deferrer[A](() => l)

  /** Holds the deferred right operand of a `#::` or `#:::` operation on streams.
   *
   *  @tparam A the element type of the deferred stream
   */
  final class Deferrer[A] private[Stream] (private val l: () => Stream[A]) extends AnyVal {
    /** Constructs a `Stream` consisting of a given first element followed by elements
     *  from another `Stream`.
     */
    def #:: [B >: A](elem: B): Stream[B] = new Cons(elem, l())
    /** Constructs a `Stream` consisting of the concatenation of the given `Stream` and
     *  another `Stream`.
     */
    def #:::[B >: A](prefix: Stream[B]): Stream[B] = prefix lazyAppendedAll l()
  }

  object #:: {
    /** Matches a non-empty stream against its head and tail.
     *
     *  Matching computes the tail of `s`, but nothing beyond it.
     *
     *  @tparam A the element type of the stream
     *  @param s the stream to decompose
     *  @return `Some((head, tail))` if `s` is non-empty, or `None` if it is empty
     */
    def unapply[A](s: Stream[A]): Option[(A, Stream[A])] =
      if (s.nonEmpty) Some((s.head, s.tail)) else None
  }

  /** Returns a stream containing the elements of `coll`.
   *
   *  If `coll` is already a stream it is returned unchanged; otherwise its iterator is
   *  drained one element at a time as the result is forced, the first element at once.
   *
   *  @tparam A the element type
   *  @param coll the collection whose elements are to be contained
   */
  def from[A](coll: collection.IterableOnce[A]): Stream[A] = coll match {
    case coll: Stream[A] => coll
    case _ => fromIterator(coll.iterator)
  }

  /**
   *  @tparam A type of elements
   *
   *  @param it Source iterator
   *  @return A `Stream[A]` that gets its elements from the given `Iterator`.
   */
  // Note that the resulting `Stream` will be effectively iterable more than once because
  // `Stream` memoizes its elements
  def fromIterator[A](it: Iterator[A]): Stream[A] =
    if (it.hasNext) {
      new Stream.Cons(it.next(), fromIterator(it))
    } else Stream.Empty

  /** Returns the empty stream.
   *
   *  All calls return the same instance, which is shared across element types.
   *
   *  @tparam A the element type of the stream
   */
  def empty[A]: Stream[A] = Empty

  /** Returns a new builder that collects elements into a stream.
   *
   *  The elements are collected strictly, in an `ArrayBuffer`, and turned into a stream
   *  when the result is asked for; this builder defers nothing.
   *
   *  @tparam A the element type of the stream being built
   */
  override def newBuilder[A]: mutable.Builder[A, Stream[A]] = ArrayBuffer.newBuilder[A].mapResult(array => from(array))

  private[immutable] def withFilter[A](l: Stream[A] @uncheckedVariance, p: A => Boolean): collection.WithFilter[A, Stream] =
    new WithFilter[A](l, p)

  private final class WithFilter[A](l: Stream[A] @uncheckedVariance, p: A => Boolean) extends collection.WithFilter[A, Stream] {
    private var s: Stream[A] = l // set to null to allow GC after filtered
    private lazy val filtered: Stream[A] = { val f = s.filter(p); s = nullForGC[Stream[A]]; f } // don't set to null if throw during filter
    /** Returns a stream of the results of applying `f` to the elements that satisfy the
     *  filter's predicate.
     *
     *  The filtering is performed on the first call to any of this wrapper's methods and
     *  its result is kept, which lets the head of the underlying stream be collected.
     *
     *  @tparam B the element type of the resulting stream
     *  @param f the function to apply to each retained element
     */
    def map[B](f: A => B): Stream[B] = filtered.map(f)
    /** Returns a stream of the concatenated results of applying `f` to the elements that
     *  satisfy the filter's predicate.
     *
     *  @tparam B the element type of the resulting stream
     *  @param f the function to apply to each retained element
     */
    def flatMap[B](f: A => IterableOnce[B]): Stream[B] = filtered.flatMap(f)
    /** Applies `f` to each element that satisfies the filter's predicate.
     *
     *  This computes every element of the underlying stream, so it does not terminate on
     *  an infinite stream, and stops early only if the filter's predicate or `f` throws.
     *
     *  @tparam U the result type of `f`, used only for its side effects
     *  @param f the function to apply to each retained element
     */
    def foreach[U](f: A => U): Unit = filtered.foreach(f)
    /** Returns a `WithFilter` that also requires `q`, restricting the elements further.
     *
     *  @param q the additional predicate an element must satisfy
     */
    def withFilter(q: A => Boolean): collection.WithFilter[A, Stream] = new WithFilter(filtered, q)
  }

  /** An infinite Stream that repeatedly applies a given function to a start value.
   *
   *  @tparam A the element type of the stream
   *  @param start the start value of the Stream
   *  @param f     the function that's repeatedly applied
   *  @return      the Stream returning the infinite sequence of values `start, f(start), f(f(start)), ...`
   */
  def iterate[A](start: A)(f: A => A): Stream[A] = {
    cons(start, iterate(f(start))(f))
  }

  /** Creates an infinite Stream starting at `start` and incrementing by
   *  step `step`.
   *
   *  @param start the start value of the Stream
   *  @param step the increment value of the Stream
   *  @return the Stream starting at value `start`.
   */
  def from(start: Int, step: Int): Stream[Int] =
    cons(start, from(start + step, step))

  /** Creates an infinite Stream starting at `start` and incrementing by `1`.
   *
   *  @param start the start value of the Stream
   *  @return the Stream starting at value `start`.
   */
  def from(start: Int): Stream[Int] = from(start, 1)

  /** Creates an infinite Stream containing the given element expression (which
   *  is computed for each occurrence).
   *
   *  @tparam A the element type of the stream
   *  @param elem the element composing the resulting Stream
   *  @return the Stream containing an infinite number of elem
   */
  def continually[A](elem: => A): Stream[A] = cons(elem, continually(elem))


  private[Stream] def filteredTail[A](stream: Stream[A] @uncheckedVariance, p: A => Boolean, isFlipped: Boolean) = {
    cons(stream.head, stream.tail.filterImpl(p, isFlipped))
  }

  private[Stream] def collectedTail[A, B](head: B, stream: Stream[A] @uncheckedVariance, pf: PartialFunction[A, B]) = {
    cons(head, stream.tail.collect(pf))
  }

  /** This serialization proxy is used for Streams which start with a sequence of evaluated cons cells.
   *  The forced sequence is serialized in a compact, sequential format, followed by the unevaluated tail, which uses
   *  standard Java serialization to store the complete structure of unevaluated thunks. This allows the serialization
   *  of long evaluated streams without exhausting the stack through recursive serialization of cons cells.
   */
  @SerialVersionUID(3L)
  class SerializationProxy[A](@transient protected var coll: Stream[A]) extends Serializable {

    private def writeObject(out: ObjectOutputStream): Unit = {
      out.defaultWriteObject()
      var these = coll
      while(these.nonEmpty && these.tailDefined) {
        out.writeObject(these.head)
        these = these.tail
      }
      out.writeObject(SerializeEnd)
      out.writeObject(these)
    }

    private def readObject(in: ObjectInputStream): Unit = {
      in.defaultReadObject()
      val init = new ArrayBuffer[A]
      var initRead = false
      while (!initRead) in.readObject match {
        case SerializeEnd => initRead = true
        case a => init += a.asInstanceOf[A]
      }
      val tail = in.readObject().asInstanceOf[Stream[A]]
      coll = (init ++: tail)
    }

    /** Returns the stream rebuilt from the stream contents, replacing this proxy when the
     *  object is read back.
     */
    protected def readResolve(): Any = coll
  }
}
