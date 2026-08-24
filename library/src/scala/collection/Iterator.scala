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

import scala.collection.mutable.{ArrayBuffer, ArrayBuilder, Builder, ImmutableBuilder}
import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.runtime.ScalaRunTime.nullForGC
import scala.runtime.Statics
import caps.unsafe.untrackedCaptures

/** Iterators are data structures that allow to iterate over a sequence
 *  of elements. They have a `hasNext` method for checking
 *  if there is a next element available, and a `next` method
 *  which returns the next element and advances the iterator.
 *
 *  An iterator is mutable: most operations on it change its state. While it is often used
 *  to iterate through the elements of a collection, it can also be used without
 *  being backed by any collection (see constructors on the companion object).
 *
 *  It is of particular importance to note that, unless stated otherwise, ''one should never
 *  use an iterator after calling a method on it''. The two most important exceptions
 *  are also the sole abstract methods: `next` and `hasNext`.
 *
 *  Both these methods can be called any number of times without having to discard the
 *  iterator. Note that even `hasNext` may cause mutation -- such as when iterating
 *  from an input stream, where it will block until the stream is closed or some
 *  input becomes available.
 *
 *  Consider this example for safe and unsafe use:
 *
 *  ```scala sc:compile
 *  def f[A](it: Iterator[A]) = {
 *   if (it.hasNext) {            // Safe to reuse "it" after "hasNext"
 *     it.next()                  // Safe to reuse "it" after "next"
 *     val remainder = it.drop(2) // it is *not* safe to use "it" again after this line!
 *     remainder.take(2)          // it is *not* safe to use "remainder" after this line!
 *   } else it
 *  }
 *  ```
 *
 *  @define mayNotTerminateInf
 *  Note: may not terminate for infinite iterators.
 *  @define preservesIterator
 *  The iterator remains valid for further use whatever result is returned.
 *  @define consumesIterator
 *  After calling this method, one should discard the iterator it was called
 *  on. Using it is undefined and subject to change.
 *  @define consumesAndProducesIterator
 *  After calling this method, one should discard the iterator it was called
 *  on, and use only the iterator that was returned. Using the old iterator
 *  is undefined, subject to change, and may result in changes to the new
 *  iterator as well.
 *  @define consumesTwoAndProducesOneIterator
 *  After calling this method, one should discard the iterator it was called
 *  on, as well as the one passed as a parameter, and use only the iterator
 *  that was returned. Using the old iterators is undefined, subject to change,
 *  and may result in changes to the new iterator as well.
 *  @define consumesOneAndProducesTwoIterators
 *  After calling this method, one should discard the iterator it was called
 *  on, and use only the iterators that were returned. Using the old iterator
 *  is undefined, subject to change, and may result in changes to the new
 *  iterators as well.
 *  @define coll iterator
 *
 *  @tparam A the element type of the iterator
 */
trait Iterator[+A] extends IterableOnce[A] with IterableOnceOps[A, Iterator, Iterator[A]] {
  self: Iterator[A]^ =>

  /** Checks if there is a next element available.
   *
   *  @return `true` if there is a next element, `false` otherwise
   *  @note   Reuse: $preservesIterator
   */
  def hasNext: Boolean

  @deprecated("hasDefiniteSize on Iterator is the same as isEmpty", "2.13.0")
  @`inline` override final def hasDefiniteSize = isEmpty

  /** Returns the next element and advance the iterator.
   *
   *  @return the next element.
   *  @throws NoSuchElementException if there is no next element.
   *  @note   Reuse: Advances the iterator, which may exhaust the elements. It is valid to
   *         make additional calls on the iterator.
   */
  @throws[NoSuchElementException]
  def next(): A

  /** Returns this iterator, since an iterator is its own iterator.
   *
   *  @note Reuse: $preservesIterator
   */
  @inline final def iterator = this

  /** Wraps the value of `next()` in an option.
   *
   *  @return `Some(next)` if a next element exists, `None` otherwise.
   */
  def nextOption(): Option[A] = if (hasNext) Some(next()) else None

  /** Tests whether this iterator contains a given value as an element.
   *  $mayNotTerminateInf
   *
   *  @param elem  the element to test.
   *  @return     `true` if this iterator produces some value that is
   *               is equal (as determined by `==`) to `elem`, `false` otherwise.
   *  @note        Reuse: $consumesIterator
   */
  def contains(elem: Any): Boolean = exists(_ == elem)    // Note--this seems faster than manual inlining!

  /** Creates a buffered iterator from this iterator.
   *
   *  @see [[scala.collection.BufferedIterator]]
   *  @return  a buffered iterator producing the same values as this iterator.
   *  @note    Reuse: $consumesAndProducesIterator
   */
  def buffered: BufferedIterator[A]^{this} = new AbstractIterator[A] with BufferedIterator[A] {
    private var hd: A = compiletime.uninitialized
    private var hdDefined: Boolean = false

    def head: A = {
      if (!hdDefined) {
        hd = next()
        hdDefined = true
      }
      hd
    }

    override def knownSize = {
      val thisSize = self.knownSize
      if (thisSize >= 0 && hdDefined) thisSize + 1
      else thisSize
    }

    def hasNext =
      hdDefined || self.hasNext

    def next() =
      if (hdDefined) {
        hdDefined = false
        hd
      } else self.next()
  }

  /** A flexible iterator for transforming an `Iterator[A]` into an
   *  `Iterator[Seq[A]]`, with configurable sequence size, step, and
   *  strategy for dealing with remainder elements which don't fit evenly
   *  into the last group.
   *
   *  A `GroupedIterator` is yielded by `grouped` and by `sliding`,
   *  where the `step` may differ from the group `size`.
   *
   *  @tparam B the element type of the sequences produced by the grouped iterator, a supertype of `A`
   *  @param self the underlying iterator to group
   *  @param size the number of elements per group
   *  @param step the distance between the first elements of successive groups
   */
  class GroupedIterator[B >: A](self: Iterator[B]^, size: Int, step: Int) extends AbstractIterator[immutable.Seq[B]] {

    require(size >= 1 && step >= 1, s"size=$size and step=$step, but both must be positive")

    private var buffer: Array[B] | Null = null          // current result
    private var prev: Array[B] | Null = null            // if sliding, overlap from previous result
    private var first = true                            // if !first, advancing may skip ahead
    private var filled = false                          // whether the buffer is "hot"
    private var partial = true                          // whether to emit partial sequence
    private var padding: (() -> B) | Null = null        // what to pad short sequences with
    private def pad = padding != null                   // irrespective of partial flag
    private def newBuilder = {
      val b = ArrayBuilder.make[Any]
      val k = self.knownSize
      if (k > 0) b.sizeHint(k min size)                       // if k < size && !partial, buffer will grow on padding
      b
    }

    /** Specifies a fill element used to pad a partial segment
     *  so that all segments have the same size.
     *
     *  Any previous setting of `withPartial` is ignored,
     *  as the last group will always be padded to `size` elements.
     *
     *  The by-name argument is evaluated for each fill element.
     *
     *  @param x The element that will be appended to the last segment, if necessary.
     *  @return  The same iterator, and *not* a new iterator.
     *  @note    This method mutates the iterator it is called on, which can be safely used afterwards.
     *  @note    This method is mutually exclusive with `withPartial`.
     *  @group Configuration
     */
    def withPadding(x: -> B): this.type = {
      padding = () => x
      partial = true        // redundant, as padding always results in complete segment
      this
    }
    /** Specify whether to drop the last segment if it has less than `size` elements.
     *
     *  If this flag is `false`, elements of a partial segment at the end of the iterator
     *  are not returned.
     *
     *  The flag defaults to `true`.
     *
     *  Any previous setting of `withPadding` is ignored,
     *  as the last group will never be padded.
     *  A partial segment is either retained or dropped, per the flag.
     *
     *  @param x `true` if partial segments may be returned, `false` otherwise.
     *  @return  The same iterator, and *not* a new iterator.
     *  @note    This method mutates the iterator it is called on, which can be safely used afterwards.
     *  @note    This method is mutually exclusive with `withPadding`.
     *  @group Configuration
     */
    def withPartial(x: Boolean): this.type = {
      partial = x
      padding = null
      this
    }

    /** Eagerly fetches `size` elements to buffer.
     *
     *  If buffer is dirty and stepping, copy prefix.
     *  If skipping, skip ahead.
     *  Fetches remaining elements.
     *  If unable to deliver size, then pad if padding enabled, otherwise drop segment.
     *  Returns true if successful in delivering `count` elements,
     *  or padded segment, or partial segment.
     *
     *  @return `true` if a segment was successfully buffered, `false` otherwise
     */
    private def fulfill(): Boolean = {
      val builder = newBuilder
      var done = false
      // keep prefix of previous buffer if stepping
      if (prev != null) builder.addAll(prev.nn)
      // skip ahead
      if (!first && step > size) {
        var dropping = step - size
        while (dropping > 0 && self.hasNext) {
          self.next(): Unit
          dropping -= 1
        }
        done = dropping > 0   // skip failed
      }
      var index = builder.length
      if (!done) {
        // advance to rest of segment if possible
        while (index < size && self.hasNext) {
          builder.addOne(self.next())
          index += 1
        }
        // if unable to complete segment, pad if possible
        if (index < size && pad) {
          builder.sizeHint(size)
          while (index < size) {
            builder.addOne(padding.nn())
            index += 1
          }
        }
      }
      // segment must have data, and must be complete unless they allow partial
      val ok = index > 0 && (partial || index == size)
      if (ok) buffer = builder.result().asInstanceOf[Array[B]]
      else prev = null
      ok
    }

    // fill() returns false if no more sequences can be produced
    private def fill(): Boolean = filled || { filled = self.hasNext && fulfill() ; filled }

    /** Tests whether another segment can be produced, buffering it eagerly
     *  from the underlying iterator if it is not already buffered.
     */
    def hasNext = fill()

    /** Returns the next segment, buffering it from the underlying iterator if
     *  it has not already been buffered by `hasNext`.
     *
     *  If sliding with `step < size`, the elements shared with the next
     *  segment are retained for reuse.
     *
     *  @return the next segment, of `size` elements unless it is a partial
     *          segment permitted by `withPartial`
     *  @throws NoSuchElementException if no further segment can be produced
     */
    @throws[NoSuchElementException]
    def next(): immutable.Seq[B] =
      if (!fill()) Iterator.empty.next()
      else {
        filled = false
        val buffer = this.buffer.nn
        // if stepping, retain overlap in prev
        if (step < size) {
          if (first) prev = buffer.drop(step)
          else if (buffer.length == size) Array.copy(src = buffer, srcPos = step, dest = prev.nn, destPos = 0, length = size - step)
          else prev = null
        }
        val res = immutable.ArraySeq.unsafeWrapArray(buffer).asInstanceOf[immutable.ArraySeq[B]]
        this.buffer = null
        first = false
        res
      }
  }

  /** A copy of this $coll with an element value appended until a given target length is reached.
   *
   *  @tparam B      the element type of the returned $coll.
   *  @param   len   the target length
   *  @param   elem  the padding value
   *  @return a new $coll consisting of
   *          all elements of this $coll followed by the minimal number of occurrences of `elem` so
   *          that the resulting collection has a length of at least `len`.
   */
  def padTo[B >: A](len: Int, elem: B): Iterator[B]^{this} = new AbstractIterator[B] {
    private var i = 0

    override def knownSize: Int = {
      val thisSize = self.knownSize
      if (thisSize < 0) -1
      else thisSize max (len - i)
    }

    def next(): B = {
      val b =
        if (self.hasNext) self.next()
        else if (i < len) elem
        else Iterator.empty.next()
      i += 1
      b
    }

    def hasNext: Boolean = self.hasNext || i < len
  }

  /** Partitions this iterator in two iterators according to a predicate.
   *
   *  @param p the predicate on which to partition
   *  @return  a pair of iterators: the iterator that satisfies the predicate
   *           `p` and the iterator that does not.
   *           The relative order of the elements in the resulting iterators
   *           is the same as in the original iterator.
   *  @note    Reuse: $consumesOneAndProducesTwoIterators
   */
  def partition(p: A => Boolean): (Iterator[A]^{this, p}, Iterator[A]^{this, p}) = {
    val (a, b) = duplicate
    (a filter p, b filterNot p)
  }

  /** Returns an iterator which groups this iterator into fixed size
   *  blocks.  Example usages:
   *  ```
   *    // Returns List(List(1, 2, 3), List(4, 5, 6), List(7)))
   *    (1 to 7).iterator.grouped(3).toList
   *    // Returns List(List(1, 2, 3), List(4, 5, 6))
   *    (1 to 7).iterator.grouped(3).withPartial(false).toList
   *    // Returns List(List(1, 2, 3), List(4, 5, 6), List(7, 20, 25)
   *    // Illustrating that withPadding's argument is by-name.
   *    val it2 = Iterator.iterate(20)(_ + 5)
   *    (1 to 7).iterator.grouped(3).withPadding(it2.next).toList
   *  ```
   *
   *  @note Reuse: $consumesAndProducesIterator
   *
   *  @tparam B the element type of the sequences produced by the grouped iterator, a supertype of `A`
   *  @param size the number of elements per group
   *  @return a `GroupedIterator` producing `Seq[B]`s of size `size`, except the
   *          last segment (which may be the only segment) will be truncated
   *          if there are fewer than `size` elements remaining to be grouped.
   *          The truncation behavior can be overridden via `withPartial` or `withPadding`.
   */
  def grouped[B >: A](size: Int): GroupedIterator[B]^{this} =
    new GroupedIterator[B](self, size, size)

  /** Returns an iterator which presents a "sliding window" view of
   *  this iterator.  The first argument is the window size, and
   *  the second argument `step` is how far to advance the window
   *  on each iteration. The `step` defaults to `1`.
   *
   *  The returned `GroupedIterator` can be configured to either
   *  pad a partial result to size `size` or suppress the partial
   *  result entirely.
   *
   *  Example usages:
   *  ```
   *    // Returns List(ArraySeq(1, 2, 3), ArraySeq(2, 3, 4), ArraySeq(3, 4, 5))
   *    (1 to 5).iterator.sliding(3).toList
   *    // Returns List(ArraySeq(1, 2, 3, 4), ArraySeq(4, 5))
   *    (1 to 5).iterator.sliding(4, 3).toList
   *    // Returns List(ArraySeq(1, 2, 3, 4))
   *    (1 to 5).iterator.sliding(4, 3).withPartial(false).toList
   *    // Returns List(ArraySeq(1, 2, 3, 4), ArraySeq(4, 5, 20, 25))
   *    // Illustrating that withPadding's argument is by-name.
   *    val it2 = Iterator.iterate(20)(_ + 5)
   *    (1 to 5).iterator.sliding(4, 3).withPadding(it2.next).toList
   *  ```
   *
   *  @tparam B the element type of the sequences produced by the grouped iterator, a supertype of `A`
   *  @param size the number of elements per group
   *  @param step the distance between the first elements of successive
   *         groups
   *  @return A `GroupedIterator` producing `Seq[B]`s of size `size`, except the
   *          last element (which may be the only element) will be truncated
   *          if there are fewer than `size` elements remaining to be grouped.
   *          This behavior can be configured.
   *
   *  @note Reuse: $consumesAndProducesIterator
   */
  def sliding[B >: A](size: Int, step: Int = 1): GroupedIterator[B]^{this} =
    new GroupedIterator[B](self, size, step)

  /** Produces an iterator containing the cumulative results of applying the
   *  operator `op` going left to right, starting with the value `z`.
   *
   *  The results are computed lazily, as the returned iterator is advanced.
   *
   *  @tparam B the element type of the returned iterator
   *  @param z the initial value, produced first
   *  @param op the operator applied to the previous cumulative result and the
   *            next element of this iterator
   *  @return an iterator producing `z, op(z, x1), op(op(z, x1), x2), ...`
   *          where `x1, x2, ...` are the elements of this iterator
   *  @note   Reuse: $consumesAndProducesIterator
   */
  def scanLeft[B](z: B)(op: (B, A) => B): Iterator[B]^{this, op} = new AbstractIterator[B] {
    // We use an intermediate iterator that iterates through the first element `z`
    // and then that will be modified to iterate through the collection
    private var current: Iterator[B]^{self, op} =
      new AbstractIterator[B] {
        override def knownSize = {
          val thisSize = self.knownSize

          if (thisSize < 0) -1
          else thisSize + 1
        }
        def hasNext: Boolean = true
        def next(): B = {
          // Here we change our self-reference to a new iterator that iterates through `self`
          current = new AbstractIterator[B] {
            private var acc = z
            def next(): B = {
              acc = op(acc, self.next())
              acc
            }
            def hasNext: Boolean = self.hasNext
            override def knownSize = self.knownSize
          }
          z
        }
      }
    override def knownSize = current.knownSize
    def next(): B = current.next()
    def hasNext: Boolean = current.hasNext
  }

  /** Produces an iterator containing the cumulative results of applying the
   *  operator `op` going right to left, ending with the value `z`.
   *
   *  Unlike most iterator methods, calling this method immediately consumes
   *  this iterator into an internal buffer, so it must not be called on an
   *  infinite iterator.
   *
   *  @tparam B the element type of the returned iterator
   *  @param z the initial value, produced last
   *  @param op the operator applied to an element of this iterator and the
   *            cumulative result to its right
   *  @return an iterator producing `..., op(x(n-1), op(xn, z)), op(xn, z), z`
   *          where `x1, ..., xn` are the elements of this iterator
   *  @note   Reuse: $consumesAndProducesIterator
   */
  @deprecated("Call scanRight on an Iterable instead.", "2.13.0")
  def scanRight[B](z: B)(op: (A, B) => B): Iterator[B]^{this, op} = ArrayBuffer.from(this).scanRight(z)(op).iterator

  /** Finds index of the first element satisfying some predicate after or at some start index.
   *
   *  $mayNotTerminateInf
   *
   *  @param  p     the predicate used to test elements.
   *  @param  from   the start index
   *  @return the index `>= from` of the first element of this $coll that satisfies the predicate `p`,
   *           or `-1`, if none exists.
   *  @note   Reuse: $consumesIterator
   */
  def indexWhere(p: A => Boolean, from: Int = 0): Int = {
    var i = math.max(from, 0)
    val dropped = drop(from)
    while (dropped.hasNext) {
      if (p(dropped.next())) return i
      i += 1
    }
    -1
  }

  /** Returns the index of the first occurrence of the specified
   *  object in this iterable object.
   *  $mayNotTerminateInf
   *
   *  @tparam B the type of the element to search for, a supertype of `A`
   *  @param  elem  element to search for.
   *  @return the index of the first occurrence of `elem` in the values produced by this iterator,
   *          or -1 if such an element does not exist until the end of the iterator is reached.
   *  @note   Reuse: $consumesIterator
   */
  def indexOf[B >: A](elem: B): Int = indexOf(elem, 0)

  /** Returns the index of the first occurrence of the specified object in this iterable object
   *  after or at some start index.
   *  $mayNotTerminateInf
   *
   *  @tparam B the type of the element to search for, a supertype of `A`
   *  @param elem element to search for.
   *  @param from the start index
   *  @return the index `>= from` of the first occurrence of `elem` in the values produced by this
   *          iterator, or -1 if such an element does not exist until the end of the iterator is
   *          reached.
   *  @note   Reuse: $consumesIterator
   */
  def indexOf[B >: A](elem: B, from: Int): Int = {
    var i = 0
    while (i < from && hasNext) {
      next()
      i += 1
    }

    while (hasNext) {
      if (next() == elem) return i
      i += 1
    }
    -1
  }

  /** Returns the number of elements produced by this iterator, an alias of `size`.
   *
   *  Note: will not terminate for infinite iterators.
   *
   *  @note Reuse: $consumesIterator
   */
  @inline final def length: Int = size

  /** Tests whether this iterator is exhausted, i.e. whether `hasNext` is `false`.
   *
   *  @note Reuse: $preservesIterator
   */
  @deprecatedOverriding("isEmpty is defined as !hasNext; override hasNext instead", "2.13.0")
  override def isEmpty: Boolean = !hasNext

  /** Creates an iterator over all the elements of this iterator that
   *  satisfy the predicate `p`. The order of the elements
   *  is preserved.
   *
   *  @param p the predicate used to test values.
   *  @return  an iterator which produces those values of this iterator which satisfy the predicate `p`.
   *  @note    Reuse: $consumesAndProducesIterator
   */
  def filter(p: A => Boolean): Iterator[A]^{this, p} = filterImpl(p, isFlipped = false)

  /** Creates an iterator over all the elements of this iterator which do not
   *  satisfy the predicate `p`. The order of the elements
   *  is preserved.
   *
   *  @param p the predicate used to test values.
   *  @return  an iterator which produces those values of this iterator which do not satisfy the predicate `p`.
   *  @note    Reuse: $consumesAndProducesIterator
   */
  def filterNot(p: A => Boolean): Iterator[A]^{this, p} = filterImpl(p, isFlipped = true)

  private[collection] def filterImpl(p: A => Boolean, isFlipped: Boolean): Iterator[A]^{this, p} = new AbstractIterator[A] {
    private var hd: A = compiletime.uninitialized
    private var hdDefined: Boolean = false

    def hasNext: Boolean = hdDefined || {
      if (!self.hasNext) return false
      hd = self.next()
      while (p(hd) == isFlipped) {
        if (!self.hasNext) return false
        hd = self.next()
      }
      hdDefined = true
      true
    }

    def next() =
      if (hasNext) {
        hdDefined = false
        hd
      }
      else Iterator.empty.next()
  }

  /** Creates an iterator over all the elements of this iterator that
   *  satisfy the predicate `p`. The order of the elements
   *  is preserved.
   *
   *  **Note:** `withFilter` is the same as `filter` on iterators. It exists so that
   *  for-expressions with filters work over iterators.
   *
   *  @param p the predicate used to test values.
   *  @return  an iterator which produces those values of this iterator which satisfy the predicate `p`.
   *  @note    Reuse: $consumesAndProducesIterator
   */
  def withFilter(p: A => Boolean): Iterator[A]^{this, p} = filter(p)

  /** Creates an iterator by transforming values produced by this iterator with
   *  a partial function, dropping those values for which the partial function
   *  is not defined.
   *
   *  @tparam B the element type of the returned iterator
   *  @param pf the partial function which filters and maps the iterator.
   *  @return   an iterator which yields the value `pf(x)` for each value `x` produced by
   *            this iterator on which `pf` is defined.
   *  @note     Reuse: $consumesAndProducesIterator
   */
  def collect[B](pf: PartialFunction[A, B]^): Iterator[B]^{this, pf} = new AbstractIterator[B] with (A => B) {
    // Manually buffer to avoid extra layer of wrapping with buffered
    private var hd: B = compiletime.uninitialized

    // Little state machine to keep track of where we are
    // Seek = 0; Found = 1; Empty = -1
    // Not in vals because scalac won't make them static (@inline def only works with -optimize)
    // BE REALLY CAREFUL TO KEEP COMMENTS AND NUMBERS IN SYNC!
    private var status = 0/*Seek*/

    def apply(value: A): B = Statics.pfMarker.asInstanceOf[B]

    def hasNext = {
      val marker = Statics.pfMarker
      while (status == 0/*Seek*/) {
        if (self.hasNext) {
          val x = self.next()
          val v = pf.applyOrElse(x, this)
          if (marker ne v.asInstanceOf[AnyRef]) {
            hd = v
            status = 1/*Found*/
          }
        }
        else status = -1/*Empty*/
      }
      status == 1/*Found*/
    }
    def next() = if (hasNext) { status = 0/*Seek*/; hd } else Iterator.empty.next()
  }

  /** Builds a new iterator from this one without any duplicated elements on it.
   *  @return iterator with distinct elements
   *
   *  @note   Reuse: $consumesIterator
   */
  def distinct: Iterator[A]^{this} = distinctBy(identity)

  /** Builds a new iterator from this one without any duplicated elements as determined by `==` after applying
   *  the transforming function `f`.
   *
   *  @tparam B the type of the elements after being transformed by `f`
   *  @param f The transforming function whose result is used to determine the uniqueness of each element
   *  @return iterator with distinct elements
   *
   *  @note   Reuse: $consumesIterator
   */
  def distinctBy[B](f: A -> B): Iterator[A]^{this} = new AbstractIterator[A] {

    private val traversedValues = mutable.HashSet.empty[B]
    private var nextElementDefined: Boolean = false
    private var nextElement: A = compiletime.uninitialized

    def hasNext: Boolean = nextElementDefined || (self.hasNext && {
      val a = self.next()
      if (traversedValues.add(f(a))) {
        nextElement = a
        nextElementDefined = true
        true
      }
      else hasNext
    })

    def next(): A =
      if (hasNext) {
        nextElementDefined = false
        nextElement
      } else {
        Iterator.empty.next()
      }
  }

  /** Creates a new iterator that maps all produced values of this iterator
   *  to new values using a transformation function.
   *
   *  @tparam B the element type of the returned iterator
   *  @param f the transformation function
   *  @return  a new iterator which transforms every value produced by this
   *           iterator by applying the function `f` to it.
   *  @note    Reuse: $consumesAndProducesIterator
   */
  def map[B](f: A => B): Iterator[B]^{this, f} = new AbstractIterator[B] {
    override def knownSize = self.knownSize
    def hasNext = self.hasNext
    def next() = f(self.next())
  }

  /** Creates a new iterator by applying a function to all values produced by
   *  this iterator and concatenating the results.
   *
   *  @tparam B the element type of the returned iterator
   *  @param f the function to apply on each value produced by this iterator
   *  @return  the iterator resulting from applying the given iterator-valued
   *           function `f` to each value produced by this iterator and
   *           concatenating the results.
   *  @note    Reuse: $consumesAndProducesIterator
   */
  def flatMap[B](f: A => IterableOnce[B]^): Iterator[B]^{this, f} = new AbstractIterator[B] {
    private var cur: Iterator[B]^{f} = Iterator.empty
    /** Trillium logic boolean: -1 = unknown, 0 = false, 1 = true. */
    private var _hasNext: Int = -1

    def nextCur(): Unit = {
      cur = Iterator.empty
      cur = f(self.next()).iterator
      _hasNext = -1
    }

    def hasNext: Boolean = {
      if (_hasNext == -1) {
        while (!cur.hasNext) {
          if (!self.hasNext) {
            _hasNext = 0
            // since we know we are exhausted, we can release cur for gc, and as well replace with
            // static Iterator.empty which will support efficient subsequent `hasNext`/`next` calls
            cur = Iterator.empty
            return false
          }
          nextCur()
        }
        _hasNext = 1
        true
      } else _hasNext == 1
    }
    def next(): B = {
      if (hasNext) {
        _hasNext = -1
      }
      cur.next()
    }
  }

  /** Creates a new iterator by concatenating the elements of the collections
   *  produced by this iterator.
   *
   *  @tparam B the element type of the collections produced by this iterator
   *  @param ev evidence that each element of this iterator can be treated as
   *            an `IterableOnce[B]`
   *  @return   an iterator producing, in order, the elements of each collection
   *            produced by this iterator.
   *  @note     Reuse: $consumesAndProducesIterator
   */
  def flatten[B](implicit ev: A -> IterableOnce[B]): Iterator[B]^{this} =
    flatMap[B](ev)

  /** Concatenates this iterator with the elements of another collection.
   *
   *  The by-name argument `xs` is evaluated lazily: not when this method is
   *  called, but only once the returned iterator has produced all values of
   *  this iterator and more are demanded. Repeated concatenations are
   *  efficient because appended collections are kept in a flat queue rather
   *  than in nested iterators.
   *
   *  @tparam B the element type of the returned iterator, a supertype of `A`
   *  @param xs the collection whose elements follow the values of this iterator
   *  @return   an iterator producing the values of this iterator, followed by
   *            the elements of `xs`.
   *  @note     Reuse: $consumesTwoAndProducesOneIterator
   */
  def concat[B >: A](xs: => IterableOnce[B]^): Iterator[B]^{this, xs} = new Iterator.ConcatIterator[B](self).concat(xs)

  @`inline` final def ++ [B >: A](xs: => IterableOnce[B]^): Iterator[B]^{this, xs} = concat(xs)

  /** Selects the first `n` values of this iterator.
   *
   *  @param n the number of values to take
   *  @return  an iterator producing only the first `n` values of this iterator,
   *           or else the whole iterator, if it produces fewer than `n` values.
   *  @note    Reuse: $consumesAndProducesIterator
   */
  def take(n: Int): Iterator[A]^{this} = sliceIterator(0, n max 0)

  /** Takes the longest prefix of values produced by this iterator that satisfy
   *  a predicate.
   *
   *  @param p the predicate used to test elements.
   *  @return  an iterator producing the values of this iterator, until
   *           this iterator produces a value that does not satisfy
   *           the predicate `p`.
   *  @note    Reuse: $consumesAndProducesIterator
   */
  def takeWhile(p: A => Boolean): Iterator[A]^{this, p} = new AbstractIterator[A] {
    private var hd: A = compiletime.uninitialized
    private var hdDefined: Boolean = false
    private var tail: Iterator[A]^{self} = self

    def hasNext = hdDefined || tail.hasNext && {
      hd = tail.next()
      if (p(hd)) hdDefined = true
      else tail = Iterator.empty
      hdDefined
    }
    def next() = if (hasNext) { hdDefined = false; hd } else Iterator.empty.next()
  }

  /** Selects all values of this iterator except the first `n` ones.
   *
   *  The skipped values are not consumed immediately, but only once the
   *  returned iterator is queried.
   *
   *  @param n the number of values to drop
   *  @return  an iterator producing all values of this iterator except the
   *           first `n` ones, or else the empty iterator, if this iterator
   *           produces fewer than `n` values.
   *  @note    Reuse: $consumesAndProducesIterator
   */
  def drop(n: Int): Iterator[A]^{this} = sliceIterator(n, -1)

  /** Skips the longest prefix of values produced by this iterator that satisfy
   *  a predicate.
   *
   *  @param p the predicate used to skip values.
   *  @return  an iterator producing the values of this iterator starting with
   *           the first value that does not satisfy the predicate `p`.
   *  @note    Reuse: $consumesAndProducesIterator
   */
  def dropWhile(p: A => Boolean): Iterator[A]^{this, p} = new AbstractIterator[A] {
    // Magic value: -1 = hasn't dropped, 0 = found first, 1 = defer to parent iterator
    private var status = -1
    // Local buffering to avoid double-wrap with .buffered
    private var fst: A = compiletime.uninitialized
    def hasNext: Boolean =
      if (status == 1) self.hasNext
      else if (status == 0) true
      else {
        while (self.hasNext) {
          val a = self.next()
          if (!p(a)) {
            fst = a
            status = 0
            return true
          }
        }
        status = 1
        false
      }
    def next() =
      if (hasNext) {
        if (status == 1) self.next()
        else {
          status = 1
          fst
        }
      }
      else Iterator.empty.next()
  }

  /** @inheritdoc
   *
   *  @note    Reuse: $consumesOneAndProducesTwoIterators
   *
   *  @param p the predicate used to partition elements into the leading and trailing iterators
   *  @return a pair of iterators: the longest prefix of this iterator whose
   *          elements all satisfy `p`, and the remainder of this iterator
   *          starting with the first element that does not satisfy `p`
   */
  def span(p: A => Boolean): (Iterator[A]^{this, p}, Iterator[A]^{this, p}) = {
    /*
     * Giving a name to following iterator (as opposed to trailing) because
     * anonymous class is represented as a structural type that trailing
     * iterator is referring (the finish() method) and thus triggering
     * handling of structural calls. It's not what's intended here.
     */
    final class Leading extends AbstractIterator[A] {
      private var lookahead: mutable.Queue[A] | Null = null
      private var hd: A = compiletime.uninitialized
      /* Status is kept with magic numbers
       *   1 means next element is in hd and we're still reading into this iterator
       *   0 means we're still reading but haven't found a next element
       *   -1 means we are done reading into the iterator, so we must rely on lookahead
       *   -2 means we are done but have saved hd for the other iterator to use as its first element
       */
      private var status = 0
      private def store(a: A): Unit = {
        if (lookahead == null) lookahead = new mutable.Queue[A]
        lookahead.nn += a
      }
      /** Tests whether the prefix has another element, either buffered in the
       *  lookahead queue or read ahead from the underlying iterator and tested
       *  against `p`.
       */
      def hasNext = {
        if (status < 0) (lookahead ne null) && lookahead.nn.nonEmpty
        else if (status > 0) true
        else {
          if (self.hasNext) {
            hd = self.next()
            status = if (p(hd)) 1 else -2
          }
          else status = -1
          status > 0
        }
      }
      /** Returns the next element of the prefix, taken from the lookahead
       *  queue if the underlying iterator has been handed over to the trailing
       *  iterator.
       *
       *  @throws NoSuchElementException if the prefix is exhausted
       */
      def next() = {
        if (hasNext) {
          if (status == 1) { status = 0; hd }
          else lookahead.nn.dequeue()
        }
        else Iterator.empty.next()
      }
      /** Hands the underlying iterator over to the trailing iterator, reading
       *  all remaining prefix elements into the lookahead queue.
       *
       *  @return `true` if an element failing `p` was found and saved as the
       *          trailer, `false` if the underlying iterator was exhausted first
       */
      @tailrec
      def finish(): Boolean = status match {
        case -2 => status = -1 ; true
        case -1 => false
        case  1 => store(hd) ; status = 0 ; finish()
        case  0 =>
          status = -1
          while (self.hasNext) {
            val a = self.next()
            if (p(a)) store(a)
            else {
              hd = a
              return true
            }
          }
          false
      }
      /** Returns the first element that failed `p`, valid once `finish()` has returned `true`. */
      def trailer: A = hd
    }

    val leading = new Leading

    val trailing = new AbstractIterator[A] {
      private var myLeading = leading
      /* Status flag meanings:
       *   -1 not yet accessed
       *   0 single element waiting in leading
       *   1 defer to self
       *   2 self.hasNext already
       *   3 exhausted
       */
      private var status = -1
      def hasNext = status match {
        case 3 => false
        case 2 => true
        case 1 => if (self.hasNext) { status = 2 ; true } else { status = 3 ; false }
        case 0 => true
        case _ =>
          if (myLeading.finish()) { status = 0 ; true } else { status = 1 ; myLeading = nullForGC[Leading]; hasNext }
      }
      def next() = {
        if (hasNext) {
          if (status == 0) {
            status = 1
            val res = myLeading.trailer
            myLeading = nullForGC[Leading]
            res
          } else {
            status = 1
            self.next()
          }
        }
        else Iterator.empty.next()
      }
    }

    (leading, trailing)
  }

  /** Creates an iterator returning an interval of the values produced by this iterator.
   *
   *  @param from the index of the first element in this iterator which forms
   *              part of the slice. If negative, the slice starts at zero.
   *  @param until the index of the first element following the slice. If
   *               negative, the slice is empty.
   *  @return an iterator which advances this iterator past the first `from`
   *          elements, and then produces at most `until - from` elements.
   *  @note   Reuse: $consumesAndProducesIterator
   */
  def slice(from: Int, until: Int): Iterator[A]^{this} = sliceIterator(from, until max 0)

  /** Creates an optionally bounded slice, unbounded if `until` is negative.
   *
   *  @param from the index of the first element in the slice
   *  @param until the index of the first element following the slice, or negative for unbounded
   *  @return an iterator producing the elements of this iterator from index
   *          `from` (inclusive) up to index `until` (exclusive), or to the end
   *          of this iterator if `until` is negative
   */
  protected def sliceIterator(from: Int, until: Int): Iterator[A]^{this} = {
    val lo = from max 0
    val rest =
      if (until < 0) -1            // unbounded
      else if (until <= lo) 0      // empty
      else until - lo              // finite

    if (rest == 0) Iterator.empty
    else new Iterator.SliceIterator(this, lo, rest)
  }

  /** Creates an iterator formed from this iterator and another iterable
   *  collection by combining corresponding values in pairs. If one of the two
   *  is longer than the other, its remaining elements are ignored.
   *
   *  @tparam B the element type of `that`
   *  @param that the collection providing the second half of each result pair
   *  @return a new iterator containing pairs consisting of corresponding
   *          elements of this iterator and `that`. The number of elements
   *          produced by the new iterator is the minimum of the number of
   *          elements produced by this iterator and `that`.
   *  @note   Reuse: $consumesTwoAndProducesOneIterator
   */
  def zip[B](that: IterableOnce[B]^): Iterator[(A, B)]^{this, that} = new AbstractIterator[(A, B)] {
    val thatIterator = that.iterator
    override def knownSize = self.knownSize min thatIterator.knownSize
    def hasNext = self.hasNext && thatIterator.hasNext
    def next() = (self.next(), thatIterator.next())
  }

  /** Creates an iterator formed from this iterator and another iterable
   *  collection by combining corresponding elements in pairs. If one of the
   *  two is shorter than the other, placeholder elements are used to extend
   *  the shorter one to the length of the longer.
   *
   *  @tparam A1 the type of the first half of each result pair, a supertype of `A`
   *  @tparam B the element type of `that`
   *  @param that the collection providing the second half of each result pair
   *  @param thisElem the element used to pad the pairs if this iterator is
   *                  shorter than `that`
   *  @param thatElem the element used to pad the pairs if `that` is shorter
   *                  than this iterator
   *  @return a new iterator containing pairs consisting of corresponding
   *          values of this iterator and `that`. The number of elements
   *          produced by the new iterator is the maximum of the number of
   *          elements produced by this iterator and `that`.
   *  @note   Reuse: $consumesTwoAndProducesOneIterator
   */
  def zipAll[A1 >: A, B](that: IterableOnce[B]^, thisElem: A1, thatElem: B): Iterator[(A1, B)]^{this, that} = new AbstractIterator[(A1, B)] {
    val thatIterator = that.iterator
    override def knownSize = {
      val thisSize = self.knownSize
      val thatSize = thatIterator.knownSize
      if (thisSize < 0 || thatSize < 0) -1
      else thisSize max thatSize
    }
    def hasNext = self.hasNext || thatIterator.hasNext
    def next(): (A1, B) = {
      val next1 = self.hasNext
      val next2 = thatIterator.hasNext
      if(!(next1 || next2)) throw new NoSuchElementException
      (if(next1) self.next() else thisElem, if(next2) thatIterator.next() else thatElem)
    }
  }

  /** Creates an iterator that pairs each value produced by this iterator with
   *  its index, counting from 0.
   *
   *  @return a new iterator containing pairs consisting of each value produced
   *          by this iterator and its index.
   *  @note   Reuse: $consumesAndProducesIterator
   */
  def zipWithIndex: Iterator[(A, Int)]^{this} = new AbstractIterator[(A, Int)] {
    var idx = 0
    override def knownSize = self.knownSize
    def hasNext = self.hasNext
    def next() = {
      val ret = (self.next(), idx)
      idx += 1
      ret
    }
  }

  /** Checks whether corresponding elements of the given iterable collection
   *  compare equal (with respect to `==`) to elements of this $coll.
   *
   *  @tparam B    the type of the elements of collection `that`.
   *  @param that  the collection to compare
   *  @return `true` if both collections contain equal elements in the same order, `false` otherwise.
   */
  def sameElements[B >: A](that: IterableOnce[B]^): Boolean = {
    val those = that.iterator
    while (hasNext) {
      if (!those.hasNext) return false
      if (next() != those.next()) return false
    }
    !those.hasNext
  }

  /** Creates two new iterators that both iterate over the same elements
   *  as this iterator (in the same order).  The duplicate iterators are
   *  considered equal if they are positioned at the same element.
   *
   *  Given that most methods on iterators will make the original iterator
   *  unfit for further use, this methods provides a reliable way of calling
   *  multiple such methods on an iterator.
   *
   *  @return a pair of iterators
   *  @note   The implementation may allocate temporary storage for elements
   *          iterated by one iterator but not yet by the other.
   *  @note   Reuse: $consumesOneAndProducesTwoIterators
   */
  def duplicate: (Iterator[A]^{this}, Iterator[A]^{this}) = {
    val gap = new scala.collection.mutable.Queue[A]
    var ahead: (Iterator[A]^{this}) | Null = null
    class Partner extends AbstractIterator[A] {
      this: Partner^{Iterator.this} =>
      /** Returns the number of remaining elements if it can be computed, `-1` otherwise.
       *
       *  For the partner that is ahead this is the original iterator's
       *  `knownSize`; for the one that is behind, the buffered elements are
       *  added to that.
       */
      override def knownSize: Int = self.synchronized {
        val thisSize = self.knownSize

        if (this eq ahead) thisSize
        else if (thisSize < 0 || gap.knownSize < 0) -1
        else thisSize + gap.knownSize
      }
      /** Tests whether an element remains, either buffered in the shared queue
       *  (when this partner is behind the other) or in the original iterator.
       */
      def hasNext: Boolean = self.synchronized {
        (this ne ahead) && !gap.isEmpty || self.hasNext
      }
      /** Returns the next element, reading it from the original iterator and
       *  buffering it for the other partner if this partner is ahead, or
       *  dequeuing an already buffered element otherwise.
       */
      def next(): A = self.synchronized {
        if (gap.isEmpty) ahead = this
        if (this eq ahead) {
          val e = self.next()
          gap.enqueue(e)
          e
        } else gap.dequeue()
      }
      // to verify partnerhood we use reference equality on gap because
      // type testing does not discriminate based on origin.
      private def compareGap(queue: scala.collection.mutable.Queue[A]) = gap eq queue
      /** Returns the hash code of the queue shared by the two partner iterators, so both partners hash alike. */
      override def hashCode() = gap.hashCode()
      /** Compares this iterator with `other` for equality.
       *
       *  The result is `true` if `other` is a partner iterator created by the
       *  same `duplicate` call and the shared buffer is empty, that is, the
       *  two iterators are positioned at the same element. Any other value is
       *  compared by reference.
       *
       *  @param other the value to compare with
       */
      override def equals(other: Any) = (other: @unchecked) match {
        case x: Partner   => x.compareGap(gap) && gap.isEmpty
        case _            => super.equals(other)
      }
    }
    (new Partner, new Partner)
  }

  /** Returns this iterator with patched values.
   *  Patching at negative indices is the same as patching starting at 0.
   *  Patching at indices at or larger than the length of the original iterator appends the patch to the end.
   *  If more values are replaced than actually exist, the excess is ignored.
   *
   *  @tparam B the element type of the returned iterator, a supertype of `A`
   *  @param from       The start index from which to patch
   *  @param patchElems The iterator of patch values
   *  @param replaced   The number of values in the original iterator that are replaced by the patch.
   *  @return an iterator that yields the elements of this iterator with `replaced`
   *          elements starting at position `from` substituted by the elements of `patchElems`
   *  @note           Reuse: $consumesTwoAndProducesOneIterator
   */
  def patch[B >: A](from: Int, patchElems: Iterator[B]^, replaced: Int): Iterator[B]^{this, patchElems} =
    new AbstractIterator[B] {
      // TODO We should be able to prove that origElems is safe even though it is
      // declared as Iterator[B]^. We could show that origElems is never assigned a
      // fresh `any`. Maybe we can invent another annotation that is checked and that
      // shows that the `^` is just used as an upper bound for concete non-fresh
      // capabilities.
      @untrackedCaptures private var origElems: Iterator[B]^ = self
      // > 0  => that many more elems from `origElems` before switching to `patchElems`
      //   0  => need to drop elems from `origElems` and start using `patchElems`
      //  -1  => have dropped elems from `origElems`, will be using `patchElems` until it's empty
      //         and then using what's left of `origElems` after the drop
      private var state = if (from > 0) from else 0

      // checks state and handles 0 => -1
      @inline private def switchToPatchIfNeeded(): Unit =
        if (state == 0) {
          origElems = origElems drop replaced
          state = -1
        }

      def hasNext: Boolean = {
        switchToPatchIfNeeded()
        origElems.hasNext || patchElems.hasNext
      }

      def next(): B = {
        switchToPatchIfNeeded()
        if (state < 0 /* == -1 */) {
          if (patchElems.hasNext) patchElems.next()
          else origElems.next()
        }
        else {
          if (origElems.hasNext) {
            state -= 1
            origElems.next()
          }
          else {
            state = -1
            patchElems.next()
          }
        }
      }
    }

  /** Applies a side-effecting function to each element as it is produced,
   *  passing the element through unchanged.
   *
   *  The function is invoked lazily: `f` is applied to an element only when,
   *  and each time, that element is produced by the returned iterator.
   *
   *  @tparam U the return type of `f`, which is discarded
   *  @param f the side-effecting function applied to each element
   *  @return  an iterator producing the same values as this iterator
   *  @note    Reuse: $consumesAndProducesIterator
   */
  override def tapEach[U](f: A => U): Iterator[A]^{this, f} = new AbstractIterator[A] {
    override def knownSize = self.knownSize
    override def hasNext = self.hasNext
    override def next() = {
      val _next = self.next()
      f(_next)
      _next
    }
  }

  /** Converts this iterator to a string.
   *
   *  @return `"<iterator>"`
   *  @note    Reuse: $preservesIterator
   */
  override def toString() = "<iterator>"

  /** Returns this iterator.
   *
   *  @note Reuse: $preservesIterator
   */
  @deprecated("Iterator.seq always returns the iterator itself", "2.13.0")
  def seq: this.type = this
}

@SerialVersionUID(3L)
object Iterator extends IterableFactory[Iterator] {

  private val _empty: Iterator[Nothing] = new AbstractIterator[Nothing] {
    def hasNext = false
    def next() = throw new NoSuchElementException("next on empty iterator")
    override def knownSize: Int = 0
    override protected def sliceIterator(from: Int, until: Int): AbstractIterator[Nothing] = this
  }

  /** Creates a target $coll from an existing source collection
   *
   *  @tparam A the type of the collection’s elements
   *  @param source Source collection
   *  @return a new $coll with the elements of `source`
   */
  override def from[A](source: IterableOnce[A]^): Iterator[A]^{source} = source.iterator

  /** The iterator which produces no values.
   *
   *  @tparam T the element type of the empty iterator
   *  @return an iterator that produces no values, whose `hasNext` always
   *          returns `false` and whose `next()` always throws `NoSuchElementException`
   */
  @`inline` final def empty[T]: Iterator[T] = _empty

  /** Creates an iterator that produces the single element `a` and is then exhausted.
   *
   *  @tparam A the element type of the iterator
   *  @param a the single element to produce
   *  @return an iterator that produces `a` once
   */
  def single[A](a: A): Iterator[A] = new AbstractIterator[A] {
    private var consumed: Boolean = false
    def hasNext = !consumed
    def next() = if (consumed) empty.next() else { consumed = true; a }
    override protected def sliceIterator(from: Int, until: Int) =
      if (consumed || from > 0 || until == 0) empty
      else this
  }

  /** Creates an iterator that produces the given elements, in order.
   *
   *  @tparam A the element type of the iterator
   *  @param xs the elements to produce
   *  @return an iterator over the elements `xs`
   */
  override def apply[A](xs: A*): Iterator[A] = xs.iterator

  /**
   *  @tparam A the type of the ${coll}’s elements
   *  @return A builder for $Coll objects.
   */
  def newBuilder[A]: Builder[A, Iterator[A]] =
    new ImmutableBuilder[A, Iterator[A]](empty[A]) {
      override def addOne(elem: A): this.type = { elems = elems ++ single(elem); this }
    }

  /** Creates iterator that produces the results of some element computation a number of times.
   *
   *  @tparam A the element type of the iterator
   *  @param   len  the number of elements returned by the iterator.
   *  @param   elem the element computation
   *  @return  An iterator that produces the results of `n` evaluations of `elem`.
   */
  override def fill[A](len: Int)(elem: => A): Iterator[A]^{elem} = new AbstractIterator[A] {
    private var i = 0
    override def knownSize: Int = (len - i) max 0
    def hasNext: Boolean = i < len
    def next(): A =
      if (hasNext) { i += 1; elem }
      else empty.next()
  }

  /** Creates an iterator producing the values of a given function over a range of integer values starting from 0.
   *
   *  @tparam A the element type of the iterator
   *  @param  end The number of elements returned by the iterator
   *  @param  f   The function computing element values
   *  @return An iterator that produces the values `f(0), ..., f(n -1)`.
   */
  override def tabulate[A](end: Int)(f: Int => A): Iterator[A]^{f} = new AbstractIterator[A] {
    private var i = 0
    override def knownSize: Int = (end - i) max 0
    def hasNext: Boolean = i < end
    def next(): A =
      if (hasNext) { val result = f(i); i += 1; result }
      else empty.next()
  }

  /** Creates an infinite-length iterator which returns successive values from some start value.
   *  @param start the start value of the iterator
   *  @return      the iterator producing the infinite sequence of values `start, start + 1, start + 2, ...`
   */
  def from(start: Int): Iterator[Int] = from(start, 1)

  /** Creates an infinite-length iterator returning values equally spaced apart.
   *
   *  @param start the start value of the iterator
   *  @param step  the increment between successive values
   *  @return      the iterator producing the infinite sequence of values `start, start + 1 * step, start + 2 * step, ...`
   */
  def from(start: Int, step: Int): Iterator[Int] = new AbstractIterator[Int] {
    private var i = start
    def hasNext: Boolean = true
    def next(): Int = { val result = i; i += step; result }
  }

  /** Creates nn iterator returning successive values in some integer interval.
   *
   *  @param start the start value of the iterator
   *  @param end   the end value of the iterator (the first value NOT returned)
   *  @return      the iterator producing values `start, start + 1, ..., end - 1`
   */
  def range(start: Int, end: Int): Iterator[Int] = range(start, end, 1)

  /** An iterator producing equally spaced values in some integer interval.
   *
   *  @param start the start value of the iterator
   *  @param end   the end value of the iterator (the first value NOT returned)
   *  @param step  the increment value of the iterator (must be positive or negative)
   *  @return      the iterator producing values `start, start + step, ...` up to, but excluding `end`
   */
  def range(start: Int, end: Int, step: Int): Iterator[Int] = new AbstractIterator[Int] {
    if (step == 0) throw new IllegalArgumentException("zero step")
    private var i = start
    private var hasOverflowed = false
    override def knownSize: Int = {
      val size = math.ceil((end.toLong - i.toLong) / step.toDouble)
      if (size < 0) 0
      else if (size > Int.MaxValue) -1
      else size.toInt
    }
    def hasNext: Boolean = {
      (step <= 0 || i < end) && (step >= 0 || i > end) && !hasOverflowed
    }
    def next(): Int =
      if (hasNext) {
        val result = i
        val nextValue = i + step
        hasOverflowed = (step > 0) == nextValue < i
        i = nextValue
        result
      }
      else empty.next()
  }

  /** Creates an infinite iterator that repeatedly applies a given function to the previous result.
   *
   *  @tparam T the element type of the iterator
   *  @param start the start value of the iterator
   *  @param f     the function that's repeatedly applied
   *  @return      the iterator producing the infinite sequence of values `start, f(start), f(f(start)), ...`
   */
  def iterate[T](start: T)(f: T => T): Iterator[T]^{f} = new AbstractIterator[T] {
    private var first = true
    private var acc = start
    def hasNext: Boolean = true
    def next(): T = {
      if (first) first = false
      else acc = f(acc)

      acc
    }
  }

  /** Creates an Iterator that uses a function `f` to produce elements of type `A`
   *  and update an internal state of type `S`.
   *
   *  @tparam A   Type of the elements
   *  @tparam S   Type of the internal state
   *  @param init State initial value
   *  @param f    Computes the next element (or returns `None` to signal
   *             the end of the collection)
   *  @return an `Iterator` that produces elements using `f` until `f` returns `None`
   */
  override def unfold[A, S](init: S)(f: S => Option[(A, S)]): Iterator[A]^{f} = new UnfoldIterator(init)(f)

  /** Creates an infinite-length iterator returning the results of evaluating an expression.
   *  The expression is recomputed for every element.
   *
   *  @tparam A the element type of the iterator
   *  @param elem the element computation.
   *  @return the iterator containing an infinite number of results of evaluating `elem`.
   */
  def continually[A](elem: => A): Iterator[A]^{elem} = new AbstractIterator[A] {
    def hasNext = true
    def next() = elem
  }

  /** Creates an iterator to which other iterators can be appended efficiently.
   *  Nested ConcatIterators are merged to avoid blowing the stack.
   *
   *  @tparam A the element type of the iterator
   *  @param from the initial iterator to concatenate, or `null` if starting empty
   */
  private final class ConcatIterator[+A](val from: (Iterator[A @uncheckedVariance]^) | Null) extends AbstractIterator[A] {
    @annotation.stableNull
    private var current: Iterator[A]^{from} | Null = from
    @annotation.stableNull
    private var tail: ConcatIteratorCell[A @uncheckedVariance] | Null = null
    @annotation.stableNull
    private var last: ConcatIteratorCell[A @uncheckedVariance] | Null = null
    private var currentHasNextChecked = false

    /** Tests whether an element remains in any of the concatenated iterators.
     *
     *  Advances past exhausted constituent iterators, merging any nested
     *  `ConcatIterator` encountered into this one to keep the chain flat.
     */
    def hasNext =
      if (currentHasNextChecked) true
      else if (current == null) false
      else if (current.hasNext) {
        currentHasNextChecked = true
        true
      }
      else {
        // If we advanced the current iterator to a ConcatIterator, merge it into this one
        @tailrec def merge(): Unit =
          if (current.isInstanceOf[ConcatIterator[?]]) {
            val c = current.asInstanceOf[ConcatIterator[A]^{from}]
            current = c.current.asInstanceOf
            currentHasNextChecked = c.currentHasNextChecked
            if (c.tail != null) {
              if (last == null) last = c.last
              c.last.nn.tail = tail
              tail = c.tail
            }
            merge()
          }

        // Advance current to the next non-empty iterator
        // current is set to null when all iterators are exhausted
        @tailrec def advance(): Boolean =
          if (tail == null) {
            current = null
            last = null
            false
          }
          else {
            current = tail.headIterator
            if (last eq tail) last = last.nn.tail
            tail = tail.tail
            merge()
            if (currentHasNextChecked) true
            else if (current != null && current.hasNext) {
              currentHasNextChecked = true
              true
            } else advance()
          }

        advance()
      }

    /** Returns the next element of the first non-exhausted constituent iterator.
     *
     *  @throws NoSuchElementException if all constituent iterators are exhausted
     */
    def next()  =
      if (hasNext) {
        currentHasNextChecked = false
        current.nn.next()
      } else Iterator.empty.next()

    /** Appends a collection to this concatenation, returning this same iterator.
     *
     *  Unlike the base implementation, no wrapping iterator is created: the
     *  by-name argument is enqueued in an internal list of cells and only
     *  evaluated when iteration reaches it, so repeated concatenation neither
     *  nests iterators nor risks a stack overflow.
     *
     *  @tparam B the element type of the returned iterator, a supertype of `A`
     *  @param that the collection to append, evaluated on demand
     *  @return this iterator, extended to also produce the elements of `that`
     */
    override def concat[B >: A](that: => IterableOnce[B]^): Iterator[B]^{this, that} = {
      val c = new ConcatIteratorCell[B](that, null).asInstanceOf[ConcatIteratorCell[A]]
      if (tail == null) {
        tail = c
        last = c
      }
      else {
        last.nn.tail = c
        last = c
      }
      if (current == null) current = Iterator.empty
      this
    }
  }

  private final class ConcatIteratorCell[A](head: => IterableOnce[A]^, var tail: (ConcatIteratorCell[A]^) | Null) {
    /** Returns an iterator over this cell's collection, evaluating the by-name `head` expression. */
    def headIterator: Iterator[A]^{this} = head.iterator
  }

  /** Creates a delegating iterator capped by a limit count. Negative limit means unbounded.
   *  Lazily skip to start on first evaluation.  Avoids daisy-chained iterators due to slicing.
   *
   *  @tparam A the element type of the iterator
   *  @param underlying the source iterator to slice
   *  @param start the number of leading elements to drop
   *  @param limit the maximum number of elements to return, or negative for unbounded
   */
  private[scala] final class SliceIterator[A](val underlying: Iterator[A]^, start: Int, limit: Int) extends AbstractIterator[A] {
    private var remaining = limit
    private var dropping  = start
    @inline private def unbounded = remaining < 0
    private def skip(): Unit =
      while (dropping > 0) {
        if (underlying.hasNext) {
          underlying.next()
          dropping -= 1
        } else
          dropping = 0
      }
    /** Returns the number of remaining elements if the underlying iterator's
     *  size is known, `-1` otherwise.
     *
     *  Accounts for elements not yet skipped and, if bounded, for the
     *  remaining limit.
     */
    override def knownSize: Int = {
      val size = underlying.knownSize
      if (size < 0) -1
      else {
        val dropSize = 0 max (size - dropping)
        if (unbounded) dropSize
        else remaining min dropSize
      }
    }
    /** Tests whether an element remains within the limit, first skipping any leading elements still to be dropped. */
    def hasNext = { skip(); remaining != 0 && underlying.hasNext }
    /** Returns the next element, first skipping any leading elements still to be dropped.
     *
     *  @throws NoSuchElementException if the limit has been reached or the
     *          underlying iterator is exhausted
     */
    def next()  = {
      skip()
      if (remaining > 0) {
        remaining -= 1
        underlying.next()
      }
      else if (unbounded) underlying.next()
      else empty.next()
    }
    /** Creates an optionally bounded slice of this slice, unbounded if `until` is negative.
     *
     *  Instead of wrapping this iterator in a further `SliceIterator`, this
     *  implementation adjusts the drop count and limit of this iterator in
     *  place and returns it, avoiding daisy-chained iterators. Only when the
     *  combined drop count overflows `Int` is a second `SliceIterator`
     *  chained on to absorb the excess.
     *
     *  @param from the index of the first element in the slice
     *  @param until the index of the first element following the slice, or negative for unbounded
     *  @return this iterator with adjusted bounds, the empty iterator if the
     *          slice is empty, or a chained iterator if the combined drop
     *          count overflows `Int`
     */
    override protected def sliceIterator(from: Int, until: Int): Iterator[A]^{this} = {
      val lo = from max 0
      def adjustedBound =
        if (unbounded) -1
        else 0 max (remaining - lo)
      val rest =
        if (until < 0) adjustedBound          // respect current bound, if any
        else if (until <= lo) 0               // empty
        else if (unbounded) until - lo        // now finite
        else adjustedBound min (until - lo)   // keep lesser bound
      val sum = dropping + lo
      if (rest == 0) empty
      else if (sum < 0) {
        dropping = Int.MaxValue
        remaining = 0
        this.concat(new SliceIterator(underlying, start = sum - Int.MaxValue, limit = rest))
      }
      else {
        dropping = sum
        remaining = rest
        this
      }
    }
  }

  /** Creates an iterator that uses a function `f` to produce elements of
   *  type `A` and update an internal state of type `S`.
   *
   *  @tparam A the element type produced by the iterator
   *  @tparam S the type of the internal state
   *  @param init the initial state value
   *  @param f a function that, given the current state, returns `Some((nextElement, nextState))`
   *           to produce the next element and updated state, or `None` to signal
   *           the end of the iteration
   */
  private final class UnfoldIterator[A, S](init: S)(f: S => Option[(A, S)]) extends AbstractIterator[A] {
    private var state: S = init
    private var nextResult: Option[(A, S)] | Null = null

    /** Tests whether another element can be produced, applying `f` to the
     *  current state and caching the result for `next()` if it is not already
     *  cached.
     *
     *  @throws NullPointerException if `f` returns `null` instead of an `Option`
     */
    override def hasNext: Boolean = {
      if (nextResult eq null) {
        nextResult = {
          val res = f(state)
          if (res eq null) throw new NullPointerException("null during unfold")
          res
        }
        state = nullForGC[S]
      }
      nextResult.nn.isDefined
    }

    /** Returns the element cached by `hasNext` and advances the internal state.
     *
     *  @throws NoSuchElementException if `f` has returned `None`, i.e. the
     *          iterator is exhausted
     */
    override def next(): A = {
      if (hasNext) {
        val (value, newState) = nextResult.nn.get
        state = newState
        nextResult = null
        value
      } else Iterator.empty.next()
    }
  }
}

/** Explicit instantiation of the `Iterator` trait to reduce class file size in subclasses.
 *
 *  @tparam A the element type of the iterator
 */
abstract class AbstractIterator[+A] extends Iterator[A]
