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

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.tailrec
import scala.annotation.publicInBinary
import mutable.{Builder, ListBuffer}
import scala.collection.generic.{CommonErrors, DefaultSerializable}
import scala.runtime.Statics.releaseFence

/** A class for immutable linked lists representing ordered collections
 *  of elements of type `A`.
 *
 *  This class comes with two implementing case classes `scala.Nil`
 *  and `scala.::` that implement the abstract members `isEmpty`,
 *  `head` and `tail`.
 *
 *  This class is optimal for last-in-first-out (LIFO), stack-like access patterns. If you need another access
 *  pattern, for example, random access or FIFO, consider using a collection more suited to this than `List`.
 *
 *  ## Performance
 *  **Time:** `List` has `O(1)` prepend and head/tail access. Most other operations are `O(n)` on the number of elements in the list.
 *  This includes the index-based lookup of elements, `length`, `append` and `reverse`.
 *
 *  **Space:** `List` implements **structural sharing** of the tail list. This means that many operations are either
 *  zero- or constant-memory cost.
 *  ```scala sc:compile
 *  val mainList = List(3, 2, 1)
 *  val with4 =    4 :: mainList  // re-uses mainList, costs one :: instance
 *  val with42 =   42 :: mainList // also re-uses mainList, cost one :: instance
 *  val shorter =  mainList.tail  // costs nothing as it uses the same 2::1::Nil instances as mainList
 *  ```
 *
 *  @example ```scala sc:compile
 *  // Make a list via the companion object factory
 *  val days = List("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
 *
 *  // Make a list element-by-element
 *  val when = "AM" :: "PM" :: Nil
 *
 *  // Pattern match
 *  days match {
 *    case firstDay :: otherDays =>
 *      println("The first day of the week is: " + firstDay)
 *    case Nil =>
 *      println("There don't seem to be any week days.")
 *  }
 *  ```
 *
 *  @note The functional list is characterized by persistence and structural sharing, thus offering considerable
 *        performance and space consumption benefits in some scenarios if used correctly.
 *        However, note that objects having multiple references into the same functional list (that is,
 *        objects that rely on structural sharing), will be serialized and deserialized with multiple lists, one for
 *        each reference to it. I.e. structural sharing is lost after serialization/deserialization.
 *
 *  @see  [[https://docs.scala-lang.org/overviews/collections-2.13/concrete-immutable-collection-classes.html#lists "Scala's Collection Library overview"]]
 *  section on `Lists` for more information.
 *
 *  @define coll list
 *  @define Coll `List`
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
@SerialVersionUID(3L)
sealed abstract class List[+A]
  extends AbstractSeq[A]
    with LinearSeq[A]
    with LinearSeqOps[A, List, List[A]]
    with StrictOptimizedLinearSeqOps[A, List, List[A]]
    with StrictOptimizedSeqOps[A, List, List[A]]
    with IterableFactoryDefaults[A, List]
    with DefaultSerializable {

  /** The factory used to build lists, the [[List$ `List`]] companion object. */
  override def iterableFactory: SeqFactory[List] = List

  /** Adds an element at the beginning of this list.
   *  @param elem the element to prepend.
   *  @return  a list which contains `x` as first element and
   *           which continues with this list.
   *  Example:
   *  ```scala sc:compile
   *  1 :: List(2, 3) = List(2, 3).::(1) = List(1, 2, 3)
   *  ```
   */
  def :: [B >: A](elem: B): List[B] =  new ::(elem, this)

  /** Adds the elements of a given list in front of this list.
   *
   *  Example:
   *  ```scala sc:compile
   *  List(1, 2) ::: List(3, 4) == List(3, 4).:::(List(1, 2)) // List(1, 2, 3, 4)
   *  ```
   *
   *  @param prefix  The list elements to prepend.
   *  @return a list resulting from the concatenation of the given
   *    list `prefix` and this list.
   */
  def ::: [B >: A](prefix: List[B]): List[B] =
    if (isEmpty) prefix
    else if (prefix.isEmpty) this
    else {
      val result = new ::[B](prefix.head, this)
      var curr = result
      var that = prefix.tail
      while (!that.isEmpty) {
        val temp = new ::[B](that.head, this)
        curr.next = temp
        curr = temp
        that = that.tail
      }
      releaseFence()
      result
    }

  /** Adds the elements of a given list in reverse order in front of this list.
   *  `xs reverse_::: ys` is equivalent to
   *  `xs.reverse ::: ys` but is more efficient.
   *
   *  @param prefix the prefix to reverse and then prepend
   *  @return       the concatenation of the reversed prefix and the current list.
   */
  def reverse_:::[B >: A](prefix: List[B]): List[B] = {
    var these: List[B] = this
    var pres = prefix
    while (!pres.isEmpty) {
      these = pres.head :: these
      pres = pres.tail
    }
    these
  }

  /** Returns `true` if this list is `Nil`, which is the only empty list. */
  override final def isEmpty: Boolean = this eq Nil

  /** Returns a list with `elem` at its head and this list as its tail.
   *
   *  Only one cons cell is allocated: this list becomes the tail of the result and is
   *  not copied.
   *
   *  @tparam B the element type of the returned list, a supertype of `A`
   *  @param elem the element to prepend
   */
  override def prepended[B >: A](elem: B): List[B] = elem :: this

  /** Returns a list consisting of the elements of `prefix` followed by the elements of
   *  this list.
   *
   *  In the general case only `prefix` is copied and this list becomes the tail of the
   *  result; an empty `prefix` gives this list itself. Nothing at all is copied when
   *  this list is empty and `prefix` is a `List` or a `ListBuffer`: the prefix, or the
   *  buffer's list, is then the result.
   *
   *  @tparam B the element type of the returned list, a supertype of `A`
   *  @param prefix the elements to prepend
   */
  override def prependedAll[B >: A](prefix: collection.IterableOnce[B]^): List[B] = (prefix: @unchecked) match {
    case xs: List[B] => xs ::: this
    case _ if prefix.knownSize == 0 => this
    case b: ListBuffer[B] if this.isEmpty => b.toList
    case _ =>
      val iter = prefix.iterator
      if (iter.hasNext) {
        val result = new ::[B](iter.next(), this)
        var curr = result
        while (iter.hasNext) {
          val temp = new ::[B](iter.next(), this)
          curr.next = temp
          curr = temp
        }
        releaseFence()
        result
      } else {
        this
      }
  }

  // When calling appendAll with another list `suffix`, avoid copying `suffix`
  /** Returns a list consisting of the elements of this list followed by the elements of
   *  `suffix`.
   *
   *  When `suffix` is itself a list, only this list is copied and `suffix` becomes the
   *  tail of the result; otherwise both are copied into a new list.
   *
   *  @tparam B the element type of the returned list, a supertype of `A`
   *  @param suffix the elements to append
   */
  override def appendedAll[B >: A](suffix: collection.IterableOnce[B]^): List[B] = suffix match {
    case xs: List[B] => this ::: xs
    case _ => super.appendedAll(suffix)
  }

  /** Returns a list of the first `n` elements of this list, or of all its elements if it
   *  has fewer than `n`.
   *
   *  The elements taken are copied, so the result shares nothing with this list, except
   *  when the whole list is taken, in which case this list itself is returned. A
   *  non-positive `n` gives `Nil`.
   *
   *  @param n the number of elements to take
   */
  override def take(n: Int): List[A] = if (isEmpty || n <= 0) Nil else {
    val h = new ::(head, Nil)
    var t = h
    var rest = tail
    var i = 1
    while ({if (rest.isEmpty) return this; i < n}) {
      i += 1
      val nx = new ::(rest.head, Nil)
      t.next = nx
      t = nx
      rest = rest.tail
    }
    releaseFence()
    h
  }

  /** Returns the elements of this list from `from` up to but not including `until`.
   *
   *  A negative `from` is treated as 0 and an `until` past the end is harmless, so no
   *  exception is thrown for out-of-range values. The elements before `from` are
   *  dropped without copying. The ones kept are copied unless `until` reaches the end
   *  of this list, in which case the suffix is shared rather than copied.
   *
   *  @param from the index of the first element of the slice
   *  @param until the index one past the last element of the slice
   *  @return a list of the elements at the indices in the interval, `Nil` if `until` is
   *          not greater than `from`
   */
  override def slice(from: Int, until: Int): List[A] = {
    val lo = scala.math.max(from, 0)
    if (until <= lo || isEmpty) Nil
    else this drop lo take (until - lo)
  }

  /** Returns a list of the last `n` elements of this list, or of all its elements if it
   *  has fewer than `n`.
   *
   *  The result is a suffix of this list and is shared with it, so nothing is copied;
   *  finding it costs one traversal. A non-positive `n` gives `Nil`.
   *
   *  @param n the number of elements to take
   */
  override def takeRight(n: Int): List[A] = {
    @tailrec
    def loop(lead: List[A], lag: List[A]): List[A] = lead match {
      case Nil => lag
      case _ :: tail => loop(tail, lag.tail)
    }
    loop(drop(n), this)
  }

  // dropRight is inherited from LinearSeq

  /** Returns a pair of lists, the first holding the first `n` elements of this list and
   *  the second the rest.
   *
   *  The first `n` elements are copied; the second list is a suffix of this list and is
   *  shared with it. A non-positive `n` puts everything in the second list.
   *
   *  @param n the index at which to split this list
   *  @return a pair of the first `n` elements and the remaining ones
   */
  override def splitAt(n: Int): (List[A], List[A]) = {
    val b = new ListBuffer[A]
    var i = 0
    var these = this
    while (!these.isEmpty && i < n) {
      i += 1
      b += these.head
      these = these.tail
    }
    (b.toList, these)
  }

  /** Returns a copy of this list with the element at `index` replaced by `elem`.
   *
   *  The elements before `index` are copied and the ones after it are shared with this
   *  list.
   *
   *  @tparam B the element type of the returned list, a supertype of `A`
   *  @param index the position of the element to replace
   *  @param elem the replacing element
   *  @return a list that agrees with this list everywhere except at `index`, where it
   *          holds `elem`
   *  @throws IndexOutOfBoundsException if `index` is negative or not less than the length of this list
   */
  override def updated[B >: A](index: Int, elem: B): List[B] = {
    var i = 0
    var current = this
    val prefix = ListBuffer.empty[B]
    while (i < index && current.nonEmpty) {
      i += 1
      prefix += current.head
      current = current.tail
    }
    if (i == index && current.nonEmpty) {
      prefix.prependToList(elem :: current.tail)
    } else {
      throw CommonErrors.indexOutOfBounds(index = index, max = length - 1)
    }
  }

  /** Returns a list of the results of applying `f` to each element of this list, in
   *  order.
   *
   *  @tparam B the element type of the returned list
   *  @param f the function to apply to each element
   */
  final override def map[B](f: A => B): List[B] = {
    if (this eq Nil) Nil else {
      val h = new ::[B](f(head), Nil)
      var t: ::[B] = h
      var rest = tail
      while (rest ne Nil) {
        val nx = new ::(f(rest.head), Nil)
        t.next = nx
        t = nx
        rest = rest.tail
      }
      releaseFence()
      h
    }
  }

  /** Returns a list of the results of applying `pf` to the elements of this list on
   *  which it is defined, leaving the others out.
   *
   *  `pf` is applied at most once per element, its domain being tested and its result
   *  taken in one step.
   *
   *  @tparam B the element type of the returned list
   *  @param pf the partial function applied to the elements in its domain
   */
  final override def collect[B](pf: PartialFunction[A, B]^): List[B] = {
    if (this eq Nil) Nil else {
      var rest = this
      var h: ::[B] | Null = null
      var x: Any = null
      // Special case for first element
      while (h eq null) {
        x = pf.applyOrElse(rest.head, List.partialNotApplied)
        if (x.asInstanceOf[AnyRef] ne List.partialNotApplied) h = new ::(x.asInstanceOf[B], Nil)
        rest = rest.tail
        if (rest eq Nil) return if (h eq null) Nil else h
      }
      var t = h
      // Remaining elements
      while (rest ne Nil) {
        x = pf.applyOrElse(rest.head, List.partialNotApplied)
        if (x.asInstanceOf[AnyRef] ne List.partialNotApplied) {
          val nx = new ::(x.asInstanceOf[B], Nil)
          t.next = nx
          t = nx
        }
        rest = rest.tail
      }
      releaseFence()
      h
    }
  }

  /** Returns a list of the concatenated results of applying `f` to each element of this
   *  list, in order.
   *
   *  @tparam B the element type of the returned list
   *  @param f the function mapping each element to a collection of results
   *  @return a list of all elements of the collections returned by `f`, `Nil` if they
   *          are all empty
   */
  final override def flatMap[B](f: A => IterableOnce[B]^): List[B] = {
    var rest = this
    var h: ::[B] | Null = null
    var t: ::[B] | Null = null
    while (rest ne Nil) {
      val it = f(rest.head).iterator
      while (it.hasNext) {
        val nx = new ::(it.next(), Nil)
        if (t eq null) {
          h = nx
        } else {
          t.next = nx
        }
        t = nx
      }
      rest = rest.tail
    }
    if (h eq null) Nil else {releaseFence(); h}
  }

  /** Returns the longest prefix of this list whose elements all satisfy `p`.
   *
   *  `p` is applied to the elements in order and to at most one element that fails it;
   *  the elements kept are copied.
   *
   *  @param p the predicate the elements of the prefix must satisfy
   */
  @inline final override def takeWhile(p: A => Boolean): List[A] = {
    val b = new ListBuffer[A]
    var these = this
    while (!these.isEmpty && p(these.head)) {
      b += these.head
      these = these.tail
    }
    b.toList
  }

  /** Returns a pair of the longest prefix of this list whose elements all satisfy `p`
   *  and the rest of this list.
   *
   *  The prefix is copied; the second list is a suffix of this list and is shared with
   *  it.
   *
   *  @param p the predicate the elements of the prefix must satisfy
   *  @return a pair of the longest prefix satisfying `p` and the remaining elements
   */
  @inline final override def span(p: A => Boolean): (List[A], List[A]) = {
    val b = new ListBuffer[A]
    var these = this
    while (!these.isEmpty && p(these.head)) {
      b += these.head
      these = these.tail
    }
    (b.toList, these)
  }

  // Overridden with an implementation identical to the inherited one (at this time)
  // solely so it can be finalized and thus inlinable.
  /** Applies `f` to each element of this list, in order.
   *
   *  @tparam U the result type of `f`, used only for its side effects
   *  @param f the function to apply to each element
   */
  @inline final override def foreach[U](f: A => U): Unit = {
    var these = this
    while (!these.isEmpty) {
      f(these.head)
      these = these.tail
    }
  }

  /** Returns a list with the elements of this list in reverse order.
   *
   *  Every cons cell is rebuilt, so the result shares nothing with this list.
   */
  final override def reverse: List[A] = {
    var result: List[A] = Nil
    var these = this
    while (!these.isEmpty) {
      result = these.head :: result
      these = these.tail
    }
    result
  }

  /** Applies `op` to the elements of this list and `z`, going right to left.
   *
   *  This list is reversed first and then folded from the left, so the fold uses no
   *  stack in the length of this list but does allocate a reversed copy of it.
   *
   *  @tparam B the result type of the fold
   *  @param z the start value, combined with the last element first
   *  @param op the binary operator, applied to an element and the result accumulated so far
   *  @return the result of inserting `op` between consecutive elements and `z`, or `z`
   *          itself if this list is empty
   */
  final override def foldRight[B](z: B)(op: (A, B) => B): B = {
    var acc = z
    var these: List[A] = reverse
    while (!these.isEmpty) {
      acc = op(these.head, acc)
      these = these.tail
    }
    acc
  }

  // Copy/Paste overrides to avoid interface calls inside loops.

  /** Returns the number of elements in this list, counted by traversing it, which costs
   *  `O(n)`.
   */
  override final def length: Int = {
    var these = this
    var len = 0
    while (!these.isEmpty) {
      len += 1
      these = these.tail
    }
    len
  }

  /** Compares the length of this list with `len`, traversing no further than `len`
   *  elements rather than counting them all.
   *
   *  @param len the length to compare against
   *  @return a negative value if this list is shorter than `len`, `0` if it has exactly
   *          `len` elements, and a positive value if it is longer, which is also the
   *          answer for every negative `len`
   */
  override final def lengthCompare(len: Int): Int = {
    @tailrec def loop(i: Int, xs: List[A]): Int = {
      if (i == len)
        if (xs.isEmpty) 0 else 1
      else if (xs.isEmpty)
        -1
      else
        loop(i + 1, xs.tail)
    }
    if (len < 0) 1
    else loop(0, coll)
  }

  /** Returns `true` if `p` holds for every element of this list.
   *
   *  The traversal stops at the first element that fails `p`.
   *
   *  @param p the predicate used to test elements
   *  @return `true` if this list is empty or `p` holds for all of its elements
   */
  override final def forall(p: A => Boolean): Boolean = {
    var these: List[A] = this
    while (!these.isEmpty) {
      if (!p(these.head)) return false
      these = these.tail
    }
    true
  }

  /** Returns `true` if `p` holds for at least one element of this list.
   *
   *  The traversal stops at the first element that satisfies `p`.
   *
   *  @param p the predicate used to test elements
   *  @return `true` if `p` holds for some element, `false` if this list is empty
   */
  override final def exists(p: A => Boolean): Boolean = {
    var these: List[A] = this
    while (!these.isEmpty) {
      if (p(these.head)) return true
      these = these.tail
    }
    false
  }

  /** Returns `true` if this list holds an element that is `==` to `elem`.
   *
   *  The traversal stops at the first match.
   *
   *  @tparam A1 the type of `elem`, a supertype of `A`
   *  @param elem the element to look for
   */
  override final def contains[A1 >: A](elem: A1): Boolean = {
    var these: List[A] = this
    while (!these.isEmpty) {
      if (these.head == elem) return true
      these = these.tail
    }
    false
  }

  /** Returns the first element satisfying `p` wrapped in a `Some`, or `None` if there is
   *  none.
   *
   *  The traversal stops at the first match.
   *
   *  @param p the predicate used to test elements
   */
  override final def find(p: A => Boolean): Option[A] = {
    var these: List[A] = this
    while (!these.isEmpty) {
      if (p(these.head)) return Some(these.head)
      these = these.tail
    }
    None
  }

  /** Returns the last element of this list, reached by traversing it, which costs
   *  `O(n)`.
   *
   *  @throws NoSuchElementException if this list is empty
   */
  override def last: A = {
    if (isEmpty) throw new NoSuchElementException("List.last")
    else {
      var these = this
      var scout = tail
      while (!scout.isEmpty) {
        these = scout
        scout = scout.tail
      }
      these.head
    }
  }

  /** Returns `true` if this list and `that` have the same length and `p` holds for every
   *  pair of corresponding elements.
   *
   *  When `that` is a linear sequence the two are walked side by side, which avoids
   *  indexing into it; the traversal stops at the first pair that fails `p`.
   *
   *  @tparam B the element type of `that`
   *  @param that the sequence to compare against
   *  @param p the relation each pair of corresponding elements must satisfy
   */
  override def corresponds[B](that: collection.Seq[B])(p: (A, B) => Boolean): Boolean = that match {
    case that: LinearSeq[B @unchecked] =>
      var i = this
      var j = that
      while (!(i.isEmpty || j.isEmpty)) {
        if (!p(i.head, j.head))
          return false
        i = i.tail
        j = j.tail
      }
      i.isEmpty && j.isEmpty
    case _ =>
      super.corresponds(that)(p)
  }

  /** The prefix of this list's string representation: `"List"`. */
  override protected def className = "List"

  /** Builds a new list by applying a function to all elements of this list.
   *  Like `xs map f`, but returns `xs` unchanged if function
   *  `f` maps all elements to themselves (as determined by `eq`).
   *
   *  @tparam B     the element type of the returned collection.
   *  @param f      the function to apply to each element.
   *  @return       a list resulting from applying the given function
   *                `f` to each element of this list and collecting the results.
   */
  @`inline` final def mapConserve[B >: A <: AnyRef](f: A => B): List[B] = {
    // Note to developers: there exists a duplication between this function and `reflect.internal.util.Collections#map2Conserve`.
    // If any successful optimization attempts or other changes are made, please rehash them there too.
    @tailrec
    def loop(mappedHead: List[B] | Null, mappedLast: ::[B] | Null, unchanged: List[A], pending: List[A]): List[B] = {
      if (pending.isEmpty) {
        if (mappedHead eq null) unchanged
        else {
          mappedLast.nn.next = (unchanged: List[B])
          mappedHead
        }
      }
      else {
        val head0 = pending.head
        val head1 = f(head0)

        if (head1 eq head0.asInstanceOf[AnyRef])
          loop(mappedHead, mappedLast, unchanged, pending.tail)
        else {
          var xc = unchanged
          var mappedHead1: List[B] | Null = mappedHead
          var mappedLast1: ::[B] | Null = mappedLast
          while (xc ne pending) {
            val next = new ::[B](xc.head, Nil)
            if (mappedHead1 eq null) mappedHead1 = next
            if (mappedLast1 ne null) mappedLast1.next = next
            mappedLast1 = next
            xc = xc.tail
          }
          val next = new ::(head1, Nil)
          if (mappedHead1 eq null) mappedHead1 = next
          if (mappedLast1 ne null) mappedLast1.next = next
          mappedLast1 = next
          val tail0 = pending.tail
          loop(mappedHead1, mappedLast1, tail0, tail0)

        }
      }
    }
    val result = loop(null, null, this, this)
    releaseFence()
    result
  }

  /** Returns a list of the elements of this list that satisfy `p`, in order.
   *
   *  As much of this list as possible is shared with the result: this list itself is
   *  returned when every element satisfies `p`, and the longest suffix in which nothing
   *  is dropped is shared rather than copied.
   *
   *  @param p the predicate an element must satisfy to be kept
   */
  override def filter(p: A => Boolean): List[A] = filterCommon(p, isFlipped = false)

  /** Returns a list of the elements of this list that do not satisfy `p`, in order.
   *
   *  As much of this list as possible is shared with the result: this list itself is
   *  returned when no element satisfies `p`, and the longest suffix in which nothing is
   *  dropped is shared rather than copied.
   *
   *  @param p the predicate an element must not satisfy to be kept
   */
  override def filterNot(p: A => Boolean): List[A] = filterCommon(p, isFlipped = true)

  private def filterCommon(p: A => Boolean, isFlipped: Boolean): List[A] = {

    // everything seen so far so far is not included
    @tailrec def noneIn(l: List[A]): List[A] = {
      if (l.isEmpty)
        Nil
      else {
        val h = l.head
        val t = l.tail
        if (p(h) != isFlipped)
          allIn(l, t)
        else
          noneIn(t)
      }
    }

    // everything from 'start' is included, if everything from this point is in we can return the origin
    // start otherwise if we discover an element that is out we must create a new partial list.
    @tailrec def allIn(start: List[A], remaining: List[A]): List[A] = {
      if (remaining.isEmpty)
        start
      else {
        val x = remaining.head
        if (p(x) != isFlipped)
          allIn(start, remaining.tail)
        else
          partialFill(start, remaining)
      }
    }

    // we have seen elements that should be included then one that should be excluded, start building
    def partialFill(origStart: List[A], firstMiss: List[A]): List[A] = {
      val newHead = new ::(origStart.head, Nil)
      var toProcess = origStart.tail
      var currentLast = newHead

      // we know that all elements are :: until at least firstMiss.tail
      while (!(toProcess eq firstMiss)) {
        val newElem = new ::(toProcess.head, Nil)
        currentLast.next = newElem
        currentLast = newElem
        toProcess = toProcess.tail
      }

      // at this point newHead points to a list which is a duplicate of all the 'in' elements up to the first miss.
      // currentLast is the last element in that list.

      // now we are going to try and share as much of the tail as we can, only moving elements across when we have to.
      var next = firstMiss.tail
      var nextToCopy = next // the next element we would need to copy to our list if we cant share.
      while (!next.isEmpty) {
        // generally recommended is next.isNonEmpty but this incurs an extra method call.
        val head: A = next.head
        if (p(head) != isFlipped) {
          next = next.tail
        } else {
          // its not a match - do we have outstanding elements?
          while (!(nextToCopy eq next)) {
            val newElem = new ::(nextToCopy.head, Nil)
            currentLast.next = newElem
            currentLast = newElem
            nextToCopy = nextToCopy.tail
          }
          nextToCopy = next.tail
          next = next.tail
        }
      }

      // we have remaining elements - they are unchanged attach them to the end
      if (!nextToCopy.isEmpty)
        currentLast.next = nextToCopy

      newHead
    }

    val result = noneIn(this)
    releaseFence()
    result
  }

  /** Returns a pair of lists, the first holding the elements satisfying `p` and the
   *  second the elements that do not.
   *
   *  When all elements fall on one side, this list itself is used for that side rather
   *  than a copy, and the other side is `Nil`.
   *
   *  @param p the predicate used to test elements
   *  @return a pair of the elements satisfying `p` and the elements not satisfying it
   */
  override def partition(p: A => Boolean): (List[A], List[A]) = {
    if (isEmpty) List.TupleOfNil
    else super.partition(p) match {
      case (Nil, xs) => (Nil, this)
      case (xs, Nil) => (this, Nil)
      case pair => pair
    }
  }

  /** Returns this list itself; being already a `List`, no copy is made. */
  final override def toList: List[A] = this

  // Override for performance
  /** Returns `true` if `o` is a sequence holding the same elements in the same order.
   *
   *  Two lists are compared by walking them side by side, and two lists that are the
   *  same object are equal without any comparison; against any other value the inherited
   *  sequence equality is used.
   *
   *  @param o the value to compare against
   */
  override def equals(o: scala.Any): Boolean = {
    @tailrec def listEq(a: List[?], b: List[?]): Boolean =
      (a eq b) || {
        val aEmpty = a.isEmpty
        val bEmpty = b.isEmpty
        if (!(aEmpty || bEmpty) && a.head == b.head) {
          listEq(a.tail, b.tail)
        }
        else {
          aEmpty && bEmpty
        }
      }

    o match {
      case that: List[?] => listEq(this, that)
      case _ => super.equals(o)
    }
  }

  // TODO: uncomment once bincompat allows (reference: scala/scala#9365)
  /*
  // Override for performance: traverse only as much as needed
  // and share tail when nothing needs to be filtered out anymore
  override def diff[B >: A](that: collection.Seq[B]): AnyRef = {
    if (that.isEmpty || this.isEmpty) this
    else if (tail.isEmpty) if (that.contains(head)) Nil else this
    else {
      val occ = occCounts(that)
      val b = new ListBuffer[A]()
      @tailrec
      def rec(remainder: List[A]): List[A] = {
        if(occ.isEmpty) b.prependToList(remainder)
        else remainder match {
          case Nil => b.result()
          case head :: next => {
            occ.updateWith(head){
              case None => {
                b.append(head)
                None
              }
              case Some(1) => None
              case Some(n) => Some(n - 1)
            }
            rec(next)
          }
        }
      }
      rec(this)
    }
  }
  */

}

// Internal code that mutates `next` _must_ call `Statics.releaseFence()` if either immediately, or
// before a newly-allocated, thread-local :: instance is aliased (e.g. in ListBuffer.toList)
/** A non-empty list, holding its first element and the list of the remaining ones.
 *
 *  @tparam A the element type of this list
 *  @param head the first element of this list
 *  @param next the list of all elements after the first
 */
final case class :: [+A](override val head: A, private[scala] var next: List[A @uncheckedVariance]) // sound because `next` is used only locally
  extends List[A] {
  releaseFence()
  /** Returns the first element of this list wrapped in a `Some`, which is never `None`
   *  for a non-empty list.
   */
  override def headOption: Some[A] = Some(head)
  /** Returns the list of all elements of this list after the first. */
  override def tail: List[A] = next

  @publicInBinary
  private[::] def `next$access$1` = next

}

case object Nil extends List[Nothing] {
  /** Returns nothing; the empty list has no elements.
   *
   *  @throws NoSuchElementException always
   */
  override def head: Nothing = throw new NoSuchElementException("head of empty list")
  /** Returns `None`; the empty list has no first element. */
  override def headOption: None.type = None
  /** Returns nothing; the empty list has no tail.
   *
   *  @throws UnsupportedOperationException always
   */
  override def tail: Nothing = throw new UnsupportedOperationException("tail of empty list")
  /** Returns nothing; the empty list has no last element.
   *
   *  @throws NoSuchElementException always
   */
  override def last: Nothing = throw new NoSuchElementException("last of empty list")
  /** Returns nothing; the empty list has no elements to drop the last of.
   *
   *  @throws UnsupportedOperationException always
   */
  override def init: Nothing = throw new UnsupportedOperationException("init of empty list")
  /** Returns `0`; the size of the empty list is known without traversal. */
  override def knownSize: Int = 0
  /** Returns the empty iterator. */
  override def iterator: Iterator[Nothing] = Iterator.empty
  /** Returns a pair of empty lists, the same shared pair on every call.
   *
   *  @tparam A1 the element type of the first returned list
   *  @tparam A2 the element type of the second returned list
   *  @param asPair evidence that the elements are pairs; never used, since there are no
   *                elements to split
   */
  override def unzip[A1, A2](implicit asPair: Nothing -> (A1, A2)): (List[A1], List[A2]) = EmptyUnzip

  @transient
  private val EmptyUnzip = (Nil, Nil)
}

/** $factoryInfo
 *  @define coll list
 *  @define Coll `List`
 */
@SerialVersionUID(3L)
object List extends StrictOptimizedSeqFactory[List] {
  private val TupleOfNil = (Nil, Nil)

  /** Returns a list containing the elements of `coll`, in the order `coll` gives them
   *  out.
   *
   *  If `coll` is already a list it is returned unchanged.
   *
   *  @tparam B the element type
   *  @param coll the collection whose elements are to be contained
   */
  def from[B](coll: collection.IterableOnce[B]^): List[B] = Nil.prependedAll(coll)

  /** Returns a new builder that collects elements into a list, in the order they are
   *  added, using a `ListBuffer` to append in constant time.
   *
   *  @tparam A the element type of the list being built
   */
  def newBuilder[A]: Builder[A, List[A]] = new ListBuffer()

  /** Returns the empty list, [[Nil]].
   *
   *  @tparam A the element type of the list
   */
  def empty[A]: List[A] = Nil

  @transient
  private[collection] val partialNotApplied = new Function1[Any, Any] { def apply(x: Any): Any = this }
}
