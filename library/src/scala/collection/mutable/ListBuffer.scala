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
package mutable

import scala.language.`2.13`
import language.experimental.captureChecking
import scala.annotation.{nowarn, tailrec}
import scala.collection.generic.CommonErrors
import scala.collection.immutable.{::, List, Nil}
import java.lang.{IllegalArgumentException, IndexOutOfBoundsException}

import scala.collection.generic.DefaultSerializable
import scala.runtime.Statics.releaseFence

/** A `Buffer` implementation backed by a list. It provides constant time
 *  prepend and append. Most other operations are linear.
 *
 *  @see ["Scala's Collection Library overview"](https://docs.scala-lang.org/overviews/collections-2.13/concrete-mutable-collection-classes.html#list-buffers)
 *  section on `List Buffers` for more information.
 *
 *  @tparam A    the type of this list buffer's elements.
 *
 *  @define Coll `ListBuffer`
 *  @define coll list buffer
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
@SerialVersionUID(-8428291952499836345L)
class ListBuffer[A]
  extends AbstractBuffer[A]
     with SeqOps[A, ListBuffer, ListBuffer[A]]
     with StrictOptimizedSeqOps[A, ListBuffer, ListBuffer[A]]
     with ReusableBuilder[A, immutable.List[A]]
     with IterableFactoryDefaults[A, ListBuffer]
     with DefaultSerializable {
  @transient private var mutationCount: Int = 0

  private var first: List[A] = Nil
  @annotation.stableNull
  private var last0: ::[A] | Null = null // last element (`last0` just because the name `last` is already taken)
  private var aliased = false
  private var len = 0

  private type Predecessor = ::[A] | Null

  /** Returns an iterator over the elements of this buffer.
   *
   *  The iterator is fail-fast: if the buffer is mutated after the iterator's
   *  creation, `hasNext` throws a [[java.util.ConcurrentModificationException]].
   */
  def iterator: Iterator[A] = new MutationTracker.CheckedIterator(first.iterator, mutationCount)

  /** The companion object `ListBuffer`, used by transformation methods to build new list buffers. */
  override def iterableFactory: SeqFactory[ListBuffer] = ListBuffer

  /** Returns the element of this buffer at index `i`.
   *
   *  Takes time linear in `i`.
   *
   *  @param i the index of the element to return
   *  @throws IndexOutOfBoundsException if `i < 0` or `i >= length`
   */
  @throws[IndexOutOfBoundsException]
  def apply(i: Int) = first.apply(i)

  /** Returns the number of elements in this buffer, in constant time. */
  def length = len
  /** Returns the number of elements in this buffer. Never `-1`, as the size is always known. */
  override def knownSize = len

  /** Returns `true` if this buffer contains no elements. */
  override def isEmpty: Boolean = len == 0

  private def copyElems(): Unit = {
    val buf = new ListBuffer[A].freshFrom(this)
    first = buf.first
    last0 = buf.last0
    aliased = false
  }

  // we only call this before mutating things, so it's
  // a good place to track mutations for the iterator
  private def ensureUnaliased(): Unit = {
    mutationCount += 1
    if (aliased) copyElems()
  }

  // Avoids copying where possible.
  /** Returns the contents of this buffer as an immutable [[scala.collection.immutable.List]].
   *
   *  Takes constant time: the buffer's internal list is returned directly rather
   *  than copied. The buffer is instead marked as aliased, and any later operation
   *  that would modify the shared cells copies them first, so the returned list is
   *  never affected by later changes to the buffer. Operations that abandon those
   *  cells rather than modify them, such as `clear`, `mapInPlace` and
   *  `flatMapInPlace`, need no copy. The elements are safely published,
   *  so the returned list may be shared with other threads.
   */
  override def toList: List[A] = {
    aliased = nonEmpty
    // We've accumulated a number of mutations to `List.tail` by this stage.
    // Make sure they are visible to threads that the client of this ListBuffer might be about
    // to share this List with.
    releaseFence()
    first
  }

  /** Returns the contents of this buffer as an immutable [[scala.collection.immutable.List]], like `toList`.
   *
   *  The buffer remains valid afterwards: it can be mutated further, with the shared
   *  cells copied first where an operation would modify them, leaving the returned
   *  list unchanged, or reused after `clear()`.
   */
  def result(): immutable.List[A] = toList

  /** Prepends the elements of this buffer to a given list
   *
   *  @param xs   the list to which elements are prepended
   *  @return a list consisting of the elements of this buffer followed by `xs`, or `xs` itself if this buffer is empty
   */
  def prependToList(xs: List[A]): List[A] = {
    if (isEmpty) xs
    else {
      ensureUnaliased()
      last0.nn.next = xs
      toList
    }
  }

  /** Removes all elements from this buffer, in constant time.
   *
   *  Lists previously returned by `toList` or `result()` are not affected.
   */
  def clear(): Unit = {
    mutationCount += 1
    first = Nil
    len = 0
    last0 = null
    aliased = false
  }

  /** Appends `elem` to this buffer.
   *
   *  Takes constant time, unless the buffer's list is aliased by an earlier
   *  `toList` or `result()` call, in which case the elements are first copied.
   *
   *  @param elem the element to append
   *  @return this $coll
   */
  final def addOne(elem: A): this.type = {
    ensureUnaliased()
    val last1 = new ::[A](elem, Nil)
    if (len == 0) first = last1 else last0.nn.next = last1
    last0 = last1
    len += 1
    this
  }

  // MUST only be called on fresh instances
  private def freshFrom(xs: IterableOnce[A]^): this.type = {
    val it = xs.iterator
    if (it.hasNext) {
      var len = 1
      var last0 = new ::[A](it.next(), Nil)
      first = last0
      while (it.hasNext) {
        val last1 = new ::[A](it.next(), Nil)
        last0.next = last1
        last0 = last1
        len += 1
      }
      // copy local vars into instance
      this.len = len
      this.last0 = last0
    }
    this
  }

  /** Appends all elements of `xs` to this buffer.
   *
   *  The elements are first copied into a fresh list, so `xs` may be this
   *  buffer itself or a list it previously returned.
   *
   *  @param xs the collection containing the elements to append
   *  @return this $coll
   */
  override final def addAll(xs: IterableOnce[A]^): this.type = {
    val it = xs.iterator
    if (it.hasNext) {
      val fresh = new ListBuffer[A].freshFrom(it)
      ensureUnaliased()
      if (len == 0) first = fresh.first
      else last0.nn.next = fresh.first
      last0 = fresh.last0
      len += fresh.length
    }
    this
  }

  /** Removes the first occurrence of `elem` from this buffer, if any.
   *
   *  Takes time linear in the buffer size. If this buffer does not contain
   *  `elem`, it is unchanged.
   *
   *  @param elem the element to remove
   *  @return this $coll
   */
  override def subtractOne(elem: A): this.type = {
    ensureUnaliased()
    if (isEmpty) {}
    else if (first.head == elem) {
      first = first.tail
      reduceLengthBy(1)
    }
    else {
      var cursor = first
      while (!cursor.tail.isEmpty && cursor.tail.head != elem) {
        cursor = cursor.tail
      }
      if (!cursor.tail.isEmpty) {
        val z = cursor.asInstanceOf[::[A]]
        if (z.next == last0)
          last0 = z
        z.next = cursor.tail.tail
        reduceLengthBy(1)
      }
    }
    this
  }

  /** Reduces the length of the buffer, and nulls out last0
   *  if this reduces the length to 0.
   *
   *  @param num the number of elements by which to reduce the length
   */
  private def reduceLengthBy(num: Int): Unit = {
    len -= num
    if (len <= 0)   // obviously shouldn't be < 0, but still better not to leak
      last0 = null
  }

  // returns the `::` at `i - 1` (such that its `next` at position `i` can be mutated), or `null` if `i == 0`.
  private def predecessor(i: Int): Predecessor =
    if (i == 0) null
    else if (i == len) last0
    else {
      var j = i - 1
      var p = first
      while (j > 0) {
        p = p.tail
        j -= 1
      }
      p.asInstanceOf[Predecessor]
    }

  private def getNext(p: Predecessor): List[A] =
    if (p == null) first else p.next

  /** Replaces the element at index `idx` with `elem`.
   *
   *  Takes time linear in `idx`, or in the buffer size if a previous `toList` or
   *  `result` left it aliased, since the shared cells are copied first.
   *
   *  @param idx the index of the element to replace
   *  @param elem the new element
   *  @throws IndexOutOfBoundsException if `idx < 0` or `idx >= length`
   */
  def update(idx: Int, elem: A): Unit = {
    ensureUnaliased()
    if (idx < 0 || idx >= len) throw CommonErrors.indexOutOfBounds(index = idx, max = len - 1)
    if (idx == 0) {
      val newElem = new :: (elem, first.tail)
      if (last0 eq first) {
        last0 = newElem
      }
      first = newElem
    } else {
      // `p` can not be `null` because the case where `idx == 0` is handled above
      val p = predecessor(idx).nn
      val newElem = new :: (elem, p.tail.tail)
      if (last0 eq p.tail) {
        last0 = newElem
      }
      p.asInstanceOf[::[A]].next = newElem
    }
  }

  /** Inserts `elem` at index `idx` into this buffer.
   *
   *  The elements at indices `idx` and above have their indices increased by
   *  one. Takes time linear in `idx`, or constant time when prepending
   *  (`idx == 0`) or appending (`idx == length`), except that a buffer left
   *  aliased by an earlier `toList` or `result` is copied first, which is
   *  linear in its size whatever `idx` is.
   *
   *  @param idx the index where the element is inserted
   *  @param elem the element to insert
   *  @throws IndexOutOfBoundsException if the index `idx` is not in the valid range
   *          `0 <= idx <= length`
   */
  def insert(idx: Int, elem: A): Unit = {
    ensureUnaliased()
    if (idx < 0 || idx > len) throw CommonErrors.indexOutOfBounds(index = idx, max = len - 1)
    if (idx == len) addOne(elem)
    else {
      val p = predecessor(idx)
      val nx = elem :: getNext(p)
      if(p eq null) first = nx else p.next = nx
      len += 1
    }
  }

  /** Prepends `elem` to this buffer, in constant time, or in time linear in its
   *  size if an earlier `toList` or `result` left it aliased.
   *
   *  @param elem the element to prepend
   *  @return this $coll
   */
  def prepend(elem: A): this.type = {
    insert(0, elem)
    this
  }

  // `fresh` must be a `ListBuffer` that only we have access to
  private def insertAfter(prev: Predecessor, fresh: ListBuffer[A]): Unit = {
    if (!fresh.isEmpty) {
      val follow = getNext(prev)
      if (prev eq null) first = fresh.first else prev.next = fresh.first
      fresh.last0.nn.next = follow
      if (follow.isEmpty) last0 = fresh.last0
      len += fresh.length
    }
  }

  /** Inserts all elements of `elems` at index `idx` into this buffer.
   *
   *  The elements at indices `idx` and above have their indices increased by
   *  the number of inserted elements. If `idx == length`, the elements are
   *  appended. The bounds are checked before `elems` is iterated, and the new
   *  elements are copied into a fresh list before this buffer is modified, so
   *  `elems` may be this buffer itself.
   *
   *  @param idx the index where the elements are inserted
   *  @param elems the collection containing the elements to insert
   *  @throws IndexOutOfBoundsException if the index `idx` is not in the valid range
   *          `0 <= idx <= length`
   */
  def insertAll(idx: Int, elems: IterableOnce[A]^): Unit = {
    if (idx < 0 || idx > len) throw CommonErrors.indexOutOfBounds(index = idx, max = len - 1)
    val it = elems.iterator
    if (it.hasNext) {
      if (idx == len) addAll(it)
      else {
        val fresh = new ListBuffer[A].freshFrom(it)
        ensureUnaliased()
        insertAfter(predecessor(idx), fresh)
      }
    }
  }

  /** Removes and returns the element at index `idx`.
   *
   *  The elements at higher indices have their indices decreased by one. Takes
   *  time linear in `idx`, or in the buffer size if an earlier `toList` or
   *  `result` left it aliased.
   *
   *  @param idx the index of the element to remove
   *  @return the removed element
   *  @throws IndexOutOfBoundsException if `idx < 0` or `idx >= length`
   */
  def remove(idx: Int): A = {
    ensureUnaliased()
    if (idx < 0 || idx >= len) throw CommonErrors.indexOutOfBounds(index = idx, max = len - 1)
    val p = predecessor(idx)
    val nx = getNext(p)
    if(p eq null) {
      first = nx.tail
      if(first.isEmpty) last0 = null
    } else {
      if(last0 eq nx) last0 = p
      p.next = nx.tail
    }
    len -= 1
    nx.head
  }

  /** Removes `count` elements starting at index `idx`.
   *
   *  Takes time linear in `idx + count`, or in the buffer size if `count` is
   *  positive and an earlier `toList` or `result` left it aliased. If `count`
   *  is zero, this buffer is unchanged and the bounds are not checked.
   *
   *  @param idx the index of the first element to remove
   *  @param count the number of elements to remove
   *  @throws IndexOutOfBoundsException if `count > 0` and the index `idx` is not in
   *          the valid range `0 <= idx <= length - count`
   *  @throws IllegalArgumentException if `count < 0`
   */
  def remove(idx: Int, count: Int): Unit =
    if (count > 0) {
      ensureUnaliased()
      if (idx < 0 || idx + count > len) throw new IndexOutOfBoundsException(s"$idx to ${idx + count} is out of bounds (min 0, max ${len - 1})")
      removeAfter(predecessor(idx), count)
    } else if (count < 0) {
      throw new IllegalArgumentException("removing negative number of elements: " + count)
    }

  private def removeAfter(prev: Predecessor, n: Int) = {
    @tailrec def ahead(p: List[A], n: Int): List[A] =
      if (n == 0) p else ahead(p.tail, n - 1)
    val nx = ahead(getNext(prev), n)
    if(prev eq null) first = nx else prev.next = nx
    if(nx.isEmpty) last0 = prev
    len -= n
  }

  /** Replaces the contents of this $coll with the mapped result.
   *
   *  @param f the mapping function
   *  @return this $coll
   */
  def mapInPlace(f: A => A): this.type = {
    mutationCount += 1
    val buf = new ListBuffer[A]
    for (elem <- this) buf += f(elem)
    first = buf.first
    last0 = buf.last0
    aliased = false // we just assigned from a new instance
    this
  }

  /** Replaces the contents of this $coll with the flatmapped result.
   *
   *  @param f the mapping function
   *  @return this $coll
   */
  def flatMapInPlace(f: A => IterableOnce[A]^): this.type = {
    mutationCount += 1
    var src = first
    var dst: List[A] | Null = null
    last0 = null
    len = 0
    while(!src.isEmpty) {
      val it = f(src.head).iterator
      while(it.hasNext) {
        val v = new ::(it.next(), Nil)
        if(dst eq null) dst = v else last0.nn.next = v
        last0 = v
        len += 1
      }
      src = src.tail
    }
    first = if(dst eq null) Nil else dst
    aliased = false // we just rebuilt a fresh, unaliased instance
    this
  }

  /** Replaces the contents of this $coll with the filtered result.
   *
   *  @param p the filtering predicate
   *  @return this $coll
   */
  def filterInPlace(p: A => Boolean): this.type = {
    ensureUnaliased()
    var prev: Predecessor = null
    var cur: List[A] = first
    while (!cur.isEmpty) {
      val follow = cur.tail
      if (!p(cur.head)) {
        if(prev eq null) first = follow
        else prev.next = follow
        len -= 1
      } else {
        prev = cur.asInstanceOf[Predecessor]
      }
      cur = follow
    }
    last0 = prev
    this
  }

  /** Replaces a slice of elements in this $coll by another sequence of elements.
   *
   *  `from` and `replaced` are clamped: patching at negative indices is the
   *  same as patching starting at 0, patching at indices at or larger than the
   *  length appends the patch to the end, and an excessive `replaced` count is
   *  reduced to the available elements. Implemented by removing the replaced
   *  elements and inserting the patch in their place; never fails on
   *  out-of-range arguments.
   *
   *  @param from the index of the first replaced element
   *  @param patch the replacement sequence
   *  @param replaced the number of elements to drop in the original $coll
   *  @return this $coll
   */
  def patchInPlace(from: Int, patch: collection.IterableOnce[A]^, replaced: Int): this.type = {
    val _len = len
    val _from = math.max(from, 0)         // normalized
    val _replaced = math.max(replaced, 0) // normalized
    val it = patch.iterator

    val nonEmptyPatch = it.hasNext
    val nonEmptyReplace = (_from < _len) && (_replaced > 0)

    // don't want to add a mutation or check aliasing (potentially expensive)
    // if there's no patching to do
    if (nonEmptyPatch || nonEmptyReplace) {
      val fresh = new ListBuffer[A].freshFrom(it)
      ensureUnaliased()
      val i = math.min(_from, _len)
      val n = math.min(_replaced, _len)
      val p = predecessor(i)
      removeAfter(p, math.min(n, _len - i))
      insertAfter(p, fresh)
    }
    this
  }

  /** Selects the last element.
   *
   *  Runs in constant time.
   *
   *  @return the last element of this $coll.
   *  @throws NoSuchElementException If the $coll is empty.
   */
  override def last: A = if (last0 eq null) throw new NoSuchElementException("last of empty ListBuffer") else last0.head

  /** Optionally selects the last element.
   *
   *  Runs in constant time.
   *
   *  @return the last element of this $coll if it is nonempty, `None` if it is empty.
   */
  override def lastOption: Option[A] = if (last0 eq null) None else Some(last0.head)

  /** The prefix of this $coll's `toString` representation, `"ListBuffer"`. */
  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected def stringPrefix = "ListBuffer"

}

@SerialVersionUID(3L)
object ListBuffer extends StrictOptimizedSeqFactory[ListBuffer] {

  /** Creates a list buffer containing the elements of `coll`.
   *
   *  @tparam A the element type
   *  @param coll the collection providing the elements
   *  @return a new `ListBuffer` with the elements of `coll` in order
   */
  def from[A](coll: collection.IterableOnce[A]^): ListBuffer[A] = new ListBuffer[A].freshFrom(coll)

  /** Returns a new builder that produces a `ListBuffer`.
   *
   *  A `ListBuffer` is itself a builder, but one producing a `List`; the
   *  builder returned here appends to a list buffer and returns the buffer
   *  itself as its result.
   *
   *  @tparam A the element type
   *  @return a builder producing a `ListBuffer`
   */
  def newBuilder[A]: Builder[A, ListBuffer[A]] = new GrowableBuilder(empty[A])

  /** Creates a new, empty list buffer.
   *
   *  @tparam A the element type
   *  @return a new empty `ListBuffer`
   */
  def empty[A]: ListBuffer[A] = new ListBuffer[A]
}
