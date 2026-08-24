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
import scala.annotation.tailrec
import scala.collection.generic.{CommonErrors, DefaultSerializable}
import scala.reflect.ClassTag
import scala.collection.immutable.Nil

/** A buffer that stores elements in an unrolled linked list.
 *
 *  Unrolled linked lists store elements in linked fixed size
 *  arrays.
 *
 *  Unrolled buffers retain locality and low memory overhead
 *  properties of array buffers, but offer much more efficient
 *  element addition, since they never reallocate and copy the
 *  internal array.
 *
 *  However, they provide `O(n/m)` complexity random access,
 *  where `n` is the number of elements, and `m` the size of
 *  internal array chunks.
 *
 *  Ideal to use when:
 *  - elements are added to the buffer and then all of the
 *    elements are traversed sequentially
 *  - two unrolled buffers need to be concatenated (see `concat`)
 *
 *  Better than singly linked lists for random access, but
 *  should still be avoided for such a purpose.
 *
 *  @define coll unrolled buffer
 *  @define Coll `UnrolledBuffer`
 */
@SerialVersionUID(3L)
sealed class UnrolledBuffer[T](implicit val tag: ClassTag[T])
  extends AbstractBuffer[T]
    with Buffer[T]
    with Seq[T]
    with SeqOps[T, UnrolledBuffer, UnrolledBuffer[T]]
    with StrictOptimizedSeqOps[T, UnrolledBuffer, UnrolledBuffer[T]]
    with EvidenceIterableFactoryDefaults[T, UnrolledBuffer, ClassTag]
    with Builder[T, UnrolledBuffer[T]]
    with DefaultSerializable {

  import UnrolledBuffer.Unrolled

  @transient private var headptr = newUnrolled
  @transient private var lastptr = headptr
  @transient private var sz = 0

  private[collection] def headPtr = headptr
  private[collection] def headPtr_=(head: Unrolled[T]) = headptr = head
  private[collection] def lastPtr = lastptr
  private[collection] def lastPtr_=(last: Unrolled[T]) = lastptr = last
  private[collection] def size_=(s: Int) = sz = s

  /** The factory used by the default `fromSpecific`, `newSpecificBuilder` and
   *  `empty` implementations: the companion object `UnrolledBuffer`.
   */
  protected def evidenceIterableFactory: UnrolledBuffer.type = UnrolledBuffer
  /** The `ClassTag` of the element type, passed as evidence to `evidenceIterableFactory`. */
  protected def iterableEvidence: ClassTag[T] = tag

  /** The factory used by generic transformation methods such as `map`: the
   *  `untagged` companion factory, which uses `ClassTag.Any` since no
   *  `ClassTag` for the new element type is available, so elements of the
   *  resulting buffers are stored boxed.
   */
  override def iterableFactory: SeqFactory[UnrolledBuffer] = UnrolledBuffer.untagged

  /** Creates a new, empty node associated with this buffer, holding an array of 32 elements. */
  protected def newUnrolled = new Unrolled[T](this)

  // The below would allow more flexible behavior without requiring inheritance
  // that is risky because all the important internals are private.
  // private var myLengthPolicy: Int => Int = x => x
  //
  // /** Specifies how the array lengths should vary.
  //   *
  //   *  By default,  `UnrolledBuffer` uses arrays of a fixed size.  A length
  //   *  policy can be given that changes this scheme to, for instance, an
  //   *  exponential growth.
  //   *
  //   *  @param nextLength   computes the length of the next array from the length of the latest one
  //   */
  // def setLengthPolicy(nextLength: Int => Int): Unit = { myLengthPolicy = nextLength }
  private[collection] def calcNextLength(sz: Int) = sz // myLengthPolicy(sz)

  /** The companion object `UnrolledBuffer`, which requires a `ClassTag` for
   *  the element type to create a buffer.
   */
  def classTagCompanion: UnrolledBuffer.type = UnrolledBuffer

  /** Concatenates the target unrolled buffer to this unrolled buffer.
   *
   *  The specified buffer `that` is cleared after this operation. This is
   *  an O(1) operation.
   *
   *  @param that    the unrolled buffer whose elements are added to this buffer
   */
  def concat(that: UnrolledBuffer[T]) = {
    // bind the two together
    if (!lastptr.bind(that.headptr)) lastptr = that.lastPtr

    // update size
    sz += that.sz

    // `that` is no longer usable, so clear it
    // here we rely on the fact that `clear` allocates
    // new nodes instead of modifying the previous ones
    that.clear()

    // return a reference to this
    this
  }

  /** Appends `elem` to this buffer, in constant time.
   *
   *  When the last node's array is full, a new node is allocated; existing
   *  arrays are never reallocated or copied.
   *
   *  @param elem the element to append
   *  @return this $coll
   */
  def addOne(elem: T) = {
    lastptr = lastptr.append(elem)
    sz += 1
    this
  }

  /** Removes all elements from this buffer.
   *
   *  A fresh, empty node is allocated; existing nodes are left untouched.
   *  `concat` relies on this when it clears the buffer whose nodes it has
   *  taken over.
   */
  def clear(): Unit = {
    headptr = newUnrolled
    lastptr = headptr
    sz = 0
  }

  /** Returns an iterator over the elements of this buffer, walking the chain
   *  of internal arrays.
   *
   *  The iterator does not check for concurrent mutation of the buffer.
   */
  def iterator: Iterator[T] = new AbstractIterator[T] {
    var pos: Int = -1
    var node: Unrolled[T] | Null = headptr
    scan()

    private def scan(): Unit = {
      pos += 1
      while (pos >= node.nn.size) {
        pos = 0
        node = node.nn.next
        if (node eq null) return
      }
    }
    def hasNext = node ne null
    def next() = if (hasNext) {
      val r = node.nn.array(pos)
      scan()
      r
    } else Iterator.empty.next()
  }

  // this should be faster than the iterator
  /** Applies `f` to each element of this buffer.
   *
   *  Overridden to traverse the internal arrays directly, which is faster than
   *  going through `iterator`.
   *
   *  @tparam U the result type of `f`, which is discarded
   *  @param f the function applied to each element for its side effect
   */
  override def foreach[U](f: T => U) = headptr.foreach(f)

  /** Returns this buffer itself: an `UnrolledBuffer` is its own [[Builder]]. */
  def result() = this

  /** Returns the number of elements in this buffer, in constant time. */
  def length = sz

  /** Returns the number of elements in this buffer. Never `-1`, as the size is always known. */
  override def knownSize: Int = sz

  /** Returns the element at index `idx`.
   *
   *  Takes `O(n/m)` time, where `n` is the buffer length and `m` the size of
   *  the internal arrays: the node chain is walked to locate the element.
   *
   *  @param idx the index of the element to return
   *  @throws IndexOutOfBoundsException if `idx < 0` or `idx >= length`
   */
  def apply(idx: Int) =
    if (idx >= 0 && idx < sz) headptr(idx)
    else throw CommonErrors.indexOutOfBounds(index = idx, max = sz - 1)

  /** Replaces the element at index `idx` with `newelem`.
   *
   *  Takes `O(n/m)` time, like `apply`.
   *
   *  @param idx the index of the element to replace
   *  @param newelem the new element
   *  @throws IndexOutOfBoundsException if `idx < 0` or `idx >= length`
   */
  def update(idx: Int, newelem: T) =
    if (idx >= 0 && idx < sz) headptr(idx) = newelem
    else throw CommonErrors.indexOutOfBounds(index = idx, max = sz - 1)

  /** Replaces the contents of this $coll with the mapped result.
   *
   *  @param f the mapping function
   *  @return this $coll
   */
  def mapInPlace(f: T => T): this.type = {
    headptr.mapInPlace(f)
    this
  }

  /** Removes and returns the element at index `idx`.
   *
   *  The elements after it within its node are shifted left. If the node's
   *  remaining elements together with those of the next node then fall below
   *  the waterline (see [[UnrolledBuffer.waterline]]), the two nodes are
   *  merged.
   *
   *  @param idx the index of the element to remove
   *  @return the removed element
   *  @throws IndexOutOfBoundsException if `idx < 0` or `idx >= length`
   */
  def remove(idx: Int) =
    if (idx >= 0 && idx < sz) {
      sz -= 1
      headptr.remove(idx, this)
    } else throw CommonErrors.indexOutOfBounds(index = idx, max = sz - 1)

  /** Removes `count` elements starting at index `idx`, by removing the element
   *  at `idx` repeatedly, `count` times.
   *
   *  If `count <= 0`, this buffer is unchanged and the bounds are not checked;
   *  in particular, a negative `count` does not throw an exception. If fewer
   *  than `count` elements exist at or after `idx`, those up to the end of the
   *  buffer are removed before the bounds check of the next removal fails.
   *
   *  @param idx the index of the first element to remove
   *  @param count the number of elements to remove
   *  @throws IndexOutOfBoundsException if `count > 0` and the index `idx` is not in
   *          the valid range `0 <= idx <= length - count`; the elements before the
   *          failing index remain removed
   */
  @tailrec final def remove(idx: Int, count: Int): Unit =
    if (count > 0) {
      remove(idx)
      remove(idx, count-1)
    }

  /** Prepends `elem` to this buffer.
   *
   *  The existing elements of the head node are shifted right; if the head
   *  node's array is full, a new head node is allocated instead.
   *
   *  @param elem the element to prepend
   *  @return this $coll
   */
  def prepend(elem: T) = {
    headptr = headptr prepend elem
    sz += 1
    this
  }

  /** Inserts `elem` at index `idx` into this buffer, like `insertAll` with a
   *  single element.
   *
   *  @param idx the index where the element is inserted; if `idx == length`,
   *             the element is appended
   *  @param elem the element to insert
   *  @throws IndexOutOfBoundsException if the index `idx` is not in the valid range
   *          `0 <= idx <= length`
   */
  def insert(idx: Int, elem: T): Unit =
    insertAll(idx, elem :: Nil)

  /** Inserts all elements of `elems` at index `idx` into this buffer.
   *
   *  The node containing index `idx` is split at that position, the new elements
   *  are appended to its front half, and the rear half is linked back after them.
   *  Inserting at the end of a node appends to it instead, with no split.
   *
   *  @param idx the index where the elements are inserted; if `idx == length`,
   *             the elements are appended
   *  @param elems the collection containing the elements to insert
   *  @throws IndexOutOfBoundsException if the index `idx` is not in the valid range
   *          `0 <= idx <= length`
   */
  def insertAll(idx: Int, elems: IterableOnce[T]^): Unit =
    if (idx >= 0 && idx <= sz) {
      sz += headptr.insertAll(idx, elems, this)
    } else throw CommonErrors.indexOutOfBounds(index = idx, max = sz - 1)

  /** Removes the first occurrence of `elem` from this buffer, if any.
   *
   *  If this buffer does not contain `elem`, it is unchanged.
   *
   *  @param elem the element to remove
   *  @return this $coll
   */
  override def subtractOne(elem: T): this.type = {
    if (headptr.subtractOne(elem, this)) {
      sz -= 1
    }
    this
  }

  /** Replaces `replaced` elements starting at index `from` with the elements
   *  of `patch`, as `remove(from, replaced)` followed by
   *  `insertAll(from, patch)`.
   *
   *  Unlike the general `Buffer.patchInPlace` contract, out-of-range arguments
   *  are not clamped: an out-of-range `from` or an excessive `replaced` count
   *  leads to an exception from the underlying `remove` or `insertAll`, which
   *  may leave the buffer partially modified.
   *
   *  @param from the index of the first replaced element
   *  @param patch the replacement sequence
   *  @param replaced the number of elements to drop in the original $coll
   *  @return this $coll
   *  @throws IndexOutOfBoundsException if `replaced > 0` and `from` is not in the
   *          valid range `0 <= from <= length - replaced`, or if `replaced <= 0`
   *          and `from` is not in the valid range `0 <= from <= length`
   */
  def patchInPlace(from: Int, patch: collection.IterableOnce[T]^, replaced: Int): this.type = {
    remove(from, replaced)
    insertAll(from, patch)
    this
  }

  private def writeObject(out: java.io.ObjectOutputStream): Unit = {
    out.defaultWriteObject
    out.writeInt(sz)
    for (elem <- this) out.writeObject(elem)
  }

  private def readObject(in: java.io.ObjectInputStream): Unit = {
    in.defaultReadObject

    val num = in.readInt

    headPtr = newUnrolled
    lastPtr = headPtr
    sz = 0
    var i = 0
    while (i < num) {
      this += in.readObject.asInstanceOf[T]
      i += 1
    }
  }

  /** Returns a copy of this buffer: a new `UnrolledBuffer` with fresh internal
   *  nodes holding the same elements. The elements themselves are not copied.
   */
  override def clone(): UnrolledBuffer[T] = new UnrolledBuffer[T] ++= this

  /** The prefix of this $coll's `toString` representation, `"UnrolledBuffer"`. */
  override protected def className = "UnrolledBuffer"
}


@SerialVersionUID(3L)
object UnrolledBuffer extends StrictOptimizedClassTagSeqFactory[UnrolledBuffer] { self =>

  /** A `SeqFactory` view of this companion that creates unrolled buffers using
   *  `ClassTag.Any` as the element evidence, so that no `ClassTag` is required.
   */
  val untagged: SeqFactory[UnrolledBuffer] = new ClassTagSeqFactory.AnySeqDelegate(self)

  /** Creates a new, empty unrolled buffer.
   *
   *  @tparam A the element type; its `ClassTag` determines the type of the internal arrays
   *  @return a new empty `UnrolledBuffer`
   */
  def empty[A : ClassTag]: UnrolledBuffer[A] = new UnrolledBuffer[A]

  /** Creates an unrolled buffer containing the elements of `source`.
   *
   *  @tparam A the element type; its `ClassTag` determines the type of the internal arrays
   *  @param source the collection providing the elements
   *  @return a new `UnrolledBuffer` with the elements of `source` in order
   */
  def from[A : ClassTag](source: scala.collection.IterableOnce[A]^): UnrolledBuffer[A] = newBuilder[A].addAll(source)

  /** Creates a new, empty unrolled buffer, which serves as its own builder.
   *
   *  @tparam A the element type; its `ClassTag` determines the type of the internal arrays
   *  @return a new empty `UnrolledBuffer`
   */
  def newBuilder[A : ClassTag]: UnrolledBuffer[A] = new UnrolledBuffer[A]

  /** The numerator, 50, of the waterline fraction
   *  `waterline / waterlineDenom` (50/100): after a removal, a node is merged
   *  with its successor when their combined element count falls below this
   *  fraction of the node's array length.
   */
  final val waterline: Int = 50

  /** The denominator, 100, of the waterline fraction; see [[waterline]]. */
  final def waterlineDenom: Int = 100

  /** An alias for `waterlineDenom`. */
  @deprecated("Use waterlineDenom instead.", "2.13.0")
  final val waterlineDelim: Int = waterlineDenom

  private[collection] val unrolledlength = 32

  /** Unrolled buffer node.
   *
   *  @tparam T the element type stored in the node's array; requires an implicit `ClassTag` for array creation
   */
  class Unrolled[T: ClassTag] private[collection] (var size: Int, var array: Array[T], var next: Unrolled[T] | Null, val buff: UnrolledBuffer[T] | Null = null) {
    this: Unrolled[T]^{} =>
    private[collection] def this() = this(0, new Array[T](unrolledlength), null, null)
    private[collection] def this(b: UnrolledBuffer[T] | Null) = this(0, new Array[T](unrolledlength), null, b)

    private def nextlength = if (buff eq null) unrolledlength else buff.calcNextLength(array.length)

    // adds and returns itself or the new unrolled if full
    /** Appends `elem` to this node, or to a freshly allocated successor node if
     *  this node's array is full.
     *
     *  @param elem the element to append
     *  @return the node the element was stored in: this node, or the new successor
     */
    @tailrec final def append(elem: T): Unrolled[T] = if (size < array.length) {
      array(size) = elem
      size += 1
      this
    } else {
      next = new Unrolled[T](0, new Array[T](nextlength), null, buff)
      next.nn append elem
    }
    /** Applies `f` to each element of this node and all following nodes.
     *
     *  @tparam U the result type of `f`, which is discarded
     *  @param f the function applied to each element for its side effect
     */
    def foreach[U](f: T => U): Unit = {
      var unrolled: Unrolled[T] | Null = this
      var i = 0
      while (unrolled ne null) {
        val chunkarr = unrolled.array
        val chunksz = unrolled.size
        while (i < chunksz) {
          val elem = chunkarr(i)
          f(elem)
          i += 1
        }
        i = 0
        unrolled = unrolled.next
      }
    }
    /** Replaces each element of this node and all following nodes with `f`
     *  applied to it.
     *
     *  @param f the mapping function
     */
    def mapInPlace(f: T => T): Unit = {
      var unrolled: Unrolled[T] | Null = this
      var i = 0
      while (unrolled ne null) {
        val chunkarr = unrolled.array
        val chunksz = unrolled.size
        while (i < chunksz) {
          val elem = chunkarr(i)
          chunkarr(i) = f(elem)
          i += 1
        }
        i = 0
        unrolled = unrolled.next
      }
    }
    /** Returns the element at index `idx`, counting from the start of this
     *  node and following the node chain as needed.
     *
     *  Assumes `idx` is within bounds; the public `UnrolledBuffer` methods
     *  check bounds before delegating here.
     *
     *  @param idx the index, relative to this node, of the element to return
     *  @return the element at index `idx`
     */
    @tailrec final def apply(idx: Int): T =
      if (idx < size) array(idx) else next.nn.apply(idx - size)
    /** Replaces the element at index `idx`, counting from the start of this
     *  node and following the node chain as needed, with `newelem`.
     *
     *  Assumes `idx` is within bounds; the public `UnrolledBuffer` methods
     *  check bounds before delegating here.
     *
     *  @param idx the index, relative to this node, of the element to replace
     *  @param newelem the new element
     */
    @tailrec final def update(idx: Int, newelem: T): Unit =
      if (idx < size) array(idx) = newelem else next.nn.update(idx - size, newelem)
    /** Returns the node holding the element at index `idx`, counting from the
     *  start of this node.
     *
     *  Assumes `idx` is within bounds.
     *
     *  @param idx the index, relative to this node, to locate
     *  @return the node containing that index
     */
    @tailrec final def locate(idx: Int): Unrolled[T] =
      if (idx < size) this else next.nn.locate(idx - size)
    /** Inserts `elem` before the first element of this node, shifting the
     *  existing elements right, or into a freshly allocated head node if this
     *  node's array is full.
     *
     *  @param elem the element to prepend
     *  @return the new head of the chain: this node, or the newly allocated one
     */
    def prepend(elem: T) = if (size < array.length) {
      // shift the elements of the array right
      // then insert the element
      shiftright()
      array(0) = elem
      size += 1
      this
    } else {
      // allocate a new node and store element
      // then make it point to this
      val newhead = new Unrolled[T](buff)
      newhead append elem
      newhead.next = this
      newhead
    }
    // shifts right assuming enough space
    private def shiftright(): Unit = {
      var i = size - 1
      while (i >= 0) {
        array(i + 1) = array(i)
        i -= 1
      }
    }
    // returns pointer to new last if changed
    /** Removes and returns the element at index `idx`, counting from the start
     *  of this node.
     *
     *  The elements after it in its node are shifted left, and the node is
     *  merged with its successor if their combined element count falls below
     *  the waterline. When such a merge discards the chain's last node,
     *  `buffer`'s last-node pointer is updated to the merged node.
     *
     *  @param idx the index, relative to this node, of the element to remove
     *  @param buffer the buffer this chain belongs to, whose last-node pointer
     *                may need updating
     *  @return the removed element
     */
    @tailrec final def remove(idx: Int, buffer: UnrolledBuffer[T]): T =
      if (idx < size) {
        // remove the element
        // then try to merge with the next bucket
        val r = array(idx)
        shiftleft(idx)
        size -= 1
        if (tryMergeWithNext()) buffer.lastPtr = this
        r
      } else next.nn.remove(idx - size, buffer)

    /** Removes the first occurrence of `elem` in this node or any following
     *  node, via `remove`.
     *
     *  @param elem the element to remove
     *  @param buffer the buffer this chain belongs to, whose last-node pointer
     *                may need updating
     *  @return `true` if an occurrence was found and removed, `false` otherwise
     */
    @tailrec final def subtractOne(elem: T, buffer: UnrolledBuffer[T]): Boolean = {
      var i = 0
      while (i < size) {
        if(array(i) == elem) {
          remove(i, buffer)
          return true
        }
        i += 1
      }
      if(next ne null) next.nn.subtractOne(elem, buffer) else false
    }

    // shifts left elements after `leftb` (overwrites `leftb`)
    private def shiftleft(leftb: Int): Unit = {
      var i = leftb
      while (i < (size - 1)) {
        array(i) = array(i + 1)
        i += 1
      }
      nullout(i, i + 1)
    }
    /** Merges the next node into this one if the combined element count of the
     *  two nodes is below the waterline fraction (see
     *  [[UnrolledBuffer.waterline]]) of this node's array length.
     *
     *  @return `true` if a merge happened and the absorbed node was the last
     *          one in the chain, so the buffer's last-node pointer must be
     *          updated to this node; `false` otherwise
     */
    protected def tryMergeWithNext() = if (next != null && (size + next.nn.size) < (array.length * waterline / waterlineDenom)) {
      // copy the next array, then discard the next node
      Array.copy(next.nn.array, 0, array, size, next.nn.size)
      size = size + next.nn.size
      next = next.nn.next
      if (next eq null) true else false // checks if last node was thrown out
    } else false

    /** Inserts all elements of `t` at index `idx`, counting from the start of
     *  this node.
     *
     *  The node containing the index is split at that position into a front half
     *  and a fresh rear-half node; the new elements are appended after the front
     *  half, the rear half is linked back after them, and a waterline merge of the
     *  two is attempted. Inserting at the end of the node appends to it instead,
     *  with no split. `buffer`'s last-node pointer
     *  is updated when the insertion changes the chain's last node.
     *
     *  @param idx the index, relative to this node, at which to insert
     *  @param t the collection containing the elements to insert
     *  @param buffer the buffer this chain belongs to, whose last-node pointer
     *                may need updating
     *  @return the number of elements inserted
     */
    @tailrec final def insertAll(idx: Int, t: scala.collection.IterableOnce[T]^, buffer: UnrolledBuffer[T]): Int = {
      if (idx < size) {
        // divide this node at the appropriate position and insert all into head
        // update new next
        val newnextnode = new Unrolled[T](0, new Array(array.length), null, buff)
        Array.copy(array, idx, newnextnode.array, 0, size - idx)
        newnextnode.size = size - idx
        newnextnode.next = next

        // update this
        nullout(idx, size)
        size = idx
        next = null

        // insert everything from iterable to this
        var curr = this
        var appended = 0
        for (elem <- t.iterator) {
          curr = curr append elem
          appended += 1
        }
        curr.next = newnextnode

        // try to merge the last node of this with the newnextnode and fix tail pointer if needed
        if (curr.tryMergeWithNext()) buffer.lastPtr = curr
        else if (newnextnode.next eq null) buffer.lastPtr = newnextnode
        appended
      }
      else if (idx == size || (next eq null)) {
        var curr = this
        var appended = 0
        for (elem <- t.iterator) {
          curr = curr append elem
          appended += 1
        }
        appended
      }
      else next.nn.insertAll(idx - size, t, buffer)
    }

    private def nullout(from: Int, until: Int): Unit = {
      var idx = from
      while (idx < until) {
        array(idx) = null.asInstanceOf[T] // TODO find a way to assign a default here!!
        idx += 1
      }
    }

    // assumes this is the last node
    // `thathead` and `thatlast` are head and last node
    // of the other unrolled list, respectively
    /** Appends the chain starting at `thathead` after this node, which must be
     *  the last node of its own chain, then attempts a waterline merge.
     *
     *  @param thathead the head node of the other chain
     *  @return `true` if the merge absorbed the entire other chain, so that
     *          this node is still the last one; `false` otherwise
     */
    def bind(thathead: Unrolled[T]) = {
      assert(next eq null)
      next = thathead
      tryMergeWithNext()
    }

    /** Returns a debug string showing, for this node and all following nodes,
     *  the fill ratio (`size/capacity`) and the elements.
     */
    override def toString(): String =
      array.take(size).mkString(s"Unrolled@${System.identityHashCode(this).toHexString}[$size/${array.length}](", ", ", ")") + " -> " + (if (next ne null) next.toString else "")
  }
}

// This is used by scala.collection.parallel.mutable.UnrolledParArrayCombiner:
// Todo -- revisit whether inheritance is the best way to achieve this functionality
private[collection] class DoublingUnrolledBuffer[T](implicit t: ClassTag[T]) extends UnrolledBuffer[T]()(using t) {
  /** Returns the array length for the next node: double `sz` while `sz` is
   *  below 10000, and `sz` itself from then on.
   *
   *  @param sz the array length of the most recently allocated node
   */
  override def calcNextLength(sz: Int) = if (sz < 10000) sz * 2 else sz
  /** Creates a new, empty node associated with this buffer, holding an array of 4 elements. */
  override protected def newUnrolled = new UnrolledBuffer.Unrolled[T](0, new Array[T](4), null, this)
}
