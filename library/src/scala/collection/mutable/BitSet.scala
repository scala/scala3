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

import scala.collection.immutable.Range
import BitSetOps.{LogWL, MaxSize}
import scala.annotation.implicitNotFound

/** A class for mutable bitsets.
 *
 *  $bitsetinfo
 *
 *  @see ["Scala's Collection Library overview"](https://docs.scala-lang.org/overviews/collections-2.13/concrete-mutable-collection-classes.html#mutable-bitsets)
 *  section on `Mutable Bitsets` for more information.
 *
 *  @define Coll `BitSet`
 *  @define coll bitset
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 *
 *  @param protected[collection] final var elems the underlying array of `Long` words storing the bits; used directly without copying, so external mutations will affect this bitset
 */
class BitSet(protected[collection] final var elems: Array[Long])
  extends AbstractSet[Int]
    with SortedSet[Int]
    with SortedSetOps[Int, SortedSet, BitSet]
    with StrictOptimizedIterableOps[Int, Set, BitSet]
    with StrictOptimizedSortedSetOps[Int, SortedSet, BitSet]
    with collection.BitSet
    with collection.BitSetOps[BitSet]
    with Serializable {

  /** Creates a new empty bitset with an initial capacity for elements up to `initSize - 1`.
   *
   *  The bitset grows automatically when larger elements are added.
   *
   *  @param initSize the number of elements the initial word array can represent;
   *                  at least one 64-bit word is always allocated, and a value within
   *                  63 of `Int.MaxValue` overflows the rounding and allocates just one
   */
  def this(initSize: Int) = this(new Array[Long](math.max((initSize + 63) >> 6, 1)))

  /** Creates a new empty bitset with the minimal initial capacity of one 64-bit word. */
  def this() = this(0)

  /** Builds a new mutable bitset containing the elements of the given collection.
   *
   *  @param coll the collection of non-negative integers to include
   *  @return a new mutable bitset containing the elements of `coll`
   *  @throws IllegalArgumentException if `coll` contains a negative element
   */
  override protected def fromSpecific(coll: IterableOnce[Int]^): BitSet = bitSetFactory.fromSpecific(coll)
  /** Returns a new builder that accumulates `Int` elements into a mutable bitset. */
  override protected def newSpecificBuilder: Builder[Int, BitSet] = bitSetFactory.newBuilder
  /** Returns a new empty mutable bitset. */
  override def empty: BitSet = bitSetFactory.empty

  /** The factory used to build mutable bitsets, the [[BitSet$ `BitSet`]] companion object. */
  def bitSetFactory: BitSet.type = BitSet

  /** Returns this bitset viewed as an unsorted `Set[Int]`.
   *
   *  No copy is made: the result is this same bitset, so its iteration
   *  order remains increasing.
   */
  override def unsorted: Set[Int] = this

  /** The number of 64-bit words currently allocated in the underlying array. */
  protected[collection] final def nwords: Int = elems.length

  /** Returns the word at the given index of the underlying array.
   *
   *  @param idx the word index
   *  @return the word at index `idx`, or `0L` if `idx` is beyond the allocated words
   */
  protected[collection] final def word(idx: Int): Long =
    if (idx < nwords) elems(idx) else 0L

  /** Creates a new mutable bitset backed directly by the given array, without copying.
   *
   *  @param elems the array of `Long` words representing the bits; used as the
   *               backing array of the result unless empty
   *  @return a new bitset backed by `elems`, or the empty bitset if `elems` is empty
   */
  protected[collection] def fromBitMaskNoCopy(elems: Array[Long]): BitSet =
    if (elems.length == 0) empty
    else new BitSet(elems)

  /** Adds a single element to this bitset.
   *
   *  Grows the underlying array if `elem` does not fit in the current capacity.
   *
   *  @param elem the element to add; must be non-negative
   *  @return this bitset
   *  @throws IllegalArgumentException if `elem` is negative
   */
  def addOne(elem: Int): this.type = {
    require(elem >= 0)
    if (!contains(elem)) {
      val idx = elem >> LogWL
      updateWord(idx, word(idx) | (1L << elem))
    }
    this
  }

  /** Removes a single element from this bitset, if it is present.
   *
   *  The underlying array never shrinks: removing an element clears its bit
   *  but keeps the allocated words.
   *
   *  @param elem the element to remove; must be non-negative
   *  @return this bitset
   *  @throws IllegalArgumentException if `elem` is negative
   */
  def subtractOne(elem: Int): this.type = {
    require(elem >= 0)
    if (contains(elem)) {
      val idx = elem >> LogWL
      updateWord(idx, word(idx) & ~(1L << elem))
    }
    this
  }

  /** Removes all elements from this bitset by replacing the underlying array
   *  with a zeroed array of the same length, keeping the current capacity.
   */
  def clear(): Unit = {
    elems = new Array[Long](elems.length)
  }

  /** Sets the word at the given index of the underlying array, growing the
   *  array first if needed.
   *
   *  @param idx the word index to update
   *  @param w the new word value
   *  @throws IllegalArgumentException if `idx` is not less than the maximum
   *          number of words a bitset can hold
   */
  protected final def updateWord(idx: Int, w: Long): Unit = {
    ensureCapacity(idx)
    elems(idx) = w
  }

  /** Grows the underlying array, if needed, so that it has a word at index `idx`.
   *
   *  The array length is repeatedly doubled (capped at the maximum size) until
   *  it exceeds `idx`; existing words are copied over. Does nothing if the
   *  array is already large enough.
   *
   *  @param idx the word index that must be within the array after this call
   *  @throws IllegalArgumentException if `idx` is not less than the maximum
   *          number of words a bitset can hold
   */
  protected final def ensureCapacity(idx: Int): Unit = {
    require(idx < MaxSize)
    if (idx >= nwords) {
      var newlen = nwords
      while (idx >= newlen) newlen = math.min(newlen * 2, MaxSize)
      val elems1 = new Array[Long](newlen)
      Array.copy(elems, 0, elems1, 0, nwords)
      elems = elems1
    }
  }

  /** Returns this bitset viewed as a general `Set[Int]` without the sorted-set
   *  constraint. No copy is made: the result is this same bitset.
   */
  def unconstrained: collection.Set[Int] = this

  /** Updates this bitset to the union with another bitset by performing a bitwise "or".
   *
   *  @param   other  the bitset to form the union with.
   *  @return  the bitset itself.
   */
  def |= (other: collection.BitSet): this.type = {
    ensureCapacity(other.nwords - 1)
    var i = 0
    val othernwords = other.nwords
    while (i < othernwords) {
      elems(i) = elems(i) | other.word(i)
      i += 1
    }
    this
  }
  /** Updates this bitset to the intersection with another bitset by performing a bitwise "and".
   *
   *  @param   other  the bitset to form the intersection with.
   *  @return  the bitset itself.
   */
  def &= (other: collection.BitSet): this.type = {
    // Different from other operations: no need to ensure capacity because
    // anything beyond the capacity is 0.  Since we use other.word which is 0
    // off the end, we also don't need to make sure we stay in bounds there.
    var i = 0
    val thisnwords = nwords
    while (i < thisnwords) {
      elems(i) = elems(i) & other.word(i)
      i += 1
    }
    this
  }
  /** Updates this bitset to the symmetric difference with another bitset by performing a bitwise "xor".
   *
   *  @param   other  the bitset to form the symmetric difference with.
   *  @return  the bitset itself.
   */
  def ^= (other: collection.BitSet): this.type = {
    ensureCapacity(other.nwords - 1)
    var i = 0
    val othernwords = other.nwords
    while (i < othernwords) {

      elems(i) = elems(i) ^ other.word(i)
      i += 1
    }
    this
  }
  /** Updates this bitset to the difference with another bitset by performing a bitwise "and-not".
   *
   *  @param   other  the bitset to form the difference with.
   *  @return  the bitset itself.
   */
  def &~= (other: collection.BitSet): this.type = {
    var i = 0
    val max = Math.min(nwords, other.nwords)
    while (i < max) {
      elems(i) = elems(i) & ~other.word(i)
      i += 1
    }
    this
  }

  /** Returns a copy of this bitset, backed by a copy of the underlying array,
   *  so later changes to either bitset do not affect the other.
   */
  override def clone(): BitSet = new BitSet(java.util.Arrays.copyOf(elems, elems.length))

  /** Returns an immutable bitset containing the same elements as this bitset.
   *
   *  The underlying array is copied, so later changes to this bitset do not
   *  affect the result.
   */
  def toImmutable: immutable.BitSet = immutable.BitSet.fromBitMask(elems)

  /** Builds a new bitset by applying a function to all elements of this bitset.
   *
   *  @param f the function to apply to each element; must return non-negative values
   *  @return a new bitset containing the results of applying `f` to each element
   *  @throws IllegalArgumentException if `f` returns a negative value for some element
   */
  override def map(f: Int => Int): BitSet = strictOptimizedMap(newSpecificBuilder, f)
  /** Builds a new sorted set by applying a function to all elements of this bitset.
   *
   *  @tparam B the element type of the returned set
   *  @param f the function to apply to each element
   *  @param ev the ordering used to sort the resulting elements
   *  @return a new sorted set containing the results of applying `f` to each element
   */
  override def map[B](f: Int => B)(implicit @implicitNotFound(collection.BitSet.ordMsg) ev: Ordering[B]): SortedSet[B] =
    super[StrictOptimizedSortedSetOps].map(f)

  /** Builds a new bitset by applying a function to all elements of this bitset
   *  and concatenating the results.
   *
   *  @param f the function to apply to each element; the collections it returns
   *           must contain only non-negative values
   *  @return a new bitset containing all elements of the collections returned by `f`
   *  @throws IllegalArgumentException if a collection returned by `f` contains a negative value
   */
  override def flatMap(f: Int => IterableOnce[Int]^): BitSet = strictOptimizedFlatMap(newSpecificBuilder, f)
  /** Builds a new sorted set by applying a function to all elements of this
   *  bitset and concatenating the results.
   *
   *  @tparam B the element type of the returned set
   *  @param f the function to apply to each element
   *  @param ev the ordering used to sort the resulting elements
   *  @return a new sorted set containing all elements of the collections returned by `f`
   */
  override def flatMap[B](f: Int => IterableOnce[B]^)(implicit @implicitNotFound(collection.BitSet.ordMsg) ev: Ordering[B]): SortedSet[B] =
    super[StrictOptimizedSortedSetOps].flatMap(f)

  /** Builds a new bitset by applying a partial function to all elements of
   *  this bitset on which the function is defined.
   *
   *  @param pf the partial function to apply to each element; must return
   *            non-negative values where defined
   *  @return a new bitset containing the results of applying `pf` to each
   *          element on which it is defined
   *  @throws IllegalArgumentException if `pf` returns a negative value for some element
   */
  override def collect(pf: PartialFunction[Int, Int]^): BitSet = strictOptimizedCollect(newSpecificBuilder, pf)
  /** Builds a new sorted set by applying a partial function to all elements of
   *  this bitset on which the function is defined.
   *
   *  @tparam B the element type of the returned set
   *  @param pf the partial function to apply to each element
   *  @param ev the ordering used to sort the resulting elements
   *  @return a new sorted set containing the results of applying `pf` to each
   *          element on which it is defined
   */
  override def collect[B](pf: scala.PartialFunction[Int, B]^)(implicit @implicitNotFound(collection.BitSet.ordMsg) ev: Ordering[B]): SortedSet[B] =
    super[StrictOptimizedSortedSetOps].collect(pf)

  // necessary for disambiguation
  /** Returns a sorted set of pairs formed by zipping the elements of this
   *  bitset, in increasing order, with the elements of `that`.
   *
   *  If one of the two collections is longer than the other, its remaining
   *  elements are dropped.
   *
   *  @tparam B the element type of `that`
   *  @param that the collection providing the second half of each result pair
   *  @param ev the ordering used to sort the resulting pairs
   *  @return a new sorted set of pairs
   */
  override def zip[B](that: IterableOnce[B]^)(implicit @implicitNotFound(collection.BitSet.zipOrdMsg) ev: Ordering[(Int, B)]): SortedSet[(Int, B)] =
    super.zip(that)

  /** Adds all elements of the given collection to this bitset.
   *
   *  Optimized paths avoid element-by-element insertion where possible: another
   *  bitset is combined word-by-word with `|=`, a range with step 1 or -1 is
   *  set with whole-word masks, and a sorted set ordered by the natural `Int`
   *  ordering (or its reverse) has capacity ensured up front for its largest
   *  element.
   *
   *  @param xs the collection of elements to add; all must be non-negative
   *  @return this bitset
   *  @throws IllegalArgumentException if `xs` contains a negative element
   */
  override def addAll(xs: IterableOnce[Int]^): this.type = xs match {
    case bs: collection.BitSet =>
      this |= bs
    case range: Range =>
      if (range.nonEmpty) {
        val start = range.min
        if (start >= 0) {
          val end = range.max
          val endIdx = end >> LogWL
          ensureCapacity(endIdx)

          if (range.step == 1 || range.step == -1) {
            val startIdx = start >> LogWL
            val wordStart = startIdx * BitSetOps.WordLength
            val wordMask = -1L << (start - wordStart)

            if (endIdx > startIdx) {
              elems(startIdx) |= wordMask
              java.util.Arrays.fill(elems, startIdx + 1, endIdx, -1L)
              elems(endIdx) |= -1L >>> (BitSetOps.WordLength - (end - endIdx * BitSetOps.WordLength) - 1)
            } else elems(endIdx) |= (wordMask & (-1L >>> (BitSetOps.WordLength - (end - wordStart) - 1)))
          } else super.addAll(range)
        } else super.addAll(range)
      }
      this

    case sorted: collection.SortedSet[Int @unchecked] =>
      // if `sorted` is using the regular Int ordering, ensure capacity for the largest
      // element up front to avoid multiple resizing allocations
      if (sorted.nonEmpty) {
        val ord = sorted.ordering
        if (ord eq Ordering.Int) {
          ensureCapacity(sorted.lastKey >> LogWL)
        } else if (ord eq Ordering.Int.reverse) {
          ensureCapacity(sorted.firstKey >> LogWL)
        }
        val iter = sorted.iterator
        while (iter.hasNext) {
          addOne(iter.next())
        }
      }

      this

    case other =>
      super.addAll(other)
  }

  /** Tests whether this bitset is a subset of another set.
   *
   *  When `that` is also a bitset, the test compares whole words instead of
   *  testing elements one by one.
   *
   *  @param that the set to test against
   *  @return `true` if every element of this bitset is also an element of `that`
   */
  override def subsetOf(that: collection.Set[Int]): Boolean = that match {
    case bs: collection.BitSet =>
      val thisnwords = this.nwords
      val bsnwords = bs.nwords
      val minWords = Math.min(thisnwords, bsnwords)

      // if any bits are set to `1` in words out of range of `bs`, then this is not a subset. Start there
      var i = bsnwords
      while (i < thisnwords) {
        if (word(i) != 0L) return false
        i += 1
      }

      // the higher range of `this` is all `0`s, fall back to lower range
      var j = 0
      while (j < minWords) {
        if ((word(j) & ~bs.word(j)) != 0L) return false
        j += 1
      }

      true
    case other =>
      super.subsetOf(other)
  }

  /** Removes all elements of the given collection from this bitset.
   *
   *  When `xs` is also a bitset, the removal is performed word-by-word with `&~=`.
   *
   *  @param xs the collection of elements to remove; non-negative elements are
   *            removed if present
   *  @return this bitset
   *  @throws IllegalArgumentException if `xs` is not a bitset and contains a
   *          negative element
   */
  override def subtractAll(xs: IterableOnce[Int]^): this.type = xs match {
    case bs: collection.BitSet => this &~= bs
    case other => super.subtractAll(other)
  }

  /** Replaces this bitset with a serialization proxy during Java serialization. */
  protected def writeReplace(): AnyRef = new BitSet.SerializationProxy(this)

  /** Computes the difference of this bitset and another set.
   *
   *  This bitset is not modified. When `that` is also a bitset, the difference
   *  is computed word-by-word, and the result's array is trimmed to the highest
   *  non-zero word where possible.
   *
   *  @param that the set of elements to exclude
   *  @return a new bitset containing the elements of this bitset that are not
   *          also in `that`
   */
  override def diff(that: collection.Set[Int]): BitSet = that match {
    case bs: collection.BitSet =>
      /*
        * Algorithm:
        *
        * We iterate, word-by-word, backwards from the shortest of the two bitsets (this, or bs) i.e. the one with
        * the fewer words.
        *
        * Array Shrinking:
        * If `this` is not longer than `bs`, then since we must iterate through the full array of words,
        * we can track the new highest index word which is non-zero, at little additional cost. At the end, the new
        * Array[Long] allocated for the returned BitSet will only be of size `maxNonZeroIndex + 1`
        */

      val bsnwords = bs.nwords
      val thisnwords = nwords
      if (bsnwords >= thisnwords) {
        // here, we may have opportunity to shrink the size of the array
        // so, track the highest index which is non-zero. That ( + 1 ) will be our new array length
        var i = thisnwords - 1
        var currentWord = 0L

        while (i >= 0 && currentWord == 0L) {
          val oldWord = word(i)
          currentWord = oldWord & ~bs.word(i)
          i -= 1
        }

        if (i < 0) {
          fromBitMaskNoCopy(Array(currentWord))
        } else {
          val minimumNonZeroIndex: Int = i + 1
          val newArray = elems.take(minimumNonZeroIndex + 1)
          newArray(i + 1) = currentWord
          while (i >= 0) {
            newArray(i) = word(i) & ~bs.word(i)
            i -= 1
          }
          fromBitMaskNoCopy(newArray)
        }
      } else {
        // here, there is no opportunity to shrink the array size, no use in tracking highest non-zero index
        val newElems = elems.clone()
        var i = bsnwords - 1
        while (i >= 0) {
          newElems(i) = word(i) & ~bs.word(i)
          i -= 1
        }
        fromBitMaskNoCopy(newElems)
      }
    case _ => super.diff(that)
  }

  /** Builds a new bitset containing the elements of this bitset that satisfy
   *  (or, if `isFlipped`, do not satisfy) a predicate. Implements both `filter`
   *  and `filterNot`. This bitset is not modified.
   *
   *  The elements are tested from highest to lowest so that the result's array
   *  is allocated once, sized exactly to its highest non-zero word.
   *
   *  @param pred the predicate to test elements against
   *  @param isFlipped if `false`, keep elements satisfying `pred` (`filter`);
   *                   if `true`, keep elements not satisfying it (`filterNot`)
   *  @return a new bitset containing the retained elements
   */
  override def filterImpl(pred: Int => Boolean, isFlipped: Boolean): BitSet = {
    // We filter the BitSet from highest to lowest, so we can determine exactly the highest non-zero word
    // index which lets us avoid:
    // * over-allocating -- the resulting array will be exactly the right size
    // * multiple resizing allocations -- the array is allocated one time, not log(n) times.
    var i = nwords - 1
    var newArray: Array[Long] | Null = null
    while (i >= 0) {
      val w = BitSetOps.computeWordForFilter(pred, isFlipped, word(i), i)
      if (w != 0L) {
        if (newArray eq null) {
          newArray = new Array(i + 1)
        }
        newArray(i) = w
      }
      i -= 1
    }
    if (newArray eq null) {
      empty
    } else {
      fromBitMaskNoCopy(newArray)
    }
  }

  /** Retains only the elements of this bitset that satisfy a predicate,
   *  clearing the bits of all elements that do not.
   *
   *  The underlying array keeps its length even if the highest words become zero.
   *
   *  @param p the predicate an element must satisfy to be retained
   *  @return this bitset
   */
  override def filterInPlace(p: Int => Boolean): this.type = {
    val thisnwords = nwords
    var i = 0
    while (i < thisnwords) {
      elems(i) = BitSetOps.computeWordForFilter(p, isFlipped = false, elems(i), i)
      i += 1
    }
    this
  }

  /** Returns a copy of the underlying array of `Long` words representing the
   *  bits of this bitset. Changes to the returned array do not affect this bitset.
   */
  override def toBitMask: Array[Long] = elems.clone()
}

@SerialVersionUID(3L)
object BitSet extends SpecificIterableFactory[Int, BitSet] {

  /** Creates a new mutable bitset containing the elements of the given collection.
   *
   *  @param it the collection of elements to include; all must be non-negative
   *  @return a new mutable bitset containing the elements of `it`
   *  @throws IllegalArgumentException if `it` contains a negative element
   */
  def fromSpecific(it: scala.collection.IterableOnce[Int]^): BitSet = Growable.from(empty, it)

  /** Returns a new empty mutable bitset. */
  def empty: BitSet = new BitSet()

  /** Returns a new builder that accumulates `Int` elements into a mutable bitset. */
  def newBuilder: Builder[Int, BitSet] = new GrowableBuilder(empty)

  /** A bitset containing all the bits in an array.
   *
   *  @param elems the array of `Long` words representing the bits; a defensive copy is made
   *  @return a new bitset backed by a copy of `elems`
   */
  def fromBitMask(elems: Array[Long]): BitSet = {
    val len = elems.length
    if (len == 0) empty
    else {
      val a = java.util.Arrays.copyOf(elems, len)
      new BitSet(a)
    }
  }

  /** A bitset containing all the bits in an array, wrapping the existing
   *  array without copying.
   *
   *  @param elems the array of `Long` words representing the bits, used directly without copying; the caller must not mutate the array afterward
   *  @return a new bitset backed directly by `elems`
   */
  def fromBitMaskNoCopy(elems: Array[Long]): BitSet = {
    val len = elems.length
    if (len == 0) empty
    else new BitSet(elems)
  }

  @SerialVersionUID(3L)
  private final class SerializationProxy(coll: BitSet) extends scala.collection.BitSet.SerializationProxy(coll) {
    protected def readResolve(): Any = BitSet.fromBitMaskNoCopy(elems)
  }
}
