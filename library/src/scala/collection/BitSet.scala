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

import scala.language.`2.13`
import language.experimental.captureChecking

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.annotation.nowarn
import scala.collection.Stepper.EfficientSplit
import scala.collection.mutable.Builder


/** Base type of bitsets.
 *
 *  This trait provides most of the operations of a `BitSet` independently of its representation.
 *  It is inherited by all concrete implementations of bitsets.
 *
 *  @define bitsetinfo
 *  Bitsets are sets of non-negative integers which are represented as
 *  variable-size arrays of bits packed into 64-bit words. The lower bound of memory footprint of a bitset is
 *  determined by the largest number stored in it.
 *  @define coll bitset
 *  @define Coll `BitSet`
 */
trait BitSet extends SortedSet[Int] with BitSetOps[BitSet] { self: BitSet =>
  /** Creates a bitset of the same kind as this bitset from the given
   *  collection of elements, delegating to `bitSetFactory`.
   *
   *  @param coll the collection of elements to include; all must be non-negative
   *  @return a new bitset containing the elements of `coll`
   *  @throws IllegalArgumentException if `coll` contains a negative element
   */
  override protected def fromSpecific(coll: IterableOnce[Int]^): BitSet = bitSetFactory.fromSpecific(coll)
  /** Returns a builder for bitsets of the same kind as this bitset, obtained from `bitSetFactory`. */
  override protected def newSpecificBuilder: Builder[Int, BitSet] = bitSetFactory.newBuilder
  /** Returns an empty bitset of the same kind as this bitset, obtained from `bitSetFactory`. */
  override def empty: BitSet = bitSetFactory.empty
  /** The prefix used in the string representation of this set, `"BitSet"`. */
  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected def stringPrefix = "BitSet"
  /** Returns this bitset viewed as an unsorted `Set[Int]`.
   *
   *  No copy is made: the result is this same bitset, so its iteration
   *  order remains increasing.
   */
  override def unsorted: Set[Int] = this
}

@SerialVersionUID(3L)
object BitSet extends SpecificIterableFactory[Int, BitSet] {
  private[collection] final val ordMsg = "No implicit Ordering[${B}] found to build a SortedSet[${B}]. You may want to upcast to a Set[Int] first by calling `unsorted`."
  private[collection] final val zipOrdMsg = "No implicit Ordering[${B}] found to build a SortedSet[(Int, ${B})]. You may want to upcast to a Set[Int] first by calling `unsorted`."

  /** Returns the empty bitset; this factory delegates to [[immutable.BitSet]]. */
  def empty: BitSet = immutable.BitSet.empty
  /** Returns a builder for immutable bitsets. */
  def newBuilder: Builder[Int, BitSet] = immutable.BitSet.newBuilder
  /** Creates an immutable bitset containing the elements of the given collection.
   *
   *  @param it the collection of elements to include; all must be non-negative
   *  @return a new immutable bitset containing the elements of `it`
   *  @throws IllegalArgumentException if `it` contains a negative element
   */
  def fromSpecific(it: IterableOnce[Int]^): BitSet = immutable.BitSet.fromSpecific(it)

  @SerialVersionUID(3L)
  private[collection] abstract class SerializationProxy(@transient protected val coll: BitSet) extends Serializable {

    /** The 64-bit words of the proxied bitset, populated by `readObject`
     *  during Java deserialization and read by `readResolve` to rebuild the
     *  bitset.
     */
    @transient protected var elems: Array[Long] = compiletime.uninitialized

    private def writeObject(out: ObjectOutputStream): Unit = {
      out.defaultWriteObject()
      val nwords = coll.nwords
      out.writeInt(nwords)
      var i = 0
      while(i < nwords) {
        out.writeLong(coll.word(i))
        i += 1
      }
    }

    private def readObject(in: ObjectInputStream): Unit = {
      in.defaultReadObject()
      val nwords = in.readInt()
      elems = new Array[Long](nwords)
      var i = 0
      while(i < nwords) {
        elems(i) = in.readLong()
        i += 1
      }
    }

    /** Reconstitutes the bitset from the deserialized words in `elems`,
     *  replacing this proxy at the end of Java deserialization.
     */
    protected def readResolve(): Any
  }
}

/** Base implementation type of bitsets.
 *
 *  @tparam C the type of the bitset itself, used for return types of operations
 */
transparent trait BitSetOps[+C <: BitSet & BitSetOps[C]]
  extends SortedSetOps[Int, SortedSet, C] { self =>
  import BitSetOps._

  /** The factory used to build bitsets of the same kind as this bitset. */
  def bitSetFactory: SpecificIterableFactory[Int, C]

  /** Returns this bitset viewed as an unsorted `Set[Int]`. */
  def unsorted: Set[Int]

  /** The ordering of this bitset, always [[scala.math.Ordering.Int]]:
   *  elements are iterated in increasing numeric order.
   */
  final def ordering: Ordering[Int] = Ordering.Int

  /** The number of words (each with 64 bits) making up the set. */
  protected[collection] def nwords: Int

  /** The words at index `idx`, or 0L if outside the range of the set
   *  **Note:** requires `idx >= 0`
   *
   *  @param idx the index of the word to retrieve, must be non-negative
   *  @return the 64-bit word at position `idx`, or `0L` if `idx` is outside the range of the set
   */
  protected[collection] def word(idx: Int): Long

  /** Creates a new set of this kind from an array of longs
   *
   *  @param elems the array of 64-bit words representing the bit mask for the new set
   *  @return a new bitset of type `C` whose contents are given by the bit mask in `elems` (the array is used directly and not copied)
   */
  protected[collection] def fromBitMaskNoCopy(elems: Array[Long]): C

  /** Tests whether this bitset contains the given integer.
   *
   *  Tests a single bit of a single word, so the check runs in constant time.
   *
   *  @param elem the integer to test
   *  @return `true` if `elem` is non-negative and its bit is set in this
   *          bitset, `false` otherwise (in particular, always `false` for
   *          negative values)
   */
  def contains(elem: Int): Boolean =
    0 <= elem && (word(elem >> LogWL) & (1L << elem)) != 0L

  /** Returns an iterator over the elements of this bitset in increasing order. */
  def iterator: Iterator[Int] = iteratorFrom(0)

  /** Returns an iterator over all elements of this bitset greater than or
   *  equal to `start`, in increasing order.
   *
   *  The iterator starts directly at the word containing `start` rather than
   *  scanning from the beginning.
   *
   *  @param start the lower bound (inclusive) of the iterator; a
   *               non-positive value yields all elements
   *  @return an iterator over the elements of this bitset that are greater
   *          than or equal to `start`
   */
  def iteratorFrom(start: Int): Iterator[Int] = new AbstractIterator[Int] {
    private var currentPos = if (start > 0) start >> LogWL else 0
    private var currentWord = if (start > 0) word(currentPos) & (-1L << (start & (WordLength - 1))) else word(0)
    final override def hasNext: Boolean = {
      while (currentWord == 0) {
        if (currentPos + 1 >= nwords) return false
        currentPos += 1
        currentWord = word(currentPos)
      }
      true
    }
    final override def next(): Int = {
      if (hasNext) {
        val bitPos = java.lang.Long.numberOfTrailingZeros(currentWord)
        currentWord &= currentWord - 1
        (currentPos << LogWL) + bitPos
      } else Iterator.empty.next()
    }
  }

  /** Returns a [[scala.collection.Stepper]] for the elements of this bitset
   *  that supports efficient splitting, enabling parallel processing.
   *
   *  Yields an [[scala.collection.IntStepper]] that works on unboxed values,
   *  wrapped in a boxing [[scala.collection.AnyStepper]] if the reference
   *  shape is requested.
   *
   *  @tparam S the type of the returned `Stepper`, determined by the implicit `StepperShape`
   *  @param shape the `StepperShape` that determines the concrete `Stepper` subtype to return
   *  @return a stepper over the elements of this bitset, marked with
   *          [[scala.collection.Stepper.EfficientSplit]]
   */
  override def stepper[S <: Stepper[?]](implicit shape: StepperShape[Int, S]): S & EfficientSplit = {
    val st = scala.collection.convert.impl.BitSetStepper.from(this)
    val r =
      if (shape.shape == StepperShape.IntShape) st
      else {
        assert(shape.shape == StepperShape.ReferenceShape, s"unexpected StepperShape: $shape")
        AnyStepper.ofParIntStepper(st)
      }
    r.asInstanceOf[S & EfficientSplit]
  }

  /** The number of elements in this bitset.
   *
   *  Computed by counting the set bits of each word, so it takes time
   *  proportional to the number of words rather than the number of elements.
   */
  override def size: Int = {
    var s = 0
    var i = nwords
    while (i > 0) {
      i -= 1
      s += java.lang.Long.bitCount(word(i))
    }
    s
  }

  /** Tests whether this bitset is empty, that is, whether every word is zero. */
  override def isEmpty: Boolean = 0 until nwords forall (i => word(i) == 0)

  @inline private def smallestInt: Int = {
    val thisnwords = nwords
    var i = 0
    while(i < thisnwords) {
      val currentWord = word(i)
      if (currentWord != 0L) {
        return java.lang.Long.numberOfTrailingZeros(currentWord) + (i * WordLength)
      }
      i += 1
    }
    throw new UnsupportedOperationException("empty.smallestInt")
  }

  @inline private def largestInt: Int = {
    var i = nwords - 1
    while(i >= 0) {
      val currentWord = word(i)
      if (currentWord != 0L) {
        return ((i + 1) * WordLength) - java.lang.Long.numberOfLeadingZeros(currentWord) - 1
      }
      i -= 1
    }
    throw new UnsupportedOperationException("empty.largestInt")
  }

  /** Returns the largest element of this bitset under the given ordering.
   *
   *  For the standard `Int` ordering, the result is found directly from the
   *  highest set bit; for the reversed `Int` ordering, from the lowest set
   *  bit. Any other ordering falls back to the general implementation, which
   *  iterates over all elements.
   *
   *  @tparam B the type over which the ordering is defined (a supertype of `Int`)
   *  @param ord the ordering used to compare elements
   *  @return the largest element of this bitset with respect to `ord`
   *  @throws UnsupportedOperationException if this bitset is empty
   */
  override def max[B >: Int](implicit ord: Ordering[B]): Int =
    if (Ordering.Int eq ord) largestInt
    else if (Ordering.Int isReverseOf ord) smallestInt
    else super.max(using ord)


  /** Returns the smallest element of this bitset under the given ordering.
   *
   *  For the standard `Int` ordering, the result is found directly from the
   *  lowest set bit; for the reversed `Int` ordering, from the highest set
   *  bit. Any other ordering falls back to the general implementation, which
   *  iterates over all elements.
   *
   *  @tparam B the type over which the ordering is defined (a supertype of `Int`)
   *  @param ord the ordering used to compare elements
   *  @return the smallest element of this bitset with respect to `ord`
   *  @throws UnsupportedOperationException if this bitset is empty
   */
  override def min[B >: Int](implicit ord: Ordering[B]): Int =
    if (Ordering.Int eq ord) smallestInt
    else if (Ordering.Int isReverseOf ord) largestInt
    else super.min(using ord)

  /** Applies the function `f` to each element of this bitset in increasing
   *  order.
   *
   *  Scans the words and their set bits directly instead of creating an
   *  iterator.
   *
   *  @tparam U the result type of `f`, ignored
   *  @param f the function applied to each element for its side effects
   */
  override def foreach[U](f: Int => U): Unit = {
    /* NOTE: while loops are significantly faster as of 2.11 and
       one major use case of bitsets is performance. Also, there
       is nothing to do when all bits are clear, so use that as
       the inner loop condition. */
    var i = 0
    while (i < nwords) {
      var w = word(i)
      var j = i * WordLength
      while (w != 0L) {
        if ((w&1L) == 1L) f(j)
        w = w >>> 1
        j += 1
      }
      i += 1
    }
  }

  /** Creates a bit mask for this set as a new array of longs */
  def toBitMask: Array[Long] = {
    val a = new Array[Long](nwords)
    var i = a.length
    while(i > 0) {
      i -= 1
      a(i) = word(i)
    }
    a
  }

  /** Creates a bitset containing the elements of this bitset that fall
   *  within the given optional bounds.
   *
   *  The result is built from a masked copy of this bitset's words, so it is
   *  a new independent bitset, not a projection: later changes to either
   *  bitset are not reflected in the other.
   *
   *  @param from the lower bound (inclusive) of the range; `None` if there
   *              is no lower bound
   *  @param until the upper bound (exclusive) of the range; `None` if there
   *               is no upper bound
   *  @return a new bitset containing the elements of this bitset that are
   *          within the given bounds
   */
  def rangeImpl(from: Option[Int], until: Option[Int]): C = {
    val a = coll.toBitMask
    val len = a.length
    if (from.isDefined) {
      val f = from.get
      val w = f >> LogWL
      val b = f & (WordLength - 1)
      if (w >= 0) {
        java.util.Arrays.fill(a, 0, math.min(w, len), 0)
        if (b > 0 && w < len) a(w) &= ~((1L << b) - 1)
      }
    }
    if (until.isDefined) {
      val u = until.get
      val w = u >> LogWL
      val b = u & (WordLength - 1)
      if (w < len) {
        java.util.Arrays.fill(a, math.max(w + 1, 0), len, 0)
        if (w >= 0) a(w) &= (1L << b) - 1
      }
    }
    coll.fromBitMaskNoCopy(a)
  }

  /** Computes the union of this bitset and another collection.
   *
   *  When `other` is also a bitset, the union is computed word-by-word with
   *  bitwise "or"; otherwise the elements of `other` are added individually.
   *
   *  @param other the collection of elements to add
   *  @return a new bitset containing all elements of this bitset and all
   *          elements of `other`
   *  @throws IllegalArgumentException if `other` is not a bitset and
   *          contains a negative element
   */
  override def concat(other: collection.IterableOnce[Int]^): C = other match {
    case otherBitset: BitSet =>
      val len = coll.nwords max otherBitset.nwords
      val words = new Array[Long](len)
      for (idx <- 0 until len)
        words(idx) = this.word(idx) | otherBitset.word(idx)
      fromBitMaskNoCopy(words)
    case _ => super.concat(other)
  }

  /** Computes the intersection of this bitset and another set.
   *
   *  When `other` is also a bitset, the intersection is computed
   *  word-by-word with bitwise "and"; otherwise this bitset is filtered by
   *  membership in `other`.
   *
   *  @param other the set to intersect with
   *  @return a new bitset containing the elements of this bitset that are
   *          also in `other`
   */
  override def intersect(other: Set[Int]): C = other match {
    case otherBitset: BitSet =>
      val len = coll.nwords min otherBitset.nwords
      val words = new Array[Long](len)
      for (idx <- 0 until len)
        words(idx) = this.word(idx) & otherBitset.word(idx)
      fromBitMaskNoCopy(words)
    case _ => super.intersect(other)
  }

  /** Computes the difference of this bitset and another set.
   *
   *  When `other` is also a bitset, the difference is computed word-by-word
   *  with bitwise "and not"; otherwise it falls back to the general set
   *  difference.
   *
   *  @param other the set of elements to exclude
   *  @return a new bitset containing the elements of this bitset that are
   *          not also in `other`
   */
  abstract override def diff(other: Set[Int]): C = other match {
    case otherBitset: BitSet =>
      val len = coll.nwords
      val words = new Array[Long](len)
      for (idx <- 0 until len)
        words(idx) = this.word(idx) & ~otherBitset.word(idx)
      fromBitMaskNoCopy(words)
    case _ => super.diff(other)
  }

  /** Computes the symmetric difference of this bitset and another bitset by performing
   *  a bitwise "exclusive-or".
   *
   *  @param other the other bitset to take part in the symmetric difference.
   *  @return     a bitset containing those bits of this
   *              bitset or the other bitset that are not contained in both bitsets.
   */
  def xor(other: BitSet): C = {
    val len = coll.nwords max other.nwords
    val words = new Array[Long](len)
    for (idx <- 0 until len)
      words(idx) = coll.word(idx) ^ other.word(idx)
    coll.fromBitMaskNoCopy(words)
  }

  @`inline` final def ^ (other: BitSet): C = xor(other)

  /** Builds a new bitset by applying a function to all elements of this bitset.
   *  @param f the function to apply to each element.
   *  @return a new bitset resulting from applying the given function *f* to
   *         each element of this bitset and collecting the results
   */
  def map(f: Int => Int): C = fromSpecific(new View.Map(this, f))

  /** Builds a new bitset by applying a function to all elements of this
   *  bitset and concatenating the resulting collections.
   *
   *  @param f the function to apply to each element
   *  @return a new bitset containing the elements of the collections
   *          returned by `f` for the elements of this bitset
   *  @throws IllegalArgumentException if a collection returned by `f`
   *          contains a negative value
   */
  def flatMap(f: Int => IterableOnce[Int]^): C = fromSpecific(new View.FlatMap(this, f))

  /** Builds a new bitset by applying a partial function to all elements of
   *  this bitset on which the function is defined.
   *
   *  @param pf the partial function to apply to each element
   *  @return a new bitset containing the results of applying `pf` to each
   *          element of this bitset on which it is defined
   *  @throws IllegalArgumentException if `pf` returns a negative value for
   *          some element
   */
  def collect(pf: PartialFunction[Int, Int]^): C = fromSpecific(super[SortedSetOps].collect(pf))

  /** Splits this bitset into a pair of bitsets according to a predicate.
   *
   *  The first bitset is obtained by filtering with `p`; the second is the
   *  difference of this bitset and the first.
   *
   *  @param p the predicate used to test elements
   *  @return a pair of bitsets: the first containing all elements of this
   *          bitset that satisfy `p`, the second containing those that do not
   */
  override def partition(p: Int => Boolean): (C, C) = {
    val left = filter(p)
    (left, this &~ left)
  }
}

object BitSetOps {

  /* Final vals can sometimes be inlined as constants (faster) */
  private[collection] final val LogWL = 6
  private[collection] final val WordLength = 64
  private[collection] final val MaxSize = (Int.MaxValue >> LogWL) + 1

  private[collection] def updateArray(elems: Array[Long], idx: Int, w: Long): Array[Long] = {
    var len = elems.length
    while (len > 0 && (elems(len - 1) == 0L || w == 0L && idx == len - 1)) len -= 1
    var newlen = len
    if (idx >= newlen && w != 0L) newlen = idx + 1
    val newelems = new Array[Long](newlen)
    Array.copy(elems, 0, newelems, 0, len)
    if (idx < newlen) newelems(idx) = w
    else assert(w == 0L)
    newelems
  }

  private[collection] def computeWordForFilter(pred: Int => Boolean, isFlipped: Boolean, oldWord: Long, wordIndex: Int): Long =
    if (oldWord == 0L) 0L else {
      var w = oldWord
      val trailingZeroes = java.lang.Long.numberOfTrailingZeros(w)
      var jmask = 1L << trailingZeroes
      var j = wordIndex * BitSetOps.WordLength + trailingZeroes
      val maxJ = (wordIndex + 1) * BitSetOps.WordLength - java.lang.Long.numberOfLeadingZeros(w)
      while (j != maxJ) {
        if ((w & jmask) != 0L) {
          if (pred(j) == isFlipped) {
            // j did not pass the filter here
            w = w & ~jmask
          }
        }
        jmask = jmask << 1
        j += 1
      }
      w
    }
}
