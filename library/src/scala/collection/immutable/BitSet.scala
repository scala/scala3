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

import BitSetOps.{LogWL, updateArray}
import mutable.Builder
import scala.annotation.{implicitNotFound, nowarn}

/** A class for immutable bitsets.
 *  $bitsetinfo
 *  @see ["Scala's Collection Library overview"](https://docs.scala-lang.org/overviews/collections-2.13/concrete-immutable-collection-classes.html#immutable-bitsets)
 *  section on `Immutable BitSets` for more information.
 *
 *  @define Coll `immutable.BitSet`
 *  @define coll immutable bitset
 */
sealed abstract class BitSet
  extends AbstractSet[Int]
    with SortedSet[Int]
    with SortedSetOps[Int, SortedSet, BitSet]
    with StrictOptimizedSortedSetOps[Int, SortedSet, BitSet]
    with collection.BitSet
    with collection.BitSetOps[BitSet]
    with Serializable {

  /** Returns this bitset viewed as an unsorted `Set[Int]`.
   *
   *  No copy is made: the result is this same bitset, so its iteration order remains
   *  increasing.
   */
  override def unsorted: Set[Int] = this

  /** Returns an immutable bitset containing the elements of the given collection.
   *
   *  @param coll the collection of non-negative integers to include
   *  @return an immutable bitset containing the elements of `coll`, which is `coll`
   *          itself if it is already an immutable bitset
   */
  override protected def fromSpecific(coll: IterableOnce[Int]^): BitSet = bitSetFactory.fromSpecific(coll)
  /** Returns a new builder that accumulates `Int` elements into an immutable bitset. */
  override protected def newSpecificBuilder: Builder[Int, BitSet] = bitSetFactory.newBuilder
  /** Returns the empty immutable bitset. */
  override def empty: BitSet = bitSetFactory.empty

  /** The factory used to build immutable bitsets, the [[BitSet$ `BitSet`]] companion object. */
  def bitSetFactory: BitSet.type = BitSet

  /** Returns an immutable bitset holding the bits of the given array, without copying it.
   *
   *  Only a result of more than two words keeps `elems` as its backing array; the
   *  shorter representations store their words in fields instead, and an all-zero array
   *  yields the shared empty bitset.
   *
   *  @param elems the array of `Long` words representing the bits; the caller must not
   *               modify the array after this call
   *  @return a bitset holding the bits of `elems`
   */
  protected[collection] def fromBitMaskNoCopy(elems: Array[Long]): BitSet = BitSet.fromBitMaskNoCopy(elems)

  /** Returns a bitset containing `elem` and all elements of this bitset. Returns this
   *  bitset itself if it already contains `elem`; otherwise only the word holding
   *  `elem` differs, and the underlying storage grows if `elem` lies beyond it.
   *
   *  @param elem the element to add; must be non-negative
   *  @return a bitset containing all elements of this bitset plus `elem`
   *  @throws IllegalArgumentException if `elem` is negative
   */
  def incl(elem: Int): BitSet = {
    require(elem >= 0, "bitset element must be >= 0")
    if (contains(elem)) this
    else {
      val idx = elem >> LogWL
      updateWord(idx, word(idx) | (1L << elem))
    }
  }

  /** Returns a bitset containing all elements of this bitset except `elem`. Returns this
   *  bitset itself if it does not contain `elem`; otherwise only the word holding `elem`
   *  differs.
   *
   *  @param elem the element to remove; must be non-negative
   *  @return a bitset containing all elements of this bitset except `elem`
   *  @throws IllegalArgumentException if `elem` is negative
   */
  def excl(elem: Int): BitSet = {
    require(elem >= 0, "bitset element must be >= 0")
    if (contains(elem)) {
      val idx = elem >> LogWL
      updateWord(idx, word(idx) & ~(1L << elem))
    } else this
  }

  /** Updates word at index `idx`; enlarges set if `idx` outside range of set.
   *
   *  @param idx the index of the word to update
   *  @param w the new value for the word at index `idx`
   *  @return a new bitset with the word at `idx` set to `w`, growing the underlying storage if needed
   */
  protected def updateWord(idx: Int, w: Long): BitSet

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

  /** Builds a new bitset by applying a function to all elements of this bitset and
   *  concatenating the results.
   *
   *  @param f the function to apply to each element; the collections it returns must
   *           contain only non-negative values
   *  @return a new bitset containing all elements of the collections returned by `f`
   *  @throws IllegalArgumentException if a collection returned by `f` contains a negative value
   */
  override def flatMap(f: Int => IterableOnce[Int]^): BitSet = strictOptimizedFlatMap(newSpecificBuilder, f)
  /** Builds a new sorted set by applying a function to all elements of this bitset and
   *  concatenating the results.
   *
   *  @tparam B the element type of the returned set
   *  @param f the function to apply to each element
   *  @param ev the ordering used to sort the resulting elements
   *  @return a new sorted set containing all elements of the collections returned by `f`
   */
  override def flatMap[B](f: Int => IterableOnce[B]^)(implicit @implicitNotFound(collection.BitSet.ordMsg) ev: Ordering[B]): SortedSet[B] =
    super[StrictOptimizedSortedSetOps].flatMap(f)

  /** Builds a new bitset by applying a partial function to all elements of this bitset
   *  on which the function is defined.
   *
   *  @param pf the partial function to apply to each element; must return non-negative
   *            values where defined
   *  @return a new bitset containing the results of applying `pf` to each element on
   *          which it is defined
   *  @throws IllegalArgumentException if `pf` returns a negative value for some element
   */
  override def collect(pf: PartialFunction[Int, Int]^): BitSet = strictOptimizedCollect(newSpecificBuilder, pf)
  /** Builds a new sorted set by applying a partial function to all elements of this
   *  bitset on which the function is defined.
   *
   *  @tparam B the element type of the returned set
   *  @param pf the partial function to apply to each element
   *  @param ev the ordering used to sort the resulting elements
   *  @return a new sorted set containing the results of applying `pf` to each element on
   *          which it is defined
   */
  override def collect[B](pf: scala.PartialFunction[Int, B]^)(implicit @implicitNotFound(collection.BitSet.ordMsg) ev: Ordering[B]): SortedSet[B] =
    super[StrictOptimizedSortedSetOps].collect(pf)

  // necessary for disambiguation
  /** Returns a sorted set of pairs formed by zipping the elements of this bitset, in
   *  increasing order, with the elements of `that`.
   *
   *  If one of the two collections is longer than the other, its remaining elements are
   *  dropped.
   *
   *  @tparam B the element type of `that`
   *  @param that the collection providing the second half of each result pair
   *  @param ev the ordering used to sort the resulting pairs
   *  @return a new sorted set of pairs
   */
  override def zip[B](that: scala.IterableOnce[B]^)(implicit @implicitNotFound(collection.BitSet.zipOrdMsg) ev: Ordering[(Int, B)]): SortedSet[(Int, B)] =
    super.zip(that)

  /** Replaces this bitset with a serialization proxy that stores its words during Java serialization. */
  protected def writeReplace(): AnyRef = new BitSet.SerializationProxy(this)
}

/** $factoryInfo
 *  @define Coll `immutable.BitSet`
 *  @define coll immutable bitset
 */
@nowarn("cat=deprecation&msg=Implementation classes of BitSet should not be accessed directly")
@SerialVersionUID(3L)
object BitSet extends SpecificIterableFactory[Int, BitSet] {

  /** Returns an immutable bitset containing the elements of `it`.
   *
   *  If `it` is already an immutable bitset it is returned unchanged; otherwise its
   *  elements are collected into a new one.
   *
   *  @param it the collection of non-negative integers to include
   *  @throws IllegalArgumentException if `it` contains a negative value
   */
  def fromSpecific(it: scala.collection.IterableOnce[Int]^): BitSet =
    it match {
      case bs: BitSet => bs
      case _          => (newBuilder ++= it).result()
    }

  /** The empty immutable bitset, a single all-zero word shared by every call to `empty`. */
  final val empty: BitSet = new BitSet1(0L)

  /** Returns a new builder that accumulates `Int` elements into an immutable bitset.
   *
   *  The elements are collected in a mutable bitset, whose word array is handed to the
   *  result without being copied. It becomes the result's backing array only when it
   *  holds more than two words; a shorter result stores its words in fields instead.
   *
   *  @return a builder for immutable bitsets
   */
  def newBuilder: Builder[Int, BitSet] =
    mutable.BitSet.newBuilder.mapResult(bs => fromBitMaskNoCopy(bs.elems))

  private def createSmall(a: Long, b: Long): BitSet = if (b == 0L) new BitSet1(a) else new BitSet2(a, b)

  /** A bitset containing all the bits in an array.
   *
   *  @param elems the array of `Long` words representing the bits; the array is defensively copied
   *  @return a new immutable bitset containing the bits represented by `elems`
   */
  def fromBitMask(elems: Array[Long]): BitSet = {
    val len = elems.length
    if (len == 0) empty
    else if (len == 1) new BitSet1(elems(0))
    else if (len == 2) createSmall(elems(0), elems(1))
    else {
      val a = java.util.Arrays.copyOf(elems, len)
      new BitSetN(a)
    }
  }

  /** A bitset containing all the bits in an array, wrapping the existing
   *  array without copying.
   *
   *  @param elems the array of `Long` words representing the bits; the caller must not modify the array after this call
   *  @return a new immutable bitset backed by the given `elems` array
   */
  def fromBitMaskNoCopy(elems: Array[Long]): BitSet = {
    val len = elems.length
    if (len == 0) empty
    else if (len == 1) new BitSet1(elems(0))
    else if (len == 2) createSmall(elems(0), elems(1))
    else new BitSetN(elems)
  }

  /** An immutable bitset holding the elements `0` to `63` in a single word.
   *
   *  @param elems the word holding the bits of this bitset
   */
  @deprecated("Implementation classes of BitSet should not be accessed directly", "2.13.0")
  class BitSet1(val elems: Long) extends BitSet {
    /** Returns `1`; this bitset is stored in one word. */
    protected[collection] def nwords = 1
    /** Returns the word at index `idx`, which is this bitset's only word for `idx == 0`
     *  and `0L` beyond it.
     *
     *  @param idx the word index
     */
    protected[collection] def word(idx: Int) = if (idx == 0) elems else 0L
    /** Returns a bitset that agrees with this one except that the word at `idx` is `w`.
     *
     *  The representation widens to two words, or to an array of words, only as far as
     *  the non-zero words of the result require.
     *
     *  @param idx the index of the word to update
     *  @param w the new value for the word at index `idx`
     *  @return a bitset with the word at `idx` set to `w`
     */
    protected[collection] def updateWord(idx: Int, w: Long): BitSet =
      if (idx == 0) new BitSet1(w)
      else if (idx == 1) createSmall(elems, w)
      else this.fromBitMaskNoCopy(updateArray(Array(elems), idx, w))


    /** Returns a bitset containing the elements of this bitset that are not in `other`.
     *
     *  When `other` is itself a bitset the difference is a single bitwise operation on
     *  the words; any other set falls back to the inherited element-by-element
     *  implementation.
     *
     *  @param other the set of elements to remove
     *  @return a bitset with the elements of `other` removed
     */
    override def diff(other: collection.Set[Int]): BitSet = other match {
      case bs: collection.BitSet => bs.nwords match {
        case 0 => this
        case _ =>
          val newElems = elems & ~bs.word(0)
          if (newElems == 0L) this.empty else new BitSet1(newElems)
      }
      case _ => super.diff(other)
    }

    /** Returns a bitset of the elements of this bitset that satisfy `pred`, or that fail
     *  to satisfy it when `isFlipped` is `true`.
     *
     *  `pred` is applied to each element present, and the result is narrowed to the
     *  empty bitset when no element is left.
     *
     *  @param pred the predicate used to test elements
     *  @param isFlipped if `true`, keeps the elements that do not satisfy `pred`
     *  @return a bitset of the elements that pass the test
     */
    override def filterImpl(pred: Int => Boolean, isFlipped: Boolean): BitSet = {
      val _elems = BitSetOps.computeWordForFilter(pred, isFlipped, elems, 0)
      if (_elems == 0L) this.empty else new BitSet1(_elems)
    }
  }

  /** An immutable bitset holding the elements `0` to `127` in two words.
   *
   *  @param elems0 the word holding the bits of the elements `0` to `63`
   *  @param elems1 the word holding the bits of the elements `64` to `127`
   */
  @deprecated("Implementation classes of BitSet should not be accessed directly", "2.13.0")
  class BitSet2(val elems0: Long, val elems1: Long) extends BitSet {
    /** Returns `2`; this bitset is stored in two words. */
    protected[collection] def nwords = 2
    /** Returns the word at index `idx`, which is one of this bitset's two words for
     *  `idx == 0` or `idx == 1`, and `0L` beyond them.
     *
     *  @param idx the word index
     */
    protected[collection] def word(idx: Int) = if (idx == 0) elems0 else if (idx == 1) elems1 else 0L
    /** Returns a bitset that agrees with this one except that the word at `idx` is `w`,
     *  narrowing to a single word when the second word becomes zero and widening to an
     *  array of words when `idx` lies beyond the two words held here.
     *
     *  @param idx the index of the word to update
     *  @param w the new value for the word at index `idx`
     *  @return a bitset with the word at `idx` set to `w`
     */
    protected[collection] def updateWord(idx: Int, w: Long): BitSet =
      if (idx == 0) new BitSet2(w, elems1)
      else if (idx == 1) createSmall(elems0, w)
      else this.fromBitMaskNoCopy(updateArray(Array(elems0, elems1), idx, w))


    /** Returns a bitset containing the elements of this bitset that are not in `other`.
     *
     *  When `other` is itself a bitset the difference is a bitwise operation on the
     *  words, and the result is narrowed to one word or to the empty bitset when the
     *  higher words become zero; any other set falls back to the inherited
     *  element-by-element implementation.
     *
     *  @param other the set of elements to remove
     *  @return a bitset with the elements of `other` removed
     */
    override def diff(other: collection.Set[Int]): BitSet = other match {
      case bs: collection.BitSet => bs.nwords match {
        case 0 => this
        case 1 =>
          new BitSet2(elems0 & ~bs.word(0), elems1)
        case _ =>
          val _elems0 = elems0 & ~bs.word(0)
          val _elems1 = elems1 & ~bs.word(1)

          if (_elems1 == 0L) {
            if (_elems0 == 0L) {
              this.empty
            } else {
              new BitSet1(_elems0)
            }
          } else {
            new BitSet2(_elems0, _elems1)
          }
      }
      case _ => super.diff(other)
    }

    /** Returns a bitset of the elements of this bitset that satisfy `pred`, or that fail
     *  to satisfy it when `isFlipped` is `true`.
     *
     *  `pred` is applied to each element present, and the result is narrowed to one word
     *  or to the empty bitset when the higher words become zero.
     *
     *  @param pred the predicate used to test elements
     *  @param isFlipped if `true`, keeps the elements that do not satisfy `pred`
     *  @return a bitset of the elements that pass the test
     */
    override def filterImpl(pred: Int => Boolean, isFlipped: Boolean): BitSet = {
      val _elems0 = BitSetOps.computeWordForFilter(pred, isFlipped, elems0, 0)
      val _elems1 = BitSetOps.computeWordForFilter(pred, isFlipped, elems1, 1)

      if (_elems1 == 0L) {
        if (_elems0 == 0L) {
          this.empty
        }
        else new BitSet1(_elems0)
      }
      else new BitSet2(_elems0, _elems1)
    }
  }

  /** An immutable bitset of any size, holding its bits in an array of words.
   *
   *  @param elems the array of `Long` words holding the bits of this bitset; it is used
   *               as is, so it must not be modified afterwards
   */
  @deprecated("Implementation classes of BitSet should not be accessed directly", "2.13.0")
  class BitSetN(val elems: Array[Long]) extends BitSet {
    /** Returns the number of words this bitset is stored in. */
    protected[collection] def nwords = elems.length

    /** Returns the word at index `idx`, or `0L` if `idx` is beyond the words held here.
     *
     *  @param idx the word index
     */
    protected[collection] def word(idx: Int) = if (idx < nwords) elems(idx) else 0L

    /** Returns a bitset that agrees with this one except that the word at `idx` is `w`,
     *  growing the array if `idx` lies beyond it and narrowing the representation when
     *  the higher words become zero.
     *
     *  @param idx the index of the word to update
     *  @param w the new value for the word at index `idx`
     *  @return a bitset with the word at `idx` set to `w`
     */
    protected[collection] def updateWord(idx: Int, w: Long): BitSet = this.fromBitMaskNoCopy(updateArray(elems, idx, w))

    /** Returns a bitset containing the elements of this bitset that are not in `that`.
     *
     *  When `that` is itself a bitset the difference is computed word by word, this
     *  bitset itself is returned if no word changes, and the result is shrunk to the
     *  words it actually needs; any other set falls back to the inherited
     *  element-by-element implementation.
     *
     *  @param that the set of elements to remove
     *  @return a bitset with the elements of `that` removed
     */
    override def diff(that: collection.Set[Int]): BitSet = that match {
      case bs: collection.BitSet =>
        /*
          * Algorithm:
          *
          * We iterate, word-by-word, backwards from the shortest of the two bitsets (this, or bs) i.e. the one with
          * the fewer words. Two extra concerns for optimization are described below.
          *
          * Array Shrinking:
          * If `this` is not longer than `bs`, then since we must iterate through the full array of words,
          * we can track the new highest index word which is non-zero, at little additional cost. At the end, the new
          * Array[Long] allocated for the returned BitSet will only be of size `maxNonZeroIndex + 1`
          *
          * Tracking Changes:
          * If the two sets are disjoint, then we can return `this`. Therefor, until at least one change is detected,
          * we check each word for if it has changed from its corresponding word in `this`. Once a single change is
          * detected, we stop checking because the cost of the new Array must be paid anyways.
          */

        val bsnwords = bs.nwords
        val thisnwords = nwords
        if (bsnwords >= thisnwords) {
          // here, we may have opportunity to shrink the size of the array
          // so, track the highest index which is non-zero. That ( + 1 ) will be our new array length
          var i = thisnwords - 1
          var currentWord = 0L
          // if there are never any changes, we can return `this` at the end
          var anyChanges = false
          while (i >= 0 && currentWord == 0L) {
            val oldWord = word(i)
            currentWord = oldWord & ~bs.word(i)
            anyChanges ||= currentWord != oldWord
            i -= 1
          }
          i match {
            case -1 =>
              if (anyChanges) {
                if (currentWord == 0) {
                  this.empty
                } else {
                  new BitSet1(currentWord)
                }
              } else {
                this
              }
            case 0 =>
              val oldFirstWord = word(0)
              val firstWord = oldFirstWord & ~bs.word(0)
              anyChanges ||= firstWord != oldFirstWord
              if (anyChanges) {
                new BitSet2(firstWord, currentWord)
              } else {
                this
              }
            case _ =>
              val minimumNonZeroIndex: Int = i + 1
              while (!anyChanges && i >= 0) {
                val oldWord = word(i)
                currentWord = oldWord & ~bs.word(i)
                anyChanges ||= currentWord != oldWord
                i -= 1
              }
              if (anyChanges) {
                val newArray = elems.take(minimumNonZeroIndex + 1)
                newArray(i + 1) = currentWord
                while (i >= 0) {
                  newArray(i) = word(i) & ~bs.word(i)
                  i -= 1
                }
                new BitSetN(newArray)
              } else {
                this
              }
          }
        } else {
          var i = bsnwords - 1
          var anyChanges = false
          var currentWord = 0L
          while (i >= 0 && !anyChanges) {
            val oldWord = word(i)
            currentWord = oldWord & ~bs.word(i)
            anyChanges ||= currentWord != oldWord
            i -= 1
          }
          if (anyChanges) {
            val newElems = elems.clone()
            newElems(i + 1) = currentWord
            while (i >= 0) {
              newElems(i) = word(i) & ~bs.word(i)
              i -= 1
            }
            this.fromBitMaskNoCopy(newElems)
          } else {
            this
          }
        }
      case _ => super.diff(that)
    }


    /** Returns a bitset of the elements of this bitset that satisfy `pred`, or that fail
     *  to satisfy it when `isFlipped` is `true`.
     *
     *  `pred` is applied to each element present, this bitset itself is returned if no
     *  word changes, and the result is shrunk to the words it actually needs.
     *
     *  @param pred the predicate used to test elements
     *  @param isFlipped if `true`, keeps the elements that do not satisfy `pred`
     *  @return a bitset of the elements that pass the test
     */
    override def filterImpl(pred: Int => Boolean, isFlipped: Boolean): BitSet = {
      // here, we may have opportunity to shrink the size of the array
      // so, track the highest index which is non-zero. That ( + 1 ) will be our new array length
      var i = nwords - 1
      var currentWord = 0L
      // if there are never any changes, we can return `this` at the end
      var anyChanges = false
      while (i >= 0 && currentWord == 0L) {
        val oldWord = word(i)
        currentWord = BitSetOps.computeWordForFilter(pred, isFlipped, oldWord, i)
        anyChanges ||= currentWord != oldWord
        i -= 1
      }
      i match {
        case -1 =>
          if (anyChanges) {
            if (currentWord == 0) {
              this.empty
            } else {
              new BitSet1(currentWord)
            }
          } else {
            this
          }
        case 0 =>
          val oldFirstWord = word(0)
          val firstWord = BitSetOps.computeWordForFilter(pred, isFlipped, oldFirstWord, 0)
          anyChanges ||= firstWord != oldFirstWord
          if (anyChanges) {
            new BitSet2(firstWord, currentWord)
          } else {
            this
          }
        case _ =>
          val minimumNonZeroIndex: Int = i + 1
          while (!anyChanges && i >= 0) {
            val oldWord = word(i)
            currentWord = BitSetOps.computeWordForFilter(pred, isFlipped, oldWord, i)
            anyChanges ||= currentWord != oldWord
            i -= 1
          }
          if (anyChanges) {
            val newArray = elems.take(minimumNonZeroIndex + 1)
            newArray(i + 1) = currentWord
            while (i >= 0) {
              newArray(i) = BitSetOps.computeWordForFilter(pred, isFlipped, word(i), i)
              i -= 1
            }
            new BitSetN(newArray)
          } else {
            this
          }
      }
    }

    /** Returns the words of this bitset as a fresh array, so that changing it does not
     *  affect this bitset.
     */
    override def toBitMask: Array[Long] = elems.clone()
  }

  @SerialVersionUID(3L)
  private final class SerializationProxy(coll: BitSet) extends scala.collection.BitSet.SerializationProxy(coll) {
    /** Returns the bitset rebuilt from the words read back from the stream, replacing
     *  this proxy.
     */
    protected def readResolve(): Any = BitSet.fromBitMaskNoCopy(elems)
  }
}
