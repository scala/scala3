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

/** Base trait for immutable sequences: ordered collections whose elements can be
 *  reached by their position and which are guaranteed never to change.
 *
 *  @tparam A the element type of the sequence
 */
trait Seq[+A] extends Iterable[A]
                 with collection.Seq[A]
                 with SeqOps[A, Seq, Seq[A]]
                 with IterableFactoryDefaults[A, Seq] {

  /** Returns this sequence itself; being already an immutable `Seq`, no copy is made. */
  override final def toSeq: this.type = this

  /** The factory used to build immutable sequences, the [[Seq$ `Seq`]] companion object, which delegates to [[List]]. */
  override def iterableFactory: SeqFactory[Seq] = Seq
}

/**
 *  @define coll immutable sequence
 *  @define Coll `immutable.Seq`
 *
 *  @tparam A the element type of the sequence
 *  @tparam CC the type constructor for the collection type, constrained to be pure
 *  @tparam C the concrete type of this sequence
 */
transparent trait SeqOps[+A, +CC[B] <: caps.Pure, +C] extends Any with collection.SeqOps[A, CC, C] with caps.Pure

/** $factoryInfo
 *  @define coll immutable sequence
 *  @define Coll `immutable.Seq`
 */
@SerialVersionUID(3L)
object Seq extends SeqFactory.Delegate[Seq](List) {
  /** Returns an immutable sequence containing the elements of `it`.
   *
   *  If `it` is already an immutable `Seq` it is returned unchanged; otherwise its
   *  elements are copied into a new [[List]].
   *
   *  @tparam E the element type
   *  @param it the collection whose elements are to be contained
   */
  override def from[E](it: IterableOnce[E]^): Seq[E] = it match {
    case s: Seq[E @unchecked] => s
    case _ => super.from(it)
  }
}

/** Base trait for immutable indexed sequences that have efficient `apply` and `length`.
 *
 *  @tparam A the element type of the indexed sequence
 */
trait IndexedSeq[+A] extends Seq[A]
                        with collection.IndexedSeq[A]
                        with IndexedSeqOps[A, IndexedSeq, IndexedSeq[A]]
                        with IterableFactoryDefaults[A, IndexedSeq] {

  /** Returns this sequence itself; being already an immutable `IndexedSeq`, no copy is made. */
  final override def toIndexedSeq: IndexedSeq[A] = this

  /** Returns `true` if this sequence can possibly equal `that`.
   *
   *  Two indexed sequences of different length can never be equal, so the length
   *  is compared first when `that` is itself an `IndexedSeq`; otherwise the
   *  inherited test is used.
   *
   *  @param that the value being compared for possible equality
   *  @return `true` if `that` is not an `IndexedSeq` of a different length and the
   *          inherited test also succeeds
   */
  override def canEqual(that: Any): Boolean = that match {
    case otherIndexedSeq: IndexedSeq[?] => length == otherIndexedSeq.length && super.canEqual(that)
    case _ => super.canEqual(that)
  }


  /** Returns `true` if this sequence and `o` contain the same elements in the same order.
   *
   *  When `o` is another `IndexedSeq`, the two are known to be equal at once if they
   *  are the same object, and known to differ at once if their lengths differ.
   *  Otherwise the elements are compared by index for as long as indexed access is
   *  the cheaper of the two (see `applyPreferredMaxLength`) and by iterator
   *  thereafter. For any other collection the inherited iterator-based comparison
   *  is used.
   *
   *  @tparam B the element type of `o`
   *  @param o the collection to compare against
   *  @return `true` if both sequences have the same length and their corresponding
   *          elements are equal
   */
  override def sameElements[B >: A](o: IterableOnce[B]^): Boolean = o match {
    case that: IndexedSeq[?] =>
      (this eq that) || {
        val length = this.length
        var equal = length == that.length
        if (equal) {
          var index = 0
          // some IndexedSeq apply is less efficient than using Iterators
          // e.g. Vector so we can compare the first few with apply and the rest with an iterator
          // but if apply is more efficient than Iterators then we can use the apply for all the comparison
          // we default to the minimum preferred length
          val maxApplyCompare = {
            val preferredLength = Math.min(applyPreferredMaxLength, that.applyPreferredMaxLength)
            if (length > (preferredLength.toLong << 1)) preferredLength else length
          }
          while (index < maxApplyCompare && equal) {
            equal = this (index) == that(index)
            index += 1
          }
          if ((index < length) && equal) {
            val thisIt = this.iterator.drop(index)
            val thatIt = that.iterator.drop(index)
            while (equal && thisIt.hasNext) {
              equal = thisIt.next() == thatIt.next()
            }
          }
        }
        equal
      }
    case _ => super.sameElements(o)
  }

  /** a hint to the runtime when scanning values
   *  [[apply]] is preferred for scan with a max index less than this value
   *  [[iterator]] is preferred for scans above this range
   *  @return the maximum length below which [[apply]] is preferred over [[iterator]] for element access
   */
  protected def applyPreferredMaxLength: Int = IndexedSeqDefaults.defaultApplyPreferredMaxLength

  /** The factory used to build immutable indexed sequences, the [[IndexedSeq$ `IndexedSeq`]] companion object, which delegates to [[Vector]]. */
  override def iterableFactory: SeqFactory[IndexedSeq] = IndexedSeq
}

object IndexedSeqDefaults {
  /** The default value of `IndexedSeq.applyPreferredMaxLength`, the length below
   *  which indexed access is preferred to iteration when scanning an immutable
   *  indexed sequence.
   *
   *  It is read from the system property
   *  `scala.collection.immutable.IndexedSeq.defaultApplyPreferredMaxLength` and is
   *  `64` when that property is unset or cannot be read.
   */
  val defaultApplyPreferredMaxLength: Int =
    try System.getProperty(
      "scala.collection.immutable.IndexedSeq.defaultApplyPreferredMaxLength", "64").toInt
    catch {
      case _: SecurityException => 64
    }
}

@SerialVersionUID(3L)
object IndexedSeq extends SeqFactory.Delegate[IndexedSeq](Vector) {
  /** Returns an immutable indexed sequence containing the elements of `it`.
   *
   *  If `it` is already an immutable `IndexedSeq` it is returned unchanged; otherwise
   *  its elements are copied into a new [[Vector]].
   *
   *  @tparam E the element type
   *  @param it the collection whose elements are to be contained
   */
  override def from[E](it: IterableOnce[E]^): IndexedSeq[E] = it match {
    case is: IndexedSeq[E @unchecked] => is
    case _ => super.from(it)
  }
}

/** Base trait for immutable indexed `Seq` operations.
 *
 *  @tparam A the element type of the indexed sequence
 *  @tparam CC the type constructor for the collection type, constrained to be pure
 *  @tparam C the concrete type of this indexed sequence
 */
transparent trait IndexedSeqOps[+A, +CC[B] <: caps.Pure, +C]
  extends SeqOps[A, CC, C]
    with collection.IndexedSeqOps[A, CC, C] {

  /** Returns the elements of this sequence from `from` up to but not including `until`.
   *
   *  Indices outside the bounds of this sequence are clamped, so a slice that covers
   *  the whole sequence returns this sequence itself rather than a copy.
   *
   *  @param from the index of the first element of the slice
   *  @param until the index one past the last element of the slice
   *  @return a sequence of the elements at the indices in the interval, empty if
   *          `until` is not greater than `from`
   */
  override def slice(from: Int, until: Int): C = {
    // since we are immutable we can just share the same collection
    if (from <= 0 && until >= length) coll
    else super.slice(from, until)
  }

}

/** Base trait for immutable linear sequences that have efficient `head` and `tail`.
 *
 *  @tparam A the element type of the linear sequence
 */
trait LinearSeq[+A]
  extends Seq[A]
    with collection.LinearSeq[A]
    with LinearSeqOps[A, LinearSeq, LinearSeq[A]]
    with IterableFactoryDefaults[A, LinearSeq] {

  /** The factory used to build immutable linear sequences, the [[LinearSeq$ `LinearSeq`]] companion object, which delegates to [[List]]. */
  override def iterableFactory: SeqFactory[LinearSeq] = LinearSeq
}

@SerialVersionUID(3L)
object LinearSeq extends SeqFactory.Delegate[LinearSeq](List) {
  /** Returns an immutable linear sequence containing the elements of `it`.
   *
   *  If `it` is already an immutable `LinearSeq` it is returned unchanged; otherwise
   *  its elements are copied into a new [[List]].
   *
   *  @tparam E the element type
   *  @param it the collection whose elements are to be contained
   */
  override def from[E](it: IterableOnce[E]^): LinearSeq[E] = it match {
    case ls: LinearSeq[E @unchecked] => ls
    case _ => super.from(it)
  }
}

transparent trait LinearSeqOps[+A, +CC[X] <: LinearSeq[X], +C <: LinearSeq[A] & LinearSeqOps[A, CC, C]]
  extends Any with SeqOps[A, CC, C]
    with collection.LinearSeqOps[A, CC, C]

/** Explicit instantiation of the `Seq` trait to reduce class file size in subclasses.
 *
 *  @tparam A the element type of the sequence
 */
abstract class AbstractSeq[+A] extends scala.collection.AbstractSeq[A] with Seq[A]
