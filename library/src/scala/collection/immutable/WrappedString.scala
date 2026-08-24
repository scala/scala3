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
package immutable

import scala.language.`2.13`
import language.experimental.captureChecking

import scala.Predef.{wrapString => _, assert}
import scala.collection.Stepper.EfficientSplit
import scala.collection.convert.impl.CharStringStepper
import scala.collection.mutable.{Builder, StringBuilder}

/**
  *  This class serves as a wrapper augmenting `String`s with all the operations
  *  found in indexed sequences.
  *
  *  The difference between this class and `StringOps` is that calling transformer
  *  methods such as `filter` and `map` will yield an object of type `WrappedString`
  *  rather than a `String`.
  *
  *  @param self    a string contained within this wrapped string
  *
  *  @define Coll `WrappedString`
  *  @define coll wrapped string
  */
@SerialVersionUID(3L)
final class WrappedString(private val self: String) extends AbstractSeq[Char] with IndexedSeq[Char]
  with IndexedSeqOps[Char, IndexedSeq, WrappedString]
  with Serializable {

  /** Returns the character at index `i` of the underlying string, in constant time.
   *
   *  @param i the index of the character to return
   *  @throws StringIndexOutOfBoundsException if `i` is negative or not less than the length of this wrapped string
   */
  def apply(i: Int): Char = self.charAt(i)

  /** Returns a wrapped string holding the characters of `coll`, which is how
   *  transformation methods such as `filter` build their result.
   *
   *  @param coll the characters of the new wrapped string
   */
  override protected def fromSpecific(coll: scala.collection.IterableOnce[Char]^): WrappedString = WrappedString.fromSpecific(coll)
  /** Returns a builder that collects characters into a wrapped string, backed by a `StringBuilder`. */
  override protected def newSpecificBuilder: Builder[Char, WrappedString] = WrappedString.newBuilder
  /** Returns the shared wrapped string of length zero. */
  override def empty: WrappedString = WrappedString.empty

  /** Returns the characters of this wrapped string from `from` up to but not including
   *  `until`, wrapped in turn.
   *
   *  Both indices are clamped to the bounds of this wrapped string, so no exception is
   *  thrown for out-of-range values; the result is taken with `String.substring`.
   *
   *  @param from the index of the first character of the slice
   *  @param until the index one past the last character of the slice
   *  @return a wrapped string of the characters at the indices in the interval, empty
   *          if `until` is not greater than `from`
   */
  override def slice(from: Int, until: Int): WrappedString = {
    val start = if (from < 0) 0 else from
    if (until <= start || start >= self.length)
      return WrappedString.empty

    val end = if (until > length) length else until
    new WrappedString(self.substring(start, end))
  }
  /** Returns the number of characters in this wrapped string, in constant time. */
  override def length = self.length
  /** Returns the underlying string itself, not the usual `WrappedString(...)` rendering of a collection. */
  override def toString() = self
  /** Returns a [[StringView]] over the underlying string, which applies transformations lazily. */
  override def view: StringView = new StringView(self)

  /** Returns a stepper over the characters of the underlying string, without boxing
   *  them when a `CharStepper` is asked for.
   *
   *  The stepper is efficiently splittable, since the underlying string can be
   *  divided by index. Surrogate pairs are stepped over as their two separate `Char`
   *  values.
   *
   *  @tparam S the type of the resulting stepper
   *  @param shape the shape of the stepper, which must be `CharShape` or `ReferenceShape`
   *  @return a stepper of shape `S` over the characters of this wrapped string
   */
  override def stepper[S <: Stepper[?]](implicit shape: StepperShape[Char, S]): S & EfficientSplit = {
    val st = new CharStringStepper(self, 0, self.length)
    val r =
      if (shape.shape == StepperShape.CharShape) st
      else {
        assert(shape.shape == StepperShape.ReferenceShape, s"unexpected StepperShape: $shape")
        AnyStepper.ofParIntStepper(st)
      }
    r.asInstanceOf[S & EfficientSplit]
  }

  /** Returns `true` if this wrapped string contains the elements of `that` starting at
   *  index `offset`.
   *
   *  When `that` is itself a wrapped string the test is delegated to
   *  `String.startsWith`; otherwise the elements are compared one by one by the
   *  inherited implementation.
   *
   *  @tparam B the element type of `that`
   *  @param that the collection to test for
   *  @param offset the index of this wrapped string at which to start the comparison
   *  @return `true` if `that` is a prefix of this wrapped string from `offset` onwards,
   *          `false` in particular if `offset` is negative or past the end
   */
  override def startsWith[B >: Char](that: IterableOnce[B]^, offset: Int = 0): Boolean =
    that match {
      case s: WrappedString => self.startsWith(s.self, offset)
      case _                => super.startsWith(that, offset)
    }

  /** Returns `true` if this wrapped string ends with the elements of `that`.
   *
   *  When `that` is itself a wrapped string the test is delegated to
   *  `String.endsWith`; otherwise the elements are compared one by one by the
   *  inherited implementation.
   *
   *  @tparam B the element type of `that`
   *  @param that the collection to test for
   *  @return `true` if `that` is a suffix of this wrapped string
   */
  override def endsWith[B >: Char](that: collection.Iterable[B]^): Boolean =
    that match {
      case s: WrappedString => self.endsWith(s.self)
      case _                => super.endsWith(that)
    }

  /** Returns the index of the first occurrence of `elem` at or after `from`, or `-1` if
   *  there is none.
   *
   *  When `elem` is a `Char` the search is delegated to `String.indexOf`, which treats
   *  a negative `from` as `0`; any other value can never occur in this wrapped string
   *  and is looked for by the inherited implementation.
   *
   *  @tparam B the type of `elem`
   *  @param elem the element to look for
   *  @param from the index at which to start the search
   *  @return the index of the first occurrence of `elem` at or after `from`, or `-1`
   */
  override def indexOf[B >: Char](elem: B, from: Int = 0): Int = elem match {
    case c: Char => self.indexOf(c, from)
    case _       => super.indexOf(elem, from)
  }

  /** Returns the index of the last occurrence of `elem` at or before `end`, or `-1` if
   *  there is none.
   *
   *  When `elem` is a `Char` the search is delegated to `String.lastIndexOf`, which
   *  finds nothing for a negative `end`; any other value can never occur in this
   *  wrapped string and is looked for by the inherited implementation.
   *
   *  @tparam B the type of `elem`
   *  @param elem the element to look for
   *  @param end the index at which to end the backwards search, the last index by default
   *  @return the index of the last occurrence of `elem` at or before `end`, or `-1`
   */
  override def lastIndexOf[B >: Char](elem: B, end: Int = length - 1): Int =
    elem match {
      case c: Char => self.lastIndexOf(c, end)
      case _       => super.lastIndexOf(elem, end)
    }

  /** Copies characters of this wrapped string to another array, beginning at index
   *  `start` of `xs`.
   *
   *  The number of elements copied is the minimum of `len`, the length of this wrapped
   *  string, and the remaining capacity of `xs` from `start`; if that minimum is not
   *  positive, nothing is copied. When `xs` is an `Array[Char]` the copy is performed
   *  by a single `String.getChars`; otherwise the inherited implementation copies the
   *  characters one by one, boxing them.
   *
   *  @tparam B the element type of the destination array, a supertype of `Char`
   *  @param xs the destination array
   *  @param start the index of `xs` at which to write the first character
   *  @param len the maximum number of characters to copy
   *  @return the number of characters actually copied
   */
  override def copyToArray[B >: Char](xs: Array[B], start: Int, len: Int): Int =
    (xs: Any) match {
      case chs: Array[Char] =>
        val copied = IterableOnce.elemsToCopyToArray(length, chs.length, start, len)
        self.getChars(0, copied, chs, start)
        copied
      case _                => super.copyToArray(xs, start, len)
    }

  /** Returns a sequence consisting of the characters of this wrapped string followed by
   *  the elements of `suffix`.
   *
   *  When `suffix` is itself a wrapped string the result is another `WrappedString`
   *  built by `String.concat`; otherwise the inherited implementation fills a builder
   *  and the result is a [[Vector]] of the common element type.
   *
   *  @tparam B the element type of the returned sequence
   *  @param suffix the collection to append
   *  @return a sequence with the elements of `suffix` appended to the characters of
   *          this wrapped string
   */
  override def appendedAll[B >: Char](suffix: IterableOnce[B]^): IndexedSeq[B] =
    suffix match {
      case s: WrappedString => new WrappedString(self.concat(s.self))
      case _                => super.appendedAll(suffix)
    }

  /** Returns `true` if this wrapped string and `o` contain the same characters in the
   *  same order.
   *
   *  When `o` is itself a wrapped string the two underlying strings are compared
   *  directly; otherwise the inherited element-by-element comparison is used.
   *
   *  @tparam B the element type of `o`
   *  @param o the collection to compare against
   */
  override def sameElements[B >: Char](o: IterableOnce[B]^) = o match {
    case s: WrappedString => self == s.self
    case _                => super.sameElements(o)
  }

  /** The name of this collection class, `"WrappedString"`, used as the prefix in the
   *  string representation of collections derived from it.
   */
  override protected def className = "WrappedString"

  /** Returns `Int.MaxValue`: indexed access into a string is always cheaper than
   *  iterating, so `apply` is preferred at every length.
   */
  override protected final def applyPreferredMaxLength: Int = Int.MaxValue
  /** Returns `true` if `other` is a sequence holding the same characters in the same
   *  order.
   *
   *  Two wrapped strings are compared by their underlying strings; against any other
   *  value the inherited sequence equality is used, so a wrapped string can equal
   *  another immutable `Seq[Char]` but never a `String`.
   *
   *  @param other the value to compare against
   */
  override def equals(other: Any): Boolean = other match {
    case that: WrappedString =>
      this.self == that.self
    case _ =>
      super.equals(other)
  }
}

/** A companion object for wrapped strings.
  */
@SerialVersionUID(3L)
object WrappedString extends SpecificIterableFactory[Char, WrappedString] {
  /** Returns a wrapped string holding the characters of `it`, in the order `it` gives
   *  them out.
   *
   *  @param it the characters of the new wrapped string
   */
  def fromSpecific(it: IterableOnce[Char]^): WrappedString = {
    val b = newBuilder
    b.sizeHint(it)
    b ++= it
    b.result()
  }
  /** The wrapped string of length zero, shared by every call to `empty`. */
  val empty: WrappedString = new WrappedString("")
  /** Returns a new builder that collects characters into a wrapped string, using a
   *  `StringBuilder` to accumulate them.
   */
  def newBuilder: Builder[Char, WrappedString] =
    new StringBuilder().mapResult(x => new WrappedString(x))

  /** Provides the `unwrap` method on wrapped strings, which recovers the underlying
   *  `String`.
   *
   *  @param value the wrapped string to unwrap
   */
  implicit class UnwrapOp(private val value: WrappedString) extends AnyVal {
    /** Returns the underlying string, without copying it. */
    def unwrap: String = value.self
  }
}
