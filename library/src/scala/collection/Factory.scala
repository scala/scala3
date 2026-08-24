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

import scala.collection.immutable.NumericRange
import scala.language.implicitConversions
import scala.collection.mutable.Builder
import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.ClassTag

/** A factory that builds a collection of type `C` with elements of type `A`.
 *
 *  This is a general form of any factory ([[IterableFactory]],
 *  [[SortedIterableFactory]], [[MapFactory]] and [[SortedMapFactory]]) whose
 *  element type is fixed.
 *
 *  @tparam A Type of elements (e.g. `Int`, `Boolean`, etc.)
 *  @tparam C Type of collection (e.g. `List[Int]`, `TreeMap[Int, String]`, etc.)
 */
into trait Factory[-A, +C] extends Any { self: Factory[A, C] =>

  /**
   *  @param it the source of elements to include in the collection
   *  @return a collection of type `C` containing the elements from `it`
   */
  def fromSpecific(it: IterableOnce[A]^): C^{it}

  /** Gets a Builder for the collection. For non-strict collection types this will use an intermediate buffer.
   *  Building collections with `fromSpecific` is preferred because it can be lazy for lazy collections.
   */
  def newBuilder: Builder[A, C]
}

object Factory {

  /** A [[Factory]] that builds a `String` from `Char` elements. */
  implicit val stringFactory: Factory[Char, String] = new StringFactory
  @SerialVersionUID(3L)
  private class StringFactory extends Factory[Char, String] with Serializable {
    /** Returns a `String` containing the characters of `it`, in iteration order.
     *
     *  @param it the characters of the resulting string
     */
    def fromSpecific(it: IterableOnce[Char]^): String = {
      val b = new mutable.StringBuilder(scala.math.max(0, it.knownSize))
      b ++= it
      b.result()
    }
    /** Returns a new empty [[scala.collection.mutable.StringBuilder]] for building a `String` from characters. */
    def newBuilder: Builder[Char, String] = new mutable.StringBuilder()
  }

  /** A [[Factory]] that builds an `Array` from its elements.
   *
   *  @tparam A the element type of the array; a `ClassTag` for it must be available
   *            so that a properly typed array can be allocated
   *  @return a `Factory` that builds an `Array[A]`
   */
  implicit def arrayFactory[A: ClassTag]: Factory[A, Array[A]] = new ArrayFactory[A]
  @SerialVersionUID(3L)
  private class ArrayFactory[A: ClassTag] extends Factory[A, Array[A]] with Serializable {
    /** Returns an `Array` containing the elements of `it`, in iteration order.
     *
     *  @param it the elements of the resulting array
     */
    def fromSpecific(it: IterableOnce[A]^): Array[A] = {
      val b = newBuilder
      b.sizeHint(it, delta = 0)
      b ++= it
      b.result()
    }
    /** Returns a new empty [[scala.collection.mutable.ArrayBuilder]] for building an `Array[A]`. */
    def newBuilder: Builder[A, Array[A]] = mutable.ArrayBuilder.make[A]
  }

  given IArrayFactory[A: ClassTag]: Factory[A, IArray[A]] = {
    @SerialVersionUID(3L)
    class ConcreteIArrayFactory[A: ClassTag] extends Factory[A, IArray[A]] with Serializable {
      /** Returns an `IArray` containing the elements of `it`, in iteration order.
       *
       *  @param it the elements of the resulting immutable array
       */
      def fromSpecific(it: IterableOnce[A]^): IArray[A] = IArray.from(it)
      /** Returns a new empty builder for building an `IArray[A]`. */
      def newBuilder: Builder[A, IArray[A]] = IArray.newBuilder[A]
    }
    ConcreteIArrayFactory[A]
  }

}

/** Base trait for companion objects of unconstrained collection types that may require
 *  multiple traversals of a source collection to build a target collection `CC`.
 *
 *  @tparam CC Collection type constructor (e.g. `List`)
 *  @define factoryInfo
 *   This object provides a set of operations to create $Coll values.
 *
 *  @define coll collection
 *  @define Coll `Iterable`
 */
trait IterableFactory[+CC[_]] extends Serializable, caps.Pure {

  /** Creates a target $coll from an existing source collection
   *
   *  @tparam A the type of the collection’s elements
   *  @param source Source collection
   *  @return a new $coll with the elements of `source`
   */
  def from[A](source: IterableOnce[A]^): CC[A]^{source}

  /** An empty $coll.
   *
   *  @tparam A      the type of the ${coll}'s elements
   *  @return an empty $coll of type `CC[A]`
   */
  def empty[A]: CC[A]

  /** Creates a $coll with the specified elements.
   *  @tparam A     the type of the ${coll}'s elements
   *  @param elems  the elements of the created $coll
   *  @return a new $coll with elements `elems`
   */
  def apply[A](elems: A*): CC[A] = from(elems)

  /** Produces a $coll containing repeated applications of a function to a start value.
   *
   *  @tparam A the element type of the $coll
   *  @param start the start value of the $coll
   *  @param len   the number of elements contained in the $coll
   *  @param f     the function that's repeatedly applied
   *  @return      a $coll with `len` values in the sequence `start, f(start), f(f(start)), ...`
   */
  def iterate[A](start: A, len: Int)(f: A => A): CC[A]^{f} = from(new View.Iterate(start, len)(f))

  /** Produces a $coll that uses a function `f` to produce elements of type `A`
   *  and update an internal state of type `S`.
   *
   *  @tparam A   Type of the elements
   *  @tparam S   Type of the internal state
   *  @param init State initial value
   *  @param f    Computes the next element (or returns `None` to signal
   *             the end of the collection)
   *  @return a $coll that produces elements using `f` until `f` returns `None`
   */
  def unfold[A, S](init: S)(f: S => Option[(A, S)]): CC[A]^{f} = from(new View.Unfold(init)(f))

  /** Produces a $coll containing a sequence of increasing of integers.
   *
   *  @tparam A the element type of the $coll, which must have an `Integral` instance
   *  @param start the first element of the $coll
   *  @param end   the end value of the $coll (the first value NOT contained)
   *  @return  a $coll with values `start, start + 1, ..., end - 1`
   */
  def range[A : Integral](start: A, end: A): CC[A] = from(NumericRange(start, end, implicitly[Integral[A]].one))

  /** Produces a $coll containing equally spaced values in some integer interval.
   *
   *  @tparam A the element type of the $coll, which must have an `Integral` instance
   *  @param start the start value of the $coll
   *  @param end   the end value of the $coll (the first value NOT contained)
   *  @param step  the difference between successive elements of the $coll (must be positive or negative)
   *  @return      a $coll with values `start, start + step, ...` up to, but excluding `end`
   */
  def range[A : Integral](start: A, end: A, step: A): CC[A] = from(NumericRange(start, end, step))

  /**
   *  @tparam A the type of the ${coll}’s elements
   *  @return a builder for $Coll objects.
   */
  def newBuilder[A]: Builder[A, CC[A]]

  /** Produces a $coll containing the results of some element computation a number of times.
   *
   *  @tparam A the element type of the $coll
   *  @param   n  the number of elements contained in the $coll.
   *  @param   elem the element computation
   *  @return  A $coll that contains the results of `n` evaluations of `elem`.
   */
  def fill[A](n: Int)(elem: => A): CC[A]^{elem} = from(new View.Fill(n)(elem))

  /** Produces a two-dimensional $coll containing the results of some element computation a number of times.
   *
   *  @tparam A the element type of the $coll
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   elem the element computation
   *  @return  A $coll that contains the results of `n1 x n2` evaluations of `elem`.
   */
  def fill[A](n1: Int, n2: Int)(elem: => A): CC[(CC[A]^{elem}) @uncheckedVariance]^{elem} = fill(n1)(fill(n2)(elem))

  /** Produces a three-dimensional $coll containing the results of some element computation a number of times.
   *
   *  @tparam A the element type of the $coll
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3rd dimension
   *  @param   elem the element computation
   *  @return  A $coll that contains the results of `n1 x n2 x n3` evaluations of `elem`.
   */
  def fill[A](n1: Int, n2: Int, n3: Int)(elem: => A): CC[(CC[CC[A]^{elem}]^{elem}) @uncheckedVariance]^{elem} = fill(n1)(fill(n2, n3)(elem))

  /** Produces a four-dimensional $coll containing the results of some element computation a number of times.
   *
   *  @tparam A the element type of the $coll
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3rd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   elem the element computation
   *  @return  A $coll that contains the results of `n1 x n2 x n3 x n4` evaluations of `elem`.
   */
  def fill[A](n1: Int, n2: Int, n3: Int, n4: Int)(elem: => A): CC[(CC[CC[CC[A]^{elem}]^{elem}]^{elem}) @uncheckedVariance]^{elem} =
    fill(n1)(fill(n2, n3, n4)(elem))

  /** Produces a five-dimensional $coll containing the results of some element computation a number of times.
   *
   *  @tparam A the element type of the $coll
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3rd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   n5  the number of elements in the 5th dimension
   *  @param   elem the element computation
   *  @return  A $coll that contains the results of `n1 x n2 x n3 x n4 x n5` evaluations of `elem`.
   */
  def fill[A](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(elem: => A): CC[(CC[CC[CC[CC[A]^{elem}]^{elem}]^{elem}]^{elem}) @uncheckedVariance]^{elem} =
    fill(n1)(fill(n2, n3, n4, n5)(elem))

  /** Produces a $coll containing values of a given function over a range of integer values starting from 0.
   *
   *  @tparam A the element type of the $coll
   *  @param  n   The number of elements in the $coll
   *  @param  f   The function computing element values
   *  @return A $coll consisting of elements `f(0), ..., f(n -1)`
   */
  def tabulate[A](n: Int)(f: Int => A): CC[A]^{f} = from(new View.Tabulate(n)(f))

  /** Produces a two-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
   *
   *  @tparam A the element type of the $coll
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   f   The function computing element values
   *  @return A $coll consisting of elements `f(i1, i2)`
   *          for `0 <= i1 < n1` and `0 <= i2 < n2`.
   */
  def tabulate[A](n1: Int, n2: Int)(f: (Int, Int) => A): CC[(CC[A]^{f}) @uncheckedVariance]^{f} =
    tabulate(n1)(i1 => tabulate(n2)(f(i1, _)))

  /** Produces a three-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
   *
   *  @tparam A the element type of the $coll
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3rd dimension
   *  @param   f   The function computing element values
   *  @return A $coll consisting of elements `f(i1, i2, i3)`
   *          for `0 <= i1 < n1`, `0 <= i2 < n2`, and `0 <= i3 < n3`.
   */
  def tabulate[A](n1: Int, n2: Int, n3: Int)(f: (Int, Int, Int) => A): CC[(CC[CC[A]^{f}]^{f}) @uncheckedVariance]^{f} =
    tabulate(n1)(i1 => tabulate(n2, n3)(f(i1, _, _)))

  /** Produces a four-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
   *
   *  @tparam A the element type of the $coll
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3rd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   f   The function computing element values
   *  @return A $coll consisting of elements `f(i1, i2, i3, i4)`
   *          for `0 <= i1 < n1`, `0 <= i2 < n2`, `0 <= i3 < n3`, and `0 <= i4 < n4`.
   */
  def tabulate[A](n1: Int, n2: Int, n3: Int, n4: Int)(f: (Int, Int, Int, Int) => A): CC[(CC[CC[CC[A]^{f}]^{f}]^{f}) @uncheckedVariance]^{f} =
    tabulate(n1)(i1 => tabulate(n2, n3, n4)(f(i1, _, _, _)))

  /** Produces a five-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
   *
   *  @tparam A the element type of the $coll
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3rd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   n5  the number of elements in the 5th dimension
   *  @param   f   The function computing element values
   *  @return A $coll consisting of elements `f(i1, i2, i3, i4, i5)`
   *          for `0 <= i1 < n1`, `0 <= i2 < n2`, `0 <= i3 < n3`, `0 <= i4 < n4`, and `0 <= i5 < n5`.
   */
  def tabulate[A](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(f: (Int, Int, Int, Int, Int) => A): CC[(CC[CC[CC[CC[A]^{f}]^{f}]^{f}]^{f}) @uncheckedVariance]^{f} =
    tabulate(n1)(i1 => tabulate(n2, n3, n4, n5)(f(i1, _, _, _, _)))

  /** Concatenates all argument collections into a single $coll.
   *
   *  @tparam A the element type of the $coll
   *  @param xss the collections that are to be concatenated.
   *  @return the concatenation of all the collections.
   */
  def concat[A](xss: Iterable[A]*): CC[A] = {
    from(xss.foldLeft(View.empty[A])(_ ++ _))
  }

  /** A [[Factory]] view of this factory, with the element type fixed to `A`.
   *
   *  Allows this factory to be used wherever a `Factory[A, CC[A]]` is expected,
   *  for example as the argument of `to` (`xs.to(List)`).
   *
   *  @tparam A the type of the ${coll}'s elements
   *  @return a [[Factory]] that delegates to this factory to build a `CC[A]`
   */
  implicit def iterableFactory[A]: Factory[A, CC[A]] = IterableFactory.toFactory(this)
}

object IterableFactory {

  /** Fixes the element type of `factory` to `A`.
   *  @tparam A Type of elements
   *  @tparam CC Collection type constructor of the factory (e.g. `Seq`, `List`)
   *  @param factory The factory to fix the element type
   *  @return A [[Factory]] that uses the given `factory` to build a collection of elements
   *         of type `A`
   */
  implicit def toFactory[A, CC[_]](factory: IterableFactory[CC]): Factory[A, CC[A]] = new ToFactory[A, CC](factory)

  @SerialVersionUID(3L)
  private class ToFactory[A, CC[_]](factory: IterableFactory[CC]) extends Factory[A, CC[A]] with Serializable {
    /** Returns a collection of type `CC[A]` containing the elements of `it`, built with `factory`.
     *
     *  @param it the source of elements
     */
    def fromSpecific(it: IterableOnce[A]^): CC[A]^{it} = factory.from[A](it)
    /** Returns a new builder for a `CC[A]`, obtained from `factory`. */
    def newBuilder: Builder[A, CC[A]] = factory.newBuilder[A]
  }

  /** Fixes the element type of `factory` to `A` and adapts it to the [[BuildFrom]] typeclass.
   *
   *  The resulting instance ignores its source collection (its `From` type is `Any`)
   *  and always builds with the given `factory`.
   *
   *  @tparam A Type of elements
   *  @tparam CC Collection type constructor of the factory (e.g. `Seq`, `List`)
   *  @param factory The factory to adapt
   *  @return A [[BuildFrom]] that uses the given `factory` to build a collection of
   *         elements of type `A`, regardless of the source collection
   */
  implicit def toBuildFrom[A, CC[_]](factory: IterableFactory[CC]): BuildFrom[Any, A, CC[A]] =
    new BuildFrom[Any, A, CC[A]] {
      def fromSpecific(from: Any)(it: IterableOnce[A]^) = factory.from(it)
      def newBuilder(from: Any) = factory.newBuilder
    }

  /** An `IterableFactory` that forwards all operations to another factory.
   *
   *  Useful for defining a collection companion object as a delegate to an existing
   *  factory, e.g. `object Iterable extends IterableFactory.Delegate[Iterable](immutable.Iterable)`.
   *
   *  @tparam CC Collection type constructor of both this factory and the underlying factory
   *  @param delegate The factory that all operations are forwarded to
   */
  @SerialVersionUID(3L)
  class Delegate[CC[_]](delegate: IterableFactory[CC]) extends IterableFactory[CC] {
    /** Creates a collection of type `CC[A]` with the specified elements, by forwarding to `delegate`.
     *
     *  @tparam A the type of the collection's elements
     *  @param elems the elements of the created collection
     *  @return a new `CC[A]` with elements `elems`
     */
    override def apply[A](elems: A*): CC[A] = delegate.apply(elems*)
    /** An empty collection of type `CC[A]`, obtained from `delegate`.
     *
     *  @tparam A the type of the collection's elements
     *  @return an empty `CC[A]`
     */
    def empty[A]: CC[A] = delegate.empty
    /** Creates a collection of type `CC[E]` from the elements of `it`, by forwarding to `delegate`.
     *
     *  @tparam E the type of the collection's elements
     *  @param it the source collection
     *  @return a new `CC[E]` with the elements of `it`
     */
    def from[E](it: IterableOnce[E]^): CC[E]^{it} = delegate.from(it)
    /** Returns a new builder for a `CC[A]`, obtained from `delegate`.
     *
     *  @tparam A the type of the collection's elements
     */
    def newBuilder[A]: Builder[A, CC[A]] = delegate.newBuilder[A]
  }
}

/**
 *  @tparam CC Collection type constructor (e.g. `List`)
 */
trait SeqFactory[+CC[A] <: SeqOps[A, Seq, Seq[A]] & caps.Pure] extends IterableFactory[CC] {
  import SeqFactory.UnapplySeqWrapper
  /** An extractor for sequence patterns, e.g. `case Seq(a, b, rest*) => ...`.
   *
   *  The extraction itself never fails: the returned wrapper always reports
   *  `isEmpty == false`, and the pattern matcher checks the pattern's arity
   *  against the sequence via the wrapper's `lengthCompare`.
   *
   *  @tparam A the type of the sequence's elements
   *  @param x the sequence to extract elements from
   *  @return a [[SeqFactory.UnapplySeqWrapper]] exposing the elements of `x`
   */
  final def unapplySeq[A](x: CC[A] @uncheckedVariance): UnapplySeqWrapper[A] = new UnapplySeqWrapper(x) // TODO is uncheckedVariance sound here?
}

object SeqFactory {
  /** A `SeqFactory` that forwards all operations to another factory.
   *
   *  @tparam CC Collection type constructor of both this factory and the underlying factory
   *  @param delegate The factory that all operations are forwarded to
   */
  @SerialVersionUID(3L)
  class Delegate[CC[A] <: SeqOps[A, Seq, Seq[A]] & caps.Pure](delegate: SeqFactory[CC]) extends SeqFactory[CC] {
    /** Creates a collection of type `CC[A]` with the specified elements, by forwarding to `delegate`.
     *
     *  @tparam A the type of the collection's elements
     *  @param elems the elements of the created collection
     *  @return a new `CC[A]` with elements `elems`
     */
    override def apply[A](elems: A*): CC[A] = delegate.apply(elems*)
    /** An empty collection of type `CC[A]`, obtained from `delegate`.
     *
     *  @tparam A the type of the collection's elements
     *  @return an empty `CC[A]`
     */
    def empty[A]: CC[A] = delegate.empty
    /** Creates a collection of type `CC[E]` from the elements of `it`, by forwarding to `delegate`.
     *
     *  @tparam E the type of the collection's elements
     *  @param it the source collection
     *  @return a new `CC[E]` with the elements of `it`
     */
    def from[E](it: IterableOnce[E]^): CC[E] = delegate.from(it)
    /** Returns a new builder for a `CC[A]`, obtained from `delegate`.
     *
     *  @tparam A the type of the collection's elements
     */
    def newBuilder[A]: Builder[A, CC[A]] = delegate.newBuilder[A]
  }

  /** The wrapper returned by `unapplySeq`, exposing the matched sequence's elements
   *  to the pattern matcher.
   *
   *  This is a name-based extractor result: the pattern matcher calls `isEmpty`,
   *  `get`, `lengthCompare`, `apply`, `drop` and `toSeq` as needed, without
   *  allocating an intermediate `Option` or collection.
   *
   *  @tparam A the type of the sequence's elements
   *  @param c the matched sequence
   */
  final class UnapplySeqWrapper[A](private val c: SeqOps[A, Seq, Seq[A]]) extends AnyVal {
    /** Always `false`: the extraction itself never fails (the pattern's arity is checked via `lengthCompare`). */
    def isEmpty: false = false
    /** Returns this wrapper itself, whose members give the pattern matcher access to the matched elements. */
    def get: UnapplySeqWrapper[A] = this
    /** Compares the length of the matched sequence to a test value.
     *
     *  @param len the test value
     *  @return a negative value if the sequence is shorter than `len`, zero if it
     *          contains exactly `len` elements, and a positive value if it is longer
     */
    def lengthCompare(len: Int): Int = c.lengthCompare(len)
    /** Returns the element of the matched sequence at index `i`.
     *
     *  @param i the index, starting from 0
     */
    def apply(i: Int): A = c(i)
    /** Returns all elements of the matched sequence except the first `n`, used to
     *  bind a trailing varargs sub-pattern such as `rest*`.
     *
     *  @param n the number of leading elements to skip
     *  @return the remaining elements as a `Seq`: if the matched sequence is a
     *          `scala.Seq` its own `drop` is used, otherwise the remaining
     *          elements are copied to a new `Seq` via a view
     */
    def drop(n: Int): scala.Seq[A] = c match {
      case seq: scala.Seq[A @unchecked] => seq.drop(n)
      case _                 => c.view.drop(n).toSeq
    }
    /** Returns the elements of the matched sequence as a `Seq`. */
    def toSeq: scala.Seq[A] = c.toSeq
  }
}

/** A `SeqFactory` for strict collection types, overriding `fill`, `tabulate` and
 *  `concat` with builder-based implementations that avoid creating intermediate views.
 */
trait StrictOptimizedSeqFactory[+CC[A] <: SeqOps[A, Seq, Seq[A]] & caps.Pure] extends SeqFactory[CC] {

  /** Produces a $coll containing the results of some element computation a number of times.
   *
   *  @tparam A the element type of the $coll
   *  @param   n  the number of elements contained in the $coll.
   *  @param   elem the element computation, re-evaluated for every position
   *  @return  A $coll that contains the results of `n` evaluations of `elem`.
   */
  override def fill[A](n: Int)(elem: => A): CC[A] = {
    val b = newBuilder[A]
    b.sizeHint(n)
    var i = 0
    while (i < n) {
      b += elem
      i += 1
    }
    b.result()
  }

  /** Produces a $coll containing values of a given function over a range of integer values starting from 0.
   *
   *  @tparam A the element type of the $coll
   *  @param  n   The number of elements in the $coll
   *  @param  f   The function computing element values
   *  @return A $coll consisting of elements `f(0), ..., f(n -1)`
   */
  override def tabulate[A](n: Int)(f: Int => A): CC[A] = {
    val b = newBuilder[A]
    b.sizeHint(n)
    var i = 0
    while (i < n) {
      b += f(i)
      i += 1
    }
    b.result()
  }

  /** Concatenates all argument collections into a single $coll.
   *
   *  @tparam A the element type of the $coll
   *  @param xss the collections that are to be concatenated.
   *  @return the concatenation of all the collections.
   */
  override def concat[A](xss: Iterable[A]*): CC[A] = {
    val b = newBuilder[A]
    val knownSizes = xss.view.map(_.knownSize)
    if (knownSizes forall (_ >= 0)) {
      b.sizeHint(knownSizes.sum)
    }
    for (xs <- xss) b ++= xs
    b.result()
  }

}

/**
 *  @tparam A Type of elements (e.g. `Int`, `Boolean`, etc.)
 *  @tparam C Type of collection (e.g. `List[Int]`, `TreeMap[Int, String]`, etc.)
 *  @define factoryInfo
 *   This object provides a set of operations to create $Coll values.
 *
 *  @define coll collection
 *  @define Coll `Iterable`
 */
trait SpecificIterableFactory[-A, +C] extends Factory[A, C] {
  /** An empty $coll of type `C`. */
  def empty: C
  /** Creates a $coll with the specified elements.
   *
   *  @param xs the elements of the created $coll
   *  @return a new $coll with elements `xs`
   */
  def apply(xs: A*): C = fromSpecific(xs)
  /** Produces a $coll containing the results of some element computation a number of times.
   *
   *  @param n the number of elements contained in the $coll
   *  @param elem the element computation, re-evaluated for every position
   *  @return a $coll that contains the results of `n` evaluations of `elem`
   */
  def fill(n: Int)(elem: => A): C = fromSpecific(new View.Fill(n)(elem))
  /** Returns a new builder for a $coll of type `C`. */
  def newBuilder: Builder[A, C]

  /** This factory itself, made available implicitly as a [[Factory]] so that it can
   *  be used wherever a `Factory[A, C]` is expected, for example as the argument of `to`.
   */
  implicit def specificIterableFactory: Factory[A, C] = this
}

/**
 *  @define factoryInfo
 *   This object provides a set of operations to create $Coll values.
 *
 *  @define coll collection
 *  @define Coll `Iterable`
 *
 *  @tparam CC Collection type constructor for the map (e.g. `Map`, `HashMap`)
 */
trait MapFactory[+CC[_, _]] extends Serializable { self: MapFactory[CC] =>

  /** An empty Map.
   *
   *  @tparam K the type of the keys
   *  @tparam V the type of the values
   *  @return an empty map of type `CC[K, V]`
   */
  def empty[K, V]: CC[K, V]

  /** A collection of type Map generated from given iterable object.
   *
   *  @tparam K the type of the keys
   *  @tparam V the type of the values
   *  @param it the source collection of key-value pairs
   *  @return a new map of type `CC[K, V]` containing the bindings from `it`
   */
  def from[K, V](it: IterableOnce[(K, V)]^): CC[K, V]^{it}

  /** A collection of type Map that contains given key/value bindings.
   *
   *  @tparam K the type of the keys
   *  @tparam V the type of the values
   *  @param elems the key-value pairs to include in the map
   *  @return a new map of type `CC[K, V]` containing the given `elems`
   */
  def apply[K, V](elems: (K, V)*): CC[K, V] = from(elems)

  /** The default builder for Map objects.
   *
   *  @tparam K the type of the keys
   *  @tparam V the type of the values
   *  @return a new `Builder` that accepts key-value pairs and produces a `CC[K, V]`
   */
  def newBuilder[K, V]: Builder[(K, V), CC[K, V]]

  /** The default Factory instance for maps.
   *
   *  @tparam K the type of the keys
   *  @tparam V the type of the values
   *  @return a `Factory` that builds a `CC[K, V]` from a collection of key-value pairs
   */
  implicit def mapFactory[K, V]: Factory[(K, V), CC[K, V]] = MapFactory.toFactory(this)
}

object MapFactory {

  /** Fixes the key and value types of `factory` to `K` and `V`, respectively.
   *  @tparam K Type of keys
   *  @tparam V Type of values
   *  @tparam CC Collection type constructor of the factory (e.g. `Map`, `HashMap`, etc.)
   *  @param factory The factory to fix the key and value types
   *  @return A [[Factory]] that uses the given `factory` to build a map with keys of type `K`
   *         and values of type `V`
   */
  implicit def toFactory[K, V, CC[_, _]](factory: MapFactory[CC]): Factory[(K, V), CC[K, V]] = new ToFactory[K, V, CC](factory)

  @SerialVersionUID(3L)
  private class ToFactory[K, V, CC[_, _]](factory: MapFactory[CC]) extends Factory[(K, V), CC[K, V]] with Serializable {
    /** Returns a map of type `CC[K, V]` containing the key-value pairs of `it`, built with `factory`.
     *
     *  @param it the source of key-value pairs
     */
    def fromSpecific(it: IterableOnce[(K, V)]^): CC[K, V]^{it} = factory.from[K, V](it)
    /** Returns a new builder for a `CC[K, V]`, obtained from `factory`. */
    def newBuilder: Builder[(K, V), CC[K, V]] = factory.newBuilder[K, V]
  }

  /** Fixes the key and value types of `factory` to `K` and `V`, respectively, and
   *  adapts it to the [[BuildFrom]] typeclass.
   *
   *  The resulting instance ignores its source collection (its `From` type is `Any`)
   *  and always builds with the given `factory`.
   *
   *  @tparam K Type of keys
   *  @tparam V Type of values
   *  @tparam CC Collection type constructor of the factory (e.g. `Map`, `HashMap`, etc.)
   *  @param factory The factory to adapt
   *  @return A [[BuildFrom]] that uses the given `factory` to build a map with keys of
   *         type `K` and values of type `V`, regardless of the source collection
   */
  implicit def toBuildFrom[K, V, CC[_, _]](factory: MapFactory[CC]): BuildFrom[Any, (K, V), CC[K, V]] =
    new BuildFrom[Any, (K, V), CC[K, V]] {
      def fromSpecific(from: Any)(it: IterableOnce[(K, V)]^) = factory.from(it)
      def newBuilder(from: Any) = factory.newBuilder[K, V]
    }

  /** A `MapFactory` that forwards all operations to another factory.
   *
   *  @tparam C Collection type constructor of both this factory and the underlying factory
   *  @param delegate The factory that all operations are forwarded to
   */
  @SerialVersionUID(3L)
  class Delegate[C[_, _] <: caps.Pure](delegate: MapFactory[C]) extends MapFactory[C] {
    /** Creates a map of type `C[K, V]` that contains the given key-value pairs, by forwarding to `delegate`.
     *
     *  @tparam K the type of the keys
     *  @tparam V the type of the values
     *  @param elems the key-value pairs to include in the map
     *  @return a new `C[K, V]` containing the given `elems`
     */
    override def apply[K, V](elems: (K, V)*): C[K, V] = delegate.apply(elems*)
    /** Creates a map of type `C[K, V]` from the key-value pairs of `it`, by forwarding to `delegate`.
     *
     *  @tparam K the type of the keys
     *  @tparam V the type of the values
     *  @param it the source collection of key-value pairs
     *  @return a new `C[K, V]` containing the bindings from `it`
     */
    def from[K, V](it: IterableOnce[(K, V)]^): C[K, V] = delegate.from(it)
    /** An empty map of type `C[K, V]`, obtained from `delegate`.
     *
     *  @tparam K the type of the keys
     *  @tparam V the type of the values
     *  @return an empty `C[K, V]`
     */
    def empty[K, V]: C[K, V] = delegate.empty
    /** Returns a new builder for a `C[K, V]`, obtained from `delegate`.
     *
     *  @tparam K the type of the keys
     *  @tparam V the type of the values
     */
    def newBuilder[K, V]: Builder[(K, V), C[K, V]] = delegate.newBuilder
  }
}

/** Base trait for companion objects of collections that require an implicit evidence.
 *  @tparam CC Collection type constructor (e.g. `ArraySeq`)
 *  @tparam Ev Unary type constructor for the implicit evidence required for an element type
 *            (typically `Ordering` or `ClassTag`)
 *
 *  @define factoryInfo
 *   This object provides a set of operations to create $Coll values.
 *
 *  @define coll collection
 *  @define Coll `Iterable`
 */
trait EvidenceIterableFactory[+CC[_], Ev[_]] extends Serializable, caps.Pure {

  /** Creates a target $coll from an existing source collection.
   *
   *  @tparam E the type of the ${coll}'s elements, for which an implicit `Ev` instance must exist
   *  @param it Source collection
   *  @return a new $coll with the elements of `it`
   */
  def from[E : Ev](it: IterableOnce[E]^): CC[E]

  /** An empty $coll.
   *
   *  @tparam A the type of the ${coll}'s elements, for which an implicit `Ev` instance must exist
   *  @return an empty $coll of type `CC[A]`
   */
  def empty[A : Ev]: CC[A]

  /** Creates a $coll with the specified elements.
   *
   *  @tparam A the type of the ${coll}'s elements, for which an implicit `Ev` instance must exist
   *  @param xs the elements of the created $coll
   *  @return a new $coll with elements `xs`
   */
  def apply[A : Ev](xs: A*): CC[A] = from(xs)

  /** Produces a $coll containing the results of some element computation a number of times.
   *
   *  @tparam A the element type of the $coll, for which an implicit `Ev` instance must exist
   *  @param   n  the number of elements contained in the $coll.
   *  @param   elem the element computation
   *  @return  A $coll that contains the results of `n` evaluations of `elem`.
   */
  def fill[A : Ev](n: Int)(elem: => A): CC[A] = from(new View.Fill(n)(elem))

  /** Produces a $coll containing values of a given function over a range of integer values starting from 0.
   *
   *  @tparam A the element type of the $coll, for which an implicit `Ev` instance must exist
   *  @param  n   The number of elements in the $coll
   *  @param  f   The function computing element values
   *  @return A $coll consisting of elements `f(0), ..., f(n -1)`
   */
  def tabulate[A : Ev](n: Int)(f: Int => A): CC[A] = from(new View.Tabulate(n)(f))

  /** Produces a $coll containing repeated applications of a function to a start value.
   *
   *  @tparam A the element type of the $coll, for which an implicit `Ev` instance must exist
   *  @param start the start value of the $coll
   *  @param len   the number of elements contained in the $coll
   *  @param f     the function that's repeatedly applied
   *  @return      a $coll with `len` values in the sequence `start, f(start), f(f(start)), ...`
   */
  def iterate[A : Ev](start: A, len: Int)(f: A => A): CC[A] = from(new View.Iterate(start, len)(f))

  /** Produces a $coll that uses a function `f` to produce elements of type `A`
   *  and update an internal state of type `S`.
   *
   *  @tparam A   Type of the elements
   *  @tparam S   Type of the internal state
   *  @param init State initial value
   *  @param f    Computes the next element (or returns `None` to signal
   *             the end of the collection)
   *  @return a $coll that produces elements using `f` until `f` returns `None`
   */
  def unfold[A : Ev, S](init: S)(f: S => Option[(A, S)]): CC[A] = from(new View.Unfold(init)(f))

  /** Returns a new builder for $Coll objects.
   *
   *  @tparam A the type of the ${coll}'s elements, for which an implicit `Ev` instance must exist
   */
  def newBuilder[A : Ev]: Builder[A, CC[A]]

  /** A [[Factory]] view of this factory, with the element type fixed to `A`.
   *
   *  Allows this factory to be used wherever a `Factory[A, CC[A]]` is expected,
   *  for example as the argument of `to`.
   *
   *  @tparam A the type of the ${coll}'s elements, for which an implicit `Ev` instance must exist
   *  @return a [[Factory]] that delegates to this factory to build a `CC[A]`
   */
  implicit def evidenceIterableFactory[A : Ev]: Factory[A, CC[A]] = EvidenceIterableFactory.toFactory(this)
}

object EvidenceIterableFactory {

  /** Fixes the element type of `factory` to `A`.
   *  @tparam A Type of elements
   *  @tparam CC Collection type constructor of the factory (e.g. `TreeSet`)
   *  @tparam Ev Type constructor of the evidence (usually `Ordering` or `ClassTag`)
   *  @param factory The factory to fix the element type
   *  @return A [[Factory]] that uses the given `factory` to build a collection of elements
   *         of type `A`
   */
  implicit def toFactory[Ev[_], A: Ev, CC[_]](factory: EvidenceIterableFactory[CC, Ev]): Factory[A, CC[A]] = new ToFactory[Ev, A, CC](factory)

  @SerialVersionUID(3L)
  private class ToFactory[Ev[_], A: Ev, CC[_]](factory: EvidenceIterableFactory[CC, Ev]) extends Factory[A, CC[A]] with Serializable {
    /** Returns a collection of type `CC[A]` containing the elements of `it`, built with `factory`.
     *
     *  @param it the source of elements
     */
    def fromSpecific(it: IterableOnce[A]^) = factory.from[A](it)
    /** Returns a new builder for a `CC[A]`, obtained from `factory`. */
    def newBuilder: Builder[A, CC[A]] = factory.newBuilder[A]
  }

  /** Fixes the element type of `factory` to `A` and adapts it to the [[BuildFrom]] typeclass.
   *
   *  The resulting instance ignores its source collection (its `From` type is `Any`)
   *  and always builds with the given `factory`.
   *
   *  @tparam Ev Type constructor of the evidence (usually `Ordering` or `ClassTag`)
   *  @tparam A Type of elements
   *  @tparam CC Collection type constructor of the factory (e.g. `TreeSet`)
   *  @param factory The factory to adapt
   *  @return A [[BuildFrom]] that uses the given `factory` to build a collection of
   *         elements of type `A`, regardless of the source collection
   */
  implicit def toBuildFrom[Ev[_], A: Ev, CC[_]](factory: EvidenceIterableFactory[CC, Ev]): BuildFrom[Any, A, CC[A]] = new EvidenceIterableFactoryToBuildFrom(factory)
  private class EvidenceIterableFactoryToBuildFrom[Ev[_], A: Ev, CC[_]](factory: EvidenceIterableFactory[CC, Ev]) extends BuildFrom[Any, A, CC[A]] {
    /** Returns a collection of type `CC[A]` containing the elements of `it`, built with `factory`.
     *
     *  @param from the source collection; never used
     *  @param it the source of elements
     */
    def fromSpecific(from: Any)(it: IterableOnce[A]^) = factory.from[A](it)
    /** Returns a new builder for a `CC[A]`, obtained from `factory`.
     *
     *  @param from the source collection; never used
     */
    def newBuilder(from: Any): Builder[A, CC[A]] = factory.newBuilder[A]
  }

  /** An `EvidenceIterableFactory` that forwards all operations to another factory.
   *
   *  @tparam CC Collection type constructor of both this factory and the underlying factory
   *  @tparam Ev Type constructor of the evidence (usually `Ordering` or `ClassTag`)
   *  @param delegate The factory that all operations are forwarded to
   */
  @SerialVersionUID(3L)
  class Delegate[CC[_], Ev[_]](delegate: EvidenceIterableFactory[CC, Ev]) extends EvidenceIterableFactory[CC, Ev] {
    /** Creates a collection of type `CC[A]` with the specified elements, by forwarding to `delegate`.
     *
     *  @tparam A the type of the collection's elements, for which an implicit `Ev` instance must exist
     *  @param xs the elements of the created collection
     *  @return a new `CC[A]` with elements `xs`
     */
    override def apply[A: Ev](xs: A*): CC[A] = delegate.apply(xs*)
    /** An empty collection of type `CC[A]`, obtained from `delegate`.
     *
     *  @tparam A the type of the collection's elements, for which an implicit `Ev` instance must exist
     *  @return an empty `CC[A]`
     */
    def empty[A : Ev]: CC[A] = delegate.empty
    /** Creates a collection of type `CC[E]` from the elements of `it`, by forwarding to `delegate`.
     *
     *  @tparam E the type of the collection's elements, for which an implicit `Ev` instance must exist
     *  @param it the source collection
     *  @return a new `CC[E]` with the elements of `it`
     */
    def from[E : Ev](it: IterableOnce[E]^): CC[E] = delegate.from(it)
    /** Returns a new builder for a `CC[A]`, obtained from `delegate`.
     *
     *  @tparam A the type of the collection's elements, for which an implicit `Ev` instance must exist
     */
    def newBuilder[A : Ev]: Builder[A, CC[A]] = delegate.newBuilder[A]
  }
}

/** Base trait for companion objects of collections that require an implicit `Ordering`.
 *  @tparam CC Collection type constructor (e.g. `SortedSet`)
 */
trait SortedIterableFactory[+CC[_]] extends EvidenceIterableFactory[CC, Ordering]

object SortedIterableFactory {
  /** A [[SortedIterableFactory]] that forwards all operations to another factory.
   *
   *  The required evidence is an implicit `Ordering` of the element type.
   *
   *  @tparam CC Collection type constructor of both this factory and the underlying factory
   *  @param delegate The factory that all operations are forwarded to
   */
  @SerialVersionUID(3L)
  class Delegate[CC[_]](delegate: EvidenceIterableFactory[CC, Ordering])
    extends EvidenceIterableFactory.Delegate[CC, Ordering](delegate) with SortedIterableFactory[CC]
}

/** Base trait for companion objects of collections that require an implicit `ClassTag`.
 *  @tparam CC Collection type constructor (e.g. `ArraySeq`)
 */
trait ClassTagIterableFactory[+CC[_]] extends EvidenceIterableFactory[CC, ClassTag] {

  @`inline` private implicit def ccClassTag[X]: ClassTag[CC[X]] =
    ClassTag.AnyRef.asInstanceOf[ClassTag[CC[X]]] // Good enough for boxed vs primitive arrays

  /** Produces a $coll containing a sequence of increasing of integers.
   *
   *  @tparam A the element type of the $coll, which must have `Integral` and `ClassTag` instances
   *  @param start the first element of the $coll
   *  @param end   the end value of the $coll (the first value NOT contained)
   *  @return  a $coll with values `start, start + 1, ..., end - 1`
   */
  def range[A : Integral : ClassTag](start: A, end: A): CC[A] = from(NumericRange(start, end, implicitly[Integral[A]].one))

  /** Produces a $coll containing equally spaced values in some integer interval.
   *
   *  @tparam A the element type of the $coll, which must have `Integral` and `ClassTag` instances
   *  @param start the start value of the $coll
   *  @param end   the end value of the $coll (the first value NOT contained)
   *  @param step  the difference between successive elements of the $coll (must be positive or negative)
   *  @return      a $coll with values `start, start + step, ...` up to, but excluding `end`
   */
  def range[A : Integral : ClassTag](start: A, end: A, step: A): CC[A] = from(NumericRange(start, end, step))

  /** Produces a two-dimensional $coll containing the results of some element computation a number of times.
   *
   *  @tparam A the element type of the $coll, which must have a `ClassTag`
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   elem the element computation
   *  @return  A $coll that contains the results of `n1 x n2` evaluations of `elem`.
   */
  def fill[A : ClassTag](n1: Int, n2: Int)(elem: => A): CC[CC[A] @uncheckedVariance] = fill(n1)(fill(n2)(elem))

  /** Produces a three-dimensional $coll containing the results of some element computation a number of times.
   *
   *  @tparam A the element type of the $coll, which must have a `ClassTag`
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3rd dimension
   *  @param   elem the element computation
   *  @return  A $coll that contains the results of `n1 x n2 x n3` evaluations of `elem`.
   */
  def fill[A : ClassTag](n1: Int, n2: Int, n3: Int)(elem: => A): CC[CC[CC[A]] @uncheckedVariance] = fill(n1)(fill(n2, n3)(elem))

  /** Produces a four-dimensional $coll containing the results of some element computation a number of times.
   *
   *  @tparam A the element type of the $coll, which must have a `ClassTag`
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3rd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   elem the element computation
   *  @return  A $coll that contains the results of `n1 x n2 x n3 x n4` evaluations of `elem`.
   */
  def fill[A : ClassTag](n1: Int, n2: Int, n3: Int, n4: Int)(elem: => A): CC[CC[CC[CC[A]]] @uncheckedVariance] =
    fill(n1)(fill(n2, n3, n4)(elem))

  /** Produces a five-dimensional $coll containing the results of some element computation a number of times.
   *
   *  @tparam A the element type of the $coll, which must have a `ClassTag`
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3rd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   n5  the number of elements in the 5th dimension
   *  @param   elem the element computation
   *  @return  A $coll that contains the results of `n1 x n2 x n3 x n4 x n5` evaluations of `elem`.
   */
  def fill[A : ClassTag](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(elem: => A): CC[CC[CC[CC[CC[A]]]] @uncheckedVariance] =
    fill(n1)(fill(n2, n3, n4, n5)(elem))

  /** Produces a two-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
   *
   *  @tparam A the element type of the $coll, which must have a `ClassTag`
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   f   The function computing element values
   *  @return A $coll consisting of elements `f(i1, i2)`
   *          for `0 <= i1 < n1` and `0 <= i2 < n2`.
   */
  def tabulate[A : ClassTag](n1: Int, n2: Int)(f: (Int, Int) => A): CC[CC[A] @uncheckedVariance] =
    tabulate(n1)(i1 => tabulate(n2)(f(i1, _)))

  /** Produces a three-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
   *
   *  @tparam A the element type of the $coll, which must have a `ClassTag`
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3rd dimension
   *  @param   f   The function computing element values
   *  @return A $coll consisting of elements `f(i1, i2, i3)`
   *          for `0 <= i1 < n1`, `0 <= i2 < n2`, and `0 <= i3 < n3`.
   */
  def tabulate[A : ClassTag](n1: Int, n2: Int, n3: Int)(f: (Int, Int, Int) => A): CC[CC[CC[A]] @uncheckedVariance] =
    tabulate(n1)(i1 => tabulate(n2, n3)(f(i1, _, _)))

  /** Produces a four-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
   *
   *  @tparam A the element type of the $coll, which must have a `ClassTag`
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3rd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   f   The function computing element values
   *  @return A $coll consisting of elements `f(i1, i2, i3, i4)`
   *          for `0 <= i1 < n1`, `0 <= i2 < n2`, `0 <= i3 < n3`, and `0 <= i4 < n4`.
   */
  def tabulate[A : ClassTag](n1: Int, n2: Int, n3: Int, n4: Int)(f: (Int, Int, Int, Int) => A): CC[CC[CC[CC[A]]] @uncheckedVariance] =
    tabulate(n1)(i1 => tabulate(n2, n3, n4)(f(i1, _, _, _)))

  /** Produces a five-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
   *
   *  @tparam A the element type of the $coll, which must have a `ClassTag`
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3rd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   n5  the number of elements in the 5th dimension
   *  @param   f   The function computing element values
   *  @return A $coll consisting of elements `f(i1, i2, i3, i4, i5)`
   *          for `0 <= i1 < n1`, `0 <= i2 < n2`, `0 <= i3 < n3`, `0 <= i4 < n4`, and `0 <= i5 < n5`.
   */
  def tabulate[A : ClassTag](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(f: (Int, Int, Int, Int, Int) => A): CC[CC[CC[CC[CC[A]]]] @uncheckedVariance] =
    tabulate(n1)(i1 => tabulate(n2, n3, n4, n5)(f(i1, _, _, _, _)))
}

object ClassTagIterableFactory {
  /** A [[ClassTagIterableFactory]] that forwards all operations to another factory.
   *
   *  The required evidence is an implicit `ClassTag` of the element type.
   *
   *  @tparam CC Collection type constructor of both this factory and the underlying factory
   *  @param delegate The factory that all operations are forwarded to
   */
  @SerialVersionUID(3L)
  class Delegate[CC[_]](delegate: EvidenceIterableFactory[CC, ClassTag])
    extends EvidenceIterableFactory.Delegate[CC, ClassTag](delegate) with ClassTagIterableFactory[CC]

  /** An IterableFactory that uses ClassTag.Any as the evidence for every element type. This may or may not be
   *  sound depending on the use of the `ClassTag` by the collection implementation.
   */
  @SerialVersionUID(3L)
  class AnyIterableDelegate[CC[_]](delegate: ClassTagIterableFactory[CC]) extends IterableFactory[CC] {
    /** An empty collection of type `CC[A]`, obtained from `delegate` using `ClassTag.Any` as the evidence.
     *
     *  @tparam A the type of the collection's elements
     *  @return an empty `CC[A]`
     */
    def empty[A]: CC[A] = delegate.empty(using ClassTag.Any).asInstanceOf[CC[A]]
    /** Creates a collection of type `CC[A]` from the elements of `it`, by forwarding to
     *  `delegate` with `ClassTag.Any` as the evidence.
     *
     *  @tparam A the type of the collection's elements
     *  @param it the source collection
     *  @return a new `CC[A]` with the elements of `it`
     */
    def from[A](it: IterableOnce[A]^): CC[A] = delegate.from[Any](it)(using ClassTag.Any).asInstanceOf[CC[A]]
    /** Returns a new builder for a `CC[A]`, obtained from `delegate` using `ClassTag.Any` as the evidence.
     *
     *  @tparam A the type of the collection's elements
     */
    def newBuilder[A]: Builder[A, CC[A]] = delegate.newBuilder(using ClassTag.Any).asInstanceOf[Builder[A, CC[A]]]
    /** Creates a collection of type `CC[A]` with the specified elements, by forwarding to
     *  `delegate` with `ClassTag.Any` as the evidence.
     *
     *  @tparam A the type of the collection's elements
     *  @param elems the elements of the created collection
     *  @return a new `CC[A]` with elements `elems`
     */
    override def apply[A](elems: A*): CC[A] = delegate.apply[Any](elems*)(using ClassTag.Any).asInstanceOf[CC[A]]
    /** Produces a collection containing repeated applications of a function to a start value,
     *  by forwarding to `delegate` with `ClassTag.Any` as the evidence.
     *
     *  @tparam A the element type of the collection
     *  @param start the start value of the collection
     *  @param len the number of elements contained in the collection
     *  @param f the function that's repeatedly applied
     *  @return a `CC[A]` with `len` values in the sequence `start, f(start), f(f(start)), ...`
     */
    override def iterate[A](start: A, len: Int)(f: A => A): CC[A] = delegate.iterate[A](start, len)(f)(using ClassTag.Any.asInstanceOf[ClassTag[A]])
    /** Produces a collection that uses a function `f` to produce elements of type `A` and
     *  update an internal state of type `S`, by forwarding to `delegate` with `ClassTag.Any`
     *  as the evidence.
     *
     *  @tparam A Type of the elements
     *  @tparam S Type of the internal state
     *  @param init State initial value
     *  @param f Computes the next element (or returns `None` to signal the end of the collection)
     *  @return a `CC[A]` that produces elements using `f` until `f` returns `None`
     */
    override def unfold[A, S](init: S)(f: S => Option[(A, S)]): CC[A] = delegate.unfold[A, S](init)(f)(using ClassTag.Any.asInstanceOf[ClassTag[A]])
    /** Produces a collection containing a sequence of increasing integers, by forwarding to
     *  `delegate` with `ClassTag.Any` as the evidence.
     *
     *  @tparam A the element type of the collection
     *  @param start the first element of the collection
     *  @param end the end value of the collection (the first value NOT contained)
     *  @param i the `Integral` instance for `A`
     *  @return a `CC[A]` with values `start, start + 1, ..., end - 1`
     */
    override def range[A](start: A, end: A)(implicit i: Integral[A]): CC[A] = delegate.range[A](start, end)(using i, ClassTag.Any.asInstanceOf[ClassTag[A]])
    /** Produces a collection containing equally spaced values in some integer interval, by
     *  forwarding to `delegate` with `ClassTag.Any` as the evidence.
     *
     *  @tparam A the element type of the collection
     *  @param start the start value of the collection
     *  @param end the end value of the collection (the first value NOT contained)
     *  @param step the difference between successive elements of the collection (must be positive or negative)
     *  @param i the `Integral` instance for `A`
     *  @return a `CC[A]` with values `start, start + step, ...` up to, but excluding `end`
     */
    override def range[A](start: A, end: A, step: A)(implicit i: Integral[A]): CC[A] = delegate.range[A](start, end, step)(using i, ClassTag.Any.asInstanceOf[ClassTag[A]])
    /** Produces a collection containing the results of some element computation a number of
     *  times, by forwarding to `delegate` with `ClassTag.Any` as the evidence.
     *
     *  @tparam A the element type of the collection
     *  @param n the number of elements contained in the collection
     *  @param elem the element computation, re-evaluated for every position
     *  @return a `CC[A]` that contains the results of `n` evaluations of `elem`
     */
    override def fill[A](n: Int)(elem: => A): CC[A] = delegate.fill[Any](n)(elem)(using ClassTag.Any).asInstanceOf[CC[A]]
    /** Produces a collection containing values of a given function over a range of integer
     *  values starting from 0, by forwarding to `delegate` with `ClassTag.Any` as the evidence.
     *
     *  @tparam A the element type of the collection
     *  @param n the number of elements in the collection
     *  @param f the function computing element values
     *  @return a `CC[A]` consisting of elements `f(0), ..., f(n - 1)`
     */
    override def tabulate[A](n: Int)(f: Int => A): CC[A] = delegate.tabulate[Any](n)(f)(using ClassTag.Any).asInstanceOf[CC[A]]
  }
}

/**
 *  @tparam CC Collection type constructor (e.g. `ArraySeq`)
 */
trait ClassTagSeqFactory[+CC[A] <: SeqOps[A, Seq, Seq[A]] & caps.Pure] extends ClassTagIterableFactory[CC] {
  import SeqFactory.UnapplySeqWrapper
  /** An extractor for sequence patterns, e.g. `case Seq(a, b, rest*) => ...`.
   *
   *  The extraction itself never fails: the returned wrapper always reports
   *  `isEmpty == false`, and the pattern matcher checks the pattern's arity
   *  against the sequence via the wrapper's `lengthCompare`.
   *
   *  @tparam A the type of the sequence's elements
   *  @param x the sequence to extract elements from
   *  @return a [[SeqFactory.UnapplySeqWrapper]] exposing the elements of `x`
   */
  final def unapplySeq[A](x: CC[A] @uncheckedVariance): UnapplySeqWrapper[A] = new UnapplySeqWrapper(x) // TODO is uncheckedVariance sound here?
}

object ClassTagSeqFactory {
  /** A [[ClassTagSeqFactory]] that forwards all operations to another factory.
   *
   *  @tparam CC Collection type constructor of both this factory and the underlying factory
   *  @param delegate The factory that all operations are forwarded to
   */
  @SerialVersionUID(3L)
  class Delegate[CC[A] <: SeqOps[A, Seq, Seq[A]] & caps.Pure](delegate: ClassTagSeqFactory[CC])
    extends ClassTagIterableFactory.Delegate[CC](delegate) with ClassTagSeqFactory[CC]

  /** A SeqFactory that uses ClassTag.Any as the evidence for every element type. This may or may not be
   *  sound depending on the use of the `ClassTag` by the collection implementation.
   */
  @SerialVersionUID(3L)
  class AnySeqDelegate[CC[A] <: SeqOps[A, Seq, Seq[A]] & caps.Pure](delegate: ClassTagSeqFactory[CC])
    extends ClassTagIterableFactory.AnyIterableDelegate[CC](delegate) with SeqFactory[CC]
}

/** A [[ClassTagSeqFactory]] for strict collection types, overriding `fill` and
 *  `tabulate` with builder-based implementations that avoid creating intermediate views.
 */
trait StrictOptimizedClassTagSeqFactory[+CC[A] <: SeqOps[A, Seq, Seq[A]] & caps.Pure] extends ClassTagSeqFactory[CC] {

  /** Produces a $coll containing the results of some element computation a number of times.
   *
   *  @tparam A the element type of the $coll, which must have a `ClassTag`
   *  @param   n  the number of elements contained in the $coll.
   *  @param   elem the element computation, re-evaluated for every position
   *  @return  A $coll that contains the results of `n` evaluations of `elem`.
   */
  override def fill[A : ClassTag](n: Int)(elem: => A): CC[A] = {
    val b = newBuilder[A]
    b.sizeHint(n)
    var i = 0
    while (i < n) {
      b += elem
      i += 1
    }
    b.result()
  }

  /** Produces a $coll containing values of a given function over a range of integer values starting from 0.
   *
   *  @tparam A the element type of the $coll, which must have a `ClassTag`
   *  @param  n   The number of elements in the $coll
   *  @param  f   The function computing element values
   *  @return A $coll consisting of elements `f(0), ..., f(n -1)`
   */
  override def tabulate[A : ClassTag](n: Int)(f: Int => A): CC[A] = {
    val b = newBuilder[A]
    b.sizeHint(n)
    var i = 0
    while (i < n) {
      b += f(i)
      i += 1
    }
    b.result()
  }

}

/**
 *  @define factoryInfo
 *   This object provides a set of operations to create $Coll values.
 *
 *  @define coll collection
 *  @define Coll `Iterable`
 *
 *  @tparam CC Collection type constructor for the sorted map (e.g. `TreeMap`)
 */
trait SortedMapFactory[+CC[_, _]] extends Serializable { this: SortedMapFactory[CC] =>

  /** An empty sorted map, whose keys are ordered by the implicit `Ordering`.
   *
   *  @tparam K the type of the keys, which must have an `Ordering`
   *  @tparam V the type of the values
   *  @return an empty map of type `CC[K, V]`
   */
  def empty[K : Ordering, V]: CC[K, V]

  /** A sorted map that contains the key-value pairs of the given collection, with keys
   *  ordered by the implicit `Ordering`.
   *
   *  @tparam K the type of the keys, which must have an `Ordering`
   *  @tparam V the type of the values
   *  @param it the source collection of key-value pairs
   *  @return a new map of type `CC[K, V]` containing the bindings from `it`
   */
  def from[K : Ordering, V](it: IterableOnce[(K, V)]^): CC[K, V]

  /** A sorted map that contains the given key-value bindings, with keys ordered by the
   *  implicit `Ordering`.
   *
   *  @tparam K the type of the keys, which must have an `Ordering`
   *  @tparam V the type of the values
   *  @param elems the key-value pairs to include in the map
   *  @return a new map of type `CC[K, V]` containing the given `elems`
   */
  def apply[K : Ordering, V](elems: (K, V)*): CC[K, V] = from(elems)

  /** The default builder for sorted maps of type `CC`.
   *
   *  @tparam K the type of the keys, which must have an `Ordering`
   *  @tparam V the type of the values
   *  @return a new `Builder` that accepts key-value pairs and produces a `CC[K, V]`
   */
  def newBuilder[K : Ordering, V]: Builder[(K, V), CC[K, V]]

  /** The default Factory instance for sorted maps.
   *
   *  @tparam K the type of the keys, which must have an `Ordering`
   *  @tparam V the type of the values
   *  @return a `Factory` that builds a `CC[K, V]` from a collection of key-value pairs
   */
  implicit def sortedMapFactory[K : Ordering, V]: Factory[(K, V), CC[K, V]] = SortedMapFactory.toFactory(this)

}

object SortedMapFactory {

  /** Implicit conversion that fixes the key and value types of `factory` to `K` and `V`,
   *  respectively.
   *
   *  @tparam K Type of keys
   *  @tparam V Type of values
   *  @tparam CC Collection type constructor of the factory (e.g. `TreeMap`)
   *  @param factory The factory to fix the key and value types
   *  @return A [[Factory]] that uses the given `factory` to build a map with keys of
   *         type `K` and values of type `V`
   */
  implicit def toFactory[K : Ordering, V, CC[_, _]](factory: SortedMapFactory[CC]): Factory[(K, V), CC[K, V]] = new ToFactory[K, V, CC](factory)

  @SerialVersionUID(3L)
  private class ToFactory[K : Ordering, V, CC[_, _]](factory: SortedMapFactory[CC]) extends Factory[(K, V), CC[K, V]] with Serializable {
    /** Returns a sorted map of type `CC[K, V]` containing the key-value pairs of `it`, built with `factory`.
     *
     *  @param it the source of key-value pairs
     */
    def fromSpecific(it: IterableOnce[(K, V)]^): CC[K, V] = factory.from[K, V](it)
    /** Returns a new builder for a `CC[K, V]`, obtained from `factory`. */
    def newBuilder: Builder[(K, V), CC[K, V]] = factory.newBuilder[K, V]
  }

  /** Fixes the key and value types of `factory` to `K` and `V`, respectively, and
   *  adapts it to the [[BuildFrom]] typeclass.
   *
   *  The resulting instance ignores its source collection (its `From` type is `Any`)
   *  and always builds with the given `factory`.
   *
   *  @tparam K Type of keys, which must have an `Ordering`
   *  @tparam V Type of values
   *  @tparam CC Collection type constructor of the factory (e.g. `TreeMap`)
   *  @param factory The factory to adapt
   *  @return A [[BuildFrom]] that uses the given `factory` to build a map with keys of
   *         type `K` and values of type `V`, regardless of the source collection
   */
  implicit def toBuildFrom[K : Ordering, V, CC[_, _]](factory: SortedMapFactory[CC]): BuildFrom[Any, (K, V), CC[K, V]] = new SortedMapFactoryToBuildFrom(factory)
  private class SortedMapFactoryToBuildFrom[K : Ordering, V, CC[_, _]](factory: SortedMapFactory[CC]) extends BuildFrom[Any, (K, V), CC[K, V]] {
    /** Returns a sorted map of type `CC[K, V]` containing the key-value pairs of `it`, built with `factory`.
     *
     *  @param from the source collection; never used
     *  @param it the source of key-value pairs
     */
    def fromSpecific(from: Any)(it: IterableOnce[(K, V)]^) = factory.from(it)
    /** Returns a new builder for a `CC[K, V]`, obtained from `factory`.
     *
     *  @param from the source collection; never used
     */
    def newBuilder(from: Any) = factory.newBuilder[K, V]
  }

  /** A `SortedMapFactory` that forwards all operations to another factory.
   *
   *  @tparam CC Collection type constructor of both this factory and the underlying factory
   *  @param delegate The factory that all operations are forwarded to
   */
  @SerialVersionUID(3L)
  class Delegate[CC[_, _]](delegate: SortedMapFactory[CC]) extends SortedMapFactory[CC] {
    /** Creates a sorted map of type `CC[K, V]` that contains the given key-value pairs, by forwarding to `delegate`.
     *
     *  @tparam K the type of the keys, which must have an `Ordering`
     *  @tparam V the type of the values
     *  @param elems the key-value pairs to include in the map
     *  @return a new `CC[K, V]` containing the given `elems`
     */
    override def apply[K: Ordering, V](elems: (K, V)*): CC[K, V] = delegate.apply(elems*)
    /** Creates a sorted map of type `CC[K, V]` from the key-value pairs of `it`, by forwarding to `delegate`.
     *
     *  @tparam K the type of the keys, which must have an `Ordering`
     *  @tparam V the type of the values
     *  @param it the source collection of key-value pairs
     *  @return a new `CC[K, V]` containing the bindings from `it`
     */
    def from[K : Ordering, V](it: IterableOnce[(K, V)]^): CC[K, V] = delegate.from(it)
    /** An empty sorted map of type `CC[K, V]`, obtained from `delegate`.
     *
     *  @tparam K the type of the keys, which must have an `Ordering`
     *  @tparam V the type of the values
     *  @return an empty `CC[K, V]`
     */
    def empty[K : Ordering, V]: CC[K, V] = delegate.empty
    /** Returns a new builder for a `CC[K, V]`, obtained from `delegate`.
     *
     *  @tparam K the type of the keys, which must have an `Ordering`
     *  @tparam V the type of the values
     */
    def newBuilder[K : Ordering, V]: Builder[(K, V), CC[K, V]] = delegate.newBuilder
  }
}
