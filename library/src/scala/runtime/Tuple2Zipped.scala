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
package runtime

import scala.language.`2.13`
import scala.collection.{BuildFrom, IterableOps}
import scala.language.implicitConversions

/** This interface is intended as a minimal interface, not complicated
 *  by the requirement to resolve type constructors, for implicit search (which only
 *  needs to find an implicit conversion to Iterable for our purposes.)
 *  @define Coll `ZippedIterable2`
 *  @define coll collection
 *  @define collectExample
 *  @define willNotTerminateInf
 */
@deprecated("Use scala.collection.LazyZip2.", "2.13.0")
trait ZippedIterable2[+El1, +El2] extends Any {
  /** Returns an iterator that produces the element pairs of this zipped iterable. */
  def iterator: Iterator[(El1, El2)]
  /** Returns `true` if this zipped iterable produces no element pairs. */
  def isEmpty: Boolean
}
@deprecated("Use scala.collection.LazyZip2.", "2.13.0")
object ZippedIterable2 {
  /** Converts a [[ZippedIterable2]] to an `Iterable` of pairs.
   *
   *  The result is a wrapper whose `iterator` and `isEmpty` delegate to `zz`;
   *  no elements are copied.
   *
   *  @tparam El1 the first element type of each pair
   *  @tparam El2 the second element type of each pair
   *  @param zz the zipped iterable to convert
   */
  implicit def zippedIterable2ToIterable[El1, El2](zz: ZippedIterable2[El1, El2]): Iterable[(El1, El2)] = {
    new scala.collection.AbstractIterable[(El1, El2)] {
      def iterator: Iterator[(El1, El2)] = zz.iterator
      override def isEmpty: Boolean = zz.isEmpty
    }
  }
}

/** A decorator over a pair of collections that supports traversing both in
 *  lockstep.
 *
 *  Every operation iterates the two collections side by side and stops as soon
 *  as either one is exhausted, so excess elements of the longer collection are
 *  never visited. Instances are created by the deprecated `zipped` method on
 *  pairs (see [[Tuple2Zipped.Ops]]).
 *
 *  @tparam El1 the element type of the first collection
 *  @tparam It1 the type of the first collection
 *  @tparam El2 the element type of the second collection
 *  @tparam It2 the type of the second collection
 *  @param colls the pair of collections to traverse
 */
@deprecated("Use scala.collection.LazyZip2.", "2.13.0")
final class Tuple2Zipped[El1, It1 <: Iterable[El1], El2, It2 <: Iterable[El2]](private val colls: (It1, It2)) extends AnyVal with ZippedIterable2[El1, El2] {
  private def coll1 = colls._1
  private def coll2 = colls._2

  /** Returns a collection of the results of applying `f` to corresponding
   *  elements of the two collections.
   *
   *  Iteration stops as soon as the shorter collection is exhausted; excess
   *  elements of the longer collection are not passed to `f`.
   *
   *  @tparam B the type of the values returned by `f`
   *  @tparam To the type of the resulting collection
   *  @param f the function applied to each pair of corresponding elements
   *  @param bf the builder factory that creates the result collection from the
   *            first collection
   */
  def map[B, To](f: (El1, El2) => B)(implicit bf: BuildFrom[It1, B, To]): To = {
    val b = bf.newBuilder(coll1)
    b.sizeHint(coll1, delta = 0)
    val elems1 = coll1.iterator
    val elems2 = coll2.iterator

    while (elems1.hasNext && elems2.hasNext) {
      b += f(elems1.next(), elems2.next())
    }

    b.result()
  }

  /** Returns a collection built by applying `f` to corresponding elements of
   *  the two collections and concatenating the results.
   *
   *  Iteration stops as soon as the shorter collection is exhausted; excess
   *  elements of the longer collection are not passed to `f`.
   *
   *  @tparam B the element type of the collections returned by `f`
   *  @tparam To the type of the resulting collection
   *  @param f the function applied to each pair of corresponding elements
   *  @param bf the builder factory that creates the result collection from the
   *            first collection
   */
  def flatMap[B, To](f: (El1, El2) => IterableOnce[B])(implicit bf: BuildFrom[It1, B, To]): To = {
    val b = bf.newBuilder(coll1)
    val elems1 = coll1.iterator
    val elems2 = coll2.iterator

    while (elems1.hasNext && elems2.hasNext) {
      b ++= f(elems1.next(), elems2.next())
    }

    b.result()
  }

  /** Returns the pairs of corresponding elements for which `f` holds, as a
   *  pair of collections.
   *
   *  Iteration stops as soon as the shorter collection is exhausted. When `f`
   *  is true for a pair, its first element is added to the first result and
   *  its second element to the second, so the two results have equal length.
   *
   *  @tparam To1 the type of the first result collection
   *  @tparam To2 the type of the second result collection
   *  @param f the predicate applied to each pair of corresponding elements
   *  @param bf1 the builder factory that creates the first result collection
   *             from the first collection
   *  @param bf2 the builder factory that creates the second result collection
   *             from the second collection
   */
  def filter[To1, To2](f: (El1, El2) => Boolean)(implicit bf1: BuildFrom[It1, El1, To1], bf2: BuildFrom[It2, El2, To2]): (To1, To2) = {
    val b1 = bf1.newBuilder(coll1)
    val b2 = bf2.newBuilder(coll2)
    val elems1 = coll1.iterator
    val elems2 = coll2.iterator

    while (elems1.hasNext && elems2.hasNext) {
      val el1 = elems1.next()
      val el2 = elems2.next()
      if (f(el1, el2)) {
        b1 += el1
        b2 += el2
      }
    }

    (b1.result(), b2.result())
  }

  /** Returns `true` if `p` holds for at least one pair of corresponding
   *  elements of the two collections.
   *
   *  Iteration stops at the first pair satisfying `p`, or when the shorter
   *  collection is exhausted; excess elements of the longer collection are
   *  never tested.
   *
   *  @param p the predicate applied to each pair of corresponding elements
   */
  def exists(p: (El1, El2) => Boolean): Boolean = {
    val elems1 = coll1.iterator
    val elems2 = coll2.iterator

    while (elems1.hasNext && elems2.hasNext) {
      if (p(elems1.next(), elems2.next())) {
        return true
      }
    }
    false
  }

  /** Returns `true` if `p` holds for every pair of corresponding elements of
   *  the two collections.
   *
   *  Only as many pairs as the shorter collection provides are tested, so the
   *  result is `true` when either collection is empty. Iteration stops at the
   *  first pair failing `p`.
   *
   *  @param p the predicate applied to each pair of corresponding elements
   */
  def forall(p: (El1, El2) => Boolean): Boolean =
    !exists((x, y) => !p(x, y))

  /** Returns an iterator over the pairs of corresponding elements, truncated at the length of the shorter collection. */
  def iterator: Iterator[(El1, El2)] = coll1.iterator.zip(coll2.iterator)
  /** Returns `true` if either of the two collections is empty. */
  override def isEmpty: Boolean = coll1.isEmpty || coll2.isEmpty
  /** Applies `f` to each pair of corresponding elements of the two
   *  collections, for its side effects.
   *
   *  Iteration stops as soon as the shorter collection is exhausted; the
   *  results of `f` are discarded.
   *
   *  @tparam U the result type of `f`, which is discarded
   *  @param f the function applied to each pair of corresponding elements
   */
  def foreach[U](f: (El1, El2) => U): Unit = {
    val elems1 = coll1.iterator
    val elems2 = coll2.iterator

    while (elems1.hasNext && elems2.hasNext) {
      f(elems1.next(), elems2.next())
    }
  }

  /** Returns a string of the form `(coll1, coll2).zipped`, where `coll1` and `coll2` are the string representations of the two collections. */
  override def toString() = s"($coll1, $coll2).zipped"
}

@deprecated("Use scala.collection.LazyZip2.", since = "2.13.0")
object Tuple2Zipped {
  /** A value class adding the deprecated `zipped` and `invert` operations to
   *  pairs. Pairs are enriched with it through the implicit conversion
   *  `Predef.tuple2ToZippedOps`.
   *
   *  @tparam T1 the type of the pair's first component
   *  @tparam T2 the type of the pair's second component
   *  @param x the wrapped pair
   */
  final class Ops[T1, T2](private val x: (T1, T2)) extends AnyVal {
    /** Returns a collection of pairs built from corresponding elements of the
     *  two collections in this pair, turning a pair of collections into a
     *  collection of pairs.
     *
     *  Both collections are iterated in lockstep, stopping as soon as either
     *  is exhausted; excess elements of the longer collection are dropped.
     *
     *  @tparam El1 the element type of the first collection
     *  @tparam It1 the type constructor of the first collection
     *  @tparam El2 the element type of the second collection
     *  @tparam It2 the type constructor of the second collection
     *  @tparam That the type of the resulting collection of pairs
     *  @param w1 evidence that the pair's first component is a collection of `El1`
     *  @param w2 evidence that the pair's second component is a collection of `El2`
     *  @param bf the builder factory that creates the result collection from
     *            the first collection
     */
    @deprecated("Use xs.lazyZip(yz).map((_, _))", since = "2.13.0")
    def invert[El1, It1[a] <: Iterable[a], El2, It2[a] <: Iterable[a], That]
      (implicit w1: T1 <:< It1[El1],
                w2: T2 <:< It2[El2],
                bf: BuildFrom[T1, (El1, El2), That]
      ): That = {
        val buf = bf.newBuilder(x._1)
        val it1 = x._1.iterator
        val it2 = x._2.iterator
        while (it1.hasNext && it2.hasNext)
          buf += ((it1.next(), it2.next()))

        buf.result()
      }

    /** Returns a [[Tuple2Zipped]] decorator that traverses the two collections
     *  of this pair in lockstep.
     *
     *  @tparam El1 the element type of the first collection
     *  @tparam It1 the type of the first collection
     *  @tparam El2 the element type of the second collection
     *  @tparam It2 the type of the second collection
     */
    @deprecated("Use xs.lazyZip(ys)", since = "2.13.0")
    def zipped[El1, It1 <: Iterable[El1], El2, It2 <: Iterable[El2]]
      (implicit w1: T1 => IterableOps[El1, Iterable, It1] & It1,
                w2: T2 => IterableOps[El2, Iterable, It2] & It2
      ): Tuple2Zipped[El1, It1, El2, It2] = new Tuple2Zipped((w1(x._1), w2(x._2)))
  }
}
