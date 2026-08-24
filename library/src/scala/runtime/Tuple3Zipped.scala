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

/** See comment on ZippedIterable2
 *  @define Coll `ZippedIterable3`
 *  @define coll collection
 *  @define collectExample
 *  @define willNotTerminateInf
 */
@deprecated("Use scala.collection.LazyZip3.", "2.13.0")
trait ZippedIterable3[+El1, +El2, +El3] extends Any {
  /** Returns an iterator that produces the element triples of this zipped iterable. */
  def iterator: Iterator[(El1, El2, El3)]
  /** Returns `true` if this zipped iterable produces no element triples. */
  def isEmpty: Boolean
}
@deprecated("Use scala.collection.LazyZip3.", "2.13.0")
object ZippedIterable3 {
  /** Converts a [[ZippedIterable3]] to an `Iterable` of triples.
   *
   *  The result is a wrapper whose `iterator` and `isEmpty` delegate to `zz`;
   *  no elements are copied.
   *
   *  @tparam El1 the first element type of each triple
   *  @tparam El2 the second element type of each triple
   *  @tparam El3 the third element type of each triple
   *  @param zz the zipped iterable to convert
   */
  implicit def zippedIterable3ToIterable[El1, El2, El3](zz: ZippedIterable3[El1, El2, El3]): Iterable[(El1, El2, El3)] = {
    new scala.collection.AbstractIterable[(El1, El2, El3)] {
      def iterator: Iterator[(El1, El2, El3)] = zz.iterator
      override def isEmpty: Boolean = zz.isEmpty
    }
  }
}

/** A decorator over a triple of collections that supports traversing all
 *  three in lockstep.
 *
 *  Every operation iterates the three collections side by side and stops as
 *  soon as any one of them is exhausted, so excess elements of the longer
 *  collections are never visited. Instances are created by the deprecated
 *  `zipped` method on triples (see [[Tuple3Zipped.Ops]]).
 *
 *  @tparam El1 the element type of the first collection
 *  @tparam It1 the type of the first collection
 *  @tparam El2 the element type of the second collection
 *  @tparam It2 the type of the second collection
 *  @tparam El3 the element type of the third collection
 *  @tparam It3 the type of the third collection
 *  @param colls the triple of collections to traverse
 */
@deprecated("Use scala.collection.LazyZip3.", "2.13.0")
final class Tuple3Zipped[El1, It1 <: Iterable[El1], El2, It2 <: Iterable[El2], El3, It3 <: Iterable[El3]](private val colls: (It1, It2, It3))
        extends AnyVal with ZippedIterable3[El1, El2, El3] {

  private def coll1 = colls._1
  private def coll2 = colls._2
  private def coll3 = colls._3

  /** Returns a collection of the results of applying `f` to corresponding
   *  elements of the three collections.
   *
   *  Iteration stops as soon as the shortest collection is exhausted; excess
   *  elements of the longer collections are not passed to `f`.
   *
   *  @tparam B the type of the values returned by `f`
   *  @tparam To the type of the resulting collection
   *  @param f the function applied to each triple of corresponding elements
   *  @param bf the builder factory that creates the result collection from the
   *            first collection
   */
  def map[B, To](f: (El1, El2, El3) => B)(implicit bf: BuildFrom[It1, B, To]): To = {
    val b = bf.newBuilder(coll1)
    val elems1 = coll1.iterator
    val elems2 = coll2.iterator
    val elems3 = coll3.iterator

    while (elems1.hasNext && elems2.hasNext && elems3.hasNext) {
      b += f(elems1.next(), elems2.next(), elems3.next())
    }
    b.result()
  }

  /** Returns a collection built by applying `f` to corresponding elements of
   *  the three collections and concatenating the results.
   *
   *  Iteration stops as soon as the shortest collection is exhausted; excess
   *  elements of the longer collections are not passed to `f`.
   *
   *  @tparam B the element type of the collections returned by `f`
   *  @tparam To the type of the resulting collection
   *  @param f the function applied to each triple of corresponding elements
   *  @param bf the builder factory that creates the result collection from the
   *            first collection
   */
  def flatMap[B, To](f: (El1, El2, El3) => IterableOnce[B])(implicit bf: BuildFrom[It1, B, To]): To = {
    val b = bf.newBuilder(coll1)
    val elems1 = coll1.iterator
    val elems2 = coll2.iterator
    val elems3 = coll3.iterator

    while (elems1.hasNext && elems2.hasNext && elems3.hasNext) {
      b ++= f(elems1.next(), elems2.next(), elems3.next())
    }
    b.result()
  }

  /** Returns the triples of corresponding elements for which `f` holds, as a
   *  triple of collections.
   *
   *  Iteration stops as soon as the shortest collection is exhausted. When `f`
   *  is true for a triple, its elements are added to the corresponding result
   *  collections, so the three results have equal length.
   *
   *  @tparam To1 the type of the first result collection
   *  @tparam To2 the type of the second result collection
   *  @tparam To3 the type of the third result collection
   *  @param f the predicate applied to each triple of corresponding elements
   */
  def filter[To1, To2, To3](f: (El1, El2, El3) => Boolean)(
               implicit bf1: BuildFrom[It1, El1, To1],
                        bf2: BuildFrom[It2, El2, To2],
                        bf3: BuildFrom[It3, El3, To3]): (To1, To2, To3) = {
    val b1 = bf1.newBuilder(coll1)
    val b2 = bf2.newBuilder(coll2)
    val b3 = bf3.newBuilder(coll3)
    val elems1 = coll1.iterator
    val elems2 = coll2.iterator
    val elems3 = coll3.iterator

    while (elems1.hasNext && elems2.hasNext && elems3.hasNext) {
      val el1 = elems1.next()
      val el2 = elems2.next()
      val el3 = elems3.next()

      if (f(el1, el2, el3)) {
        b1 += el1
        b2 += el2
        b3 += el3
      }
    }
    (b1.result(), b2.result(), b3.result())
  }

  /** Returns `true` if `p` holds for at least one triple of corresponding
   *  elements of the three collections.
   *
   *  Iteration stops at the first triple satisfying `p`, or when the shortest
   *  collection is exhausted; excess elements of the longer collections are
   *  never tested.
   *
   *  @param p the predicate applied to each triple of corresponding elements
   */
  def exists(p: (El1, El2, El3) => Boolean): Boolean = {
    val elems1 = coll1.iterator
    val elems2 = coll2.iterator
    val elems3 = coll3.iterator

    while (elems1.hasNext && elems2.hasNext && elems3.hasNext) {
      if (p(elems1.next(), elems2.next(), elems3.next())) {
        return true
      }
    }
    false
  }

  /** Returns `true` if `p` holds for every triple of corresponding elements
   *  of the three collections.
   *
   *  Only as many triples as the shortest collection provides are tested, so
   *  the result is `true` when any of the collections is empty. Iteration
   *  stops at the first triple failing `p`.
   *
   *  @param p the predicate applied to each triple of corresponding elements
   */
  def forall(p: (El1, El2, El3) => Boolean): Boolean =
    !exists((x, y, z) => !p(x, y, z))

  /** Returns an iterator over the triples of corresponding elements, truncated at the length of the shortest collection. */
  def iterator: Iterator[(El1, El2, El3)] = coll1.iterator.zip(coll2.iterator).zip(coll3.iterator).map { case ((a, b), c) => (a, b, c)}
  /** Returns `true` if any of the three collections is empty. */
  override def isEmpty: Boolean = coll1.isEmpty || coll2.isEmpty || coll3.isEmpty
  /** Applies `f` to each triple of corresponding elements of the three
   *  collections, for its side effects.
   *
   *  Iteration stops as soon as the shortest collection is exhausted; the
   *  results of `f` are discarded.
   *
   *  @tparam U the result type of `f`, which is discarded
   *  @param f the function applied to each triple of corresponding elements
   */
  def foreach[U](f: (El1, El2, El3) => U): Unit = {
    val elems1 = coll1.iterator
    val elems2 = coll2.iterator
    val elems3 = coll3.iterator

    while (elems1.hasNext && elems2.hasNext && elems3.hasNext) {
      f(elems1.next(), elems2.next(), elems3.next())
    }
  }

  /** Returns a string of the form `(coll1, coll2, coll3).zipped`, where `coll1`, `coll2` and `coll3` are the string representations of the three collections. */
  override def toString() = s"($coll1, $coll2, $coll3).zipped"
}

@deprecated("Use scala.collection.LazyZip3.", since = "2.13.0")
object Tuple3Zipped {
  /** A value class adding the deprecated `zipped` and `invert` operations to
   *  triples. Triples are enriched with it through the implicit conversion
   *  `Predef.tuple3ToZippedOps`.
   *
   *  @tparam T1 the type of the triple's first component
   *  @tparam T2 the type of the triple's second component
   *  @tparam T3 the type of the triple's third component
   *  @param x the wrapped triple
   */
  final class Ops[T1, T2, T3](private val x: (T1, T2, T3)) extends AnyVal {
    /** Returns a collection of triples built from corresponding elements of
     *  the three collections in this triple, turning a triple of collections
     *  into a collection of triples.
     *
     *  The collections are iterated in lockstep, stopping as soon as any of
     *  them is exhausted; excess elements of the longer collections are
     *  dropped.
     *
     *  @tparam El1 the element type of the first collection
     *  @tparam It1 the type constructor of the first collection
     *  @tparam El2 the element type of the second collection
     *  @tparam It2 the type constructor of the second collection
     *  @tparam El3 the element type of the third collection
     *  @tparam It3 the type constructor of the third collection
     *  @tparam That the type of the resulting collection of triples
     *  @param w1 evidence that the triple's first component is a collection of `El1`
     *  @param w2 evidence that the triple's second component is a collection of `El2`
     *  @param w3 evidence that the triple's third component is a collection of `El3`
     *  @param bf the builder factory that creates the result collection from
     *            the first collection
     */
    @deprecated("Use xs.lazyZip(yz).lazyZip(zs).map((_, _, _))", since = "2.13.0")
    def invert[El1, It1[a] <: Iterable[a], El2, It2[a] <: Iterable[a], El3, It3[a] <: Iterable[a], That]
      (implicit w1: T1 <:< It1[El1],
                w2: T2 <:< It2[El2],
                w3: T3 <:< It3[El3],
                bf: BuildFrom[T1, (El1, El2, El3), That]
      ): That = {
        val buf = bf.newBuilder(x._1)
        val it1 = x._1.iterator
        val it2 = x._2.iterator
        val it3 = x._3.iterator
        while (it1.hasNext && it2.hasNext && it3.hasNext)
          buf += ((it1.next(), it2.next(), it3.next()))

        buf.result()
      }

    /** Returns a [[Tuple3Zipped]] decorator that traverses the three
     *  collections of this triple in lockstep.
     *
     *  @tparam El1 the element type of the first collection
     *  @tparam It1 the type of the first collection
     *  @tparam El2 the element type of the second collection
     *  @tparam It2 the type of the second collection
     *  @tparam El3 the element type of the third collection
     *  @tparam It3 the type of the third collection
     */
    @deprecated("Use xs.lazyZip(ys).lazyZip(zs)", since = "2.13.0")
    def zipped[El1, It1 <: Iterable[El1], El2, It2 <: Iterable[El2], El3, It3 <: Iterable[El3]]
      (implicit w1: T1 => IterableOps[El1, Iterable, It1] & It1,
                w2: T2 => IterableOps[El2, Iterable, It2] & It2,
                w3: T3 => IterableOps[El3, Iterable, It3] & It3
      ): Tuple3Zipped[El1, It1, El2, It2, El3, It3] = new Tuple3Zipped((w1(x._1), w2(x._2), w3(x._3)))
  }
}
