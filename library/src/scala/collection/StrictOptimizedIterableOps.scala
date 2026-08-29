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

import scala.annotation.nowarn
import scala.annotation.unchecked.uncheckedVariance
import scala.runtime.Statics

/** Trait that overrides iterable operations to take advantage of strict builders.
 *
 *  @tparam A  Elements type
 *  @tparam CC Collection type constructor
 *  @tparam C  Collection type
 */
transparent trait StrictOptimizedIterableOps[+A, +CC[_], +C]
  extends Any
    with IterableOps[A, CC, C] {

  // Optimized, push-based version of `partition`
  /** Returns a pair holding, first, all elements that satisfy predicate `p` and,
   *  second, all elements that do not.
   *
   *  Overrides the default implementation to build both results with strict
   *  builders in a single traversal of this collection, instead of one
   *  traversal each for `filter` and `filterNot`.
   *
   *  @param p the predicate used to test elements
   *  @return a pair of collections: the first containing all elements that
   *          satisfy `p`, the second containing those that do not
   */
  override def partition(p: A => Boolean): (C, C) = {
    val l, r = newSpecificBuilder
    iterator.foreach(x => (if (p(x)) l else r) += x)
    (l.result(), r.result())
  }

  /** Splits this collection into a prefix/suffix pair according to a predicate.
   *
   *  Overrides the default implementation to build both results with strict
   *  builders in a single traversal of this collection, instead of one
   *  traversal each for `takeWhile` and `dropWhile`.
   *
   *  @param p the predicate used to test elements
   *  @return a pair consisting of the longest prefix of this collection whose
   *          elements all satisfy `p`, and the rest of this collection
   */
  override def span(p: A => Boolean): (C, C) = {
    val first = newSpecificBuilder
    val second = newSpecificBuilder
    val it = iterator
    var inFirst = true
    while (it.hasNext && inFirst) {
      val a = it.next()
      if (p(a)) {
        first += a
      } else {
        second += a
        inFirst = false
      }
    }
    while (it.hasNext) {
      second += it.next()
    }
    (first.result(), second.result())
  }

  /** Converts this collection of pairs into two collections of the first and
   *  second half of each pair.
   *
   *  Overrides the default implementation to fill two strict builders in a
   *  single traversal of this collection.
   *
   *  @tparam A1 the type of the first half of the element pairs
   *  @tparam A2 the type of the second half of the element pairs
   *  @param asPair evidence that this collection's element type is a pair `(A1, A2)`
   *  @return a pair of collections containing, respectively, the first and
   *          second half of each element pair of this collection
   */
  override def unzip[A1, A2](implicit asPair: A -> (A1, A2)): (CC[A1], CC[A2]) = {
    val first = iterableFactory.newBuilder[A1]
    val second = iterableFactory.newBuilder[A2]
    foreach { a =>
      val pair = asPair(a)
      first += pair._1
      second += pair._2
    }
    (first.result(), second.result())
  }

  /** Converts this collection of triples into three collections of the first,
   *  second, and third element of each triple.
   *
   *  Overrides the default implementation to fill three strict builders in a
   *  single traversal of this collection.
   *
   *  @tparam A1 the type of the first member of the element triples
   *  @tparam A2 the type of the second member of the element triples
   *  @tparam A3 the type of the third member of the element triples
   *  @param asTriple evidence that this collection's element type is a triple `(A1, A2, A3)`
   *  @return a triple of collections containing, respectively, the first,
   *          second, and third member of each element triple of this collection
   */
  override def unzip3[A1, A2, A3](implicit asTriple: A -> (A1, A2, A3)): (CC[A1], CC[A2], CC[A3]) = {
    val b1 = iterableFactory.newBuilder[A1]
    val b2 = iterableFactory.newBuilder[A2]
    val b3 = iterableFactory.newBuilder[A3]

    foreach { xyz =>
      val triple = asTriple(xyz)
      b1 += triple._1
      b2 += triple._2
      b3 += triple._3
    }
    (b1.result(), b2.result(), b3.result())
  }

  // The implementations of the following operations are not fundamentally different from
  // the view-based implementations, but they turn out to be slightly faster because
  // a couple of indirection levels are removed

  /** Builds a new collection by applying a function to all elements of this
   *  collection.
   *
   *  Overrides the default view-based implementation to fill a strict builder
   *  directly, which is slightly faster.
   *
   *  @tparam B the element type of the returned collection
   *  @param f the function to apply to each element
   *  @return a new collection containing the results of applying `f` to every
   *          element of this collection
   */
  override def map[B](f: A => B): CC[B] =
    strictOptimizedMap(iterableFactory.newBuilder, f)

  /**
   *  @tparam B Type of elements of the resulting collection (e.g. `String`)
   *  @tparam C2 Type of the resulting collection (e.g. `List[String]`)
   *  @param b Builder to use to build the resulting collection
   *  @param f Element transformation function
   *  @return The resulting collection
   */
  @inline protected final def strictOptimizedMap[B, C2](b: mutable.Builder[B, C2], f: A => B): C2 = {
    val it = iterator
    while (it.hasNext) {
      b += f(it.next())
    }
    b.result()
  }

  /** Builds a new collection by applying a function to all elements of this
   *  collection and concatenating the results.
   *
   *  Overrides the default view-based implementation to fill a strict builder
   *  directly, which is slightly faster.
   *
   *  @tparam B the element type of the returned collection
   *  @param f the function to apply to each element
   *  @return a new collection containing the concatenated results of applying
   *          `f` to every element of this collection
   */
  override def flatMap[B](f: A => IterableOnce[B]^): CC[B] =
    strictOptimizedFlatMap(iterableFactory.newBuilder, f)

  /**
   *  @tparam B Type of elements of the resulting collection (e.g. `String`)
   *  @tparam C2 Type of the resulting collection (e.g. `List[String]`)
   *  @param b Builder to use to build the resulting collection
   *  @param f Element transformation function
   *  @return The resulting collection
   */
  @inline protected final def strictOptimizedFlatMap[B, C2](b: mutable.Builder[B, C2]^, f: A => IterableOnce[B]^): C2 = {
    val it = iterator
    while (it.hasNext) {
      b ++= f(it.next())
    }
    b.result()
  }

  /**
   *  @tparam B Type of elements of the resulting collections (e.g. `Int`)
   *  @tparam C2 Type of the resulting collection (e.g. `List[Int]`)
   *  @param that Elements to concatenate to this collection
   *  @param b Builder to use to build the resulting collection
   *  @return The resulting collection
   */
  @inline protected final def strictOptimizedConcat[B >: A, C2](that: IterableOnce[B]^, b: mutable.Builder[B, C2]^): C2 = {
    b ++= this
    b ++= that
    b.result()
  }

  /** Builds a new collection by applying a partial function to all elements of
   *  this collection on which the function is defined.
   *
   *  Overrides the default view-based implementation to fill a strict builder
   *  directly, using a single `applyOrElse` call per element to test
   *  definedness and transform the element at the same time.
   *
   *  @tparam B the element type of the returned collection
   *  @param pf the partial function to apply to each element
   *  @return a new collection containing the results of applying `pf` to every
   *          element of this collection on which it is defined
   */
  override def collect[B](pf: PartialFunction[A, B]^): CC[B] =
    strictOptimizedCollect(iterableFactory.newBuilder, pf)

  /**
   *  @tparam B Type of elements of the resulting collection (e.g. `String`)
   *  @tparam C2 Type of the resulting collection (e.g. `List[String]`)
   *  @param b Builder to use to build the resulting collection
   *  @param pf Element transformation partial function
   *  @return The resulting collection
   */
  @inline protected final def strictOptimizedCollect[B, C2](b: mutable.Builder[B, C2]^, pf: PartialFunction[A, B]^): C2 = {
    val marker = Statics.pfMarker
    val it = iterator
    while (it.hasNext) {
      val elem = it.next()
      val v = pf.applyOrElse(elem, ((x: A) => marker).asInstanceOf[Function[A, B]])
      if (marker ne v.asInstanceOf[AnyRef]) b += v
    }
    b.result()
  }

  /** Converts this collection of collections into a collection formed by the
   *  elements of the nested collections.
   *
   *  Overrides the default view-based implementation to fill a strict builder
   *  directly, which is slightly faster.
   *
   *  @tparam B the element type of the nested collections
   *  @param toIterableOnce evidence that this collection's element type can be
   *                        seen as an `IterableOnce[B]`
   *  @return a new collection containing the concatenated elements of the
   *          nested collections, in order
   */
  override def flatten[B](implicit toIterableOnce: A -> IterableOnce[B]): CC[B] =
    strictOptimizedFlatten(iterableFactory.newBuilder)

  /**
   *  @tparam B Type of elements of the resulting collection (e.g. `Int`)
   *  @tparam C2 Type of the resulting collection (e.g. `List[Int]`)
   *  @param b Builder to use to build the resulting collection
   *  @param toIterableOnce Evidence that `A` can be seen as an `IterableOnce[B]`
   *  @return The resulting collection
   */
  @inline protected final def strictOptimizedFlatten[B, C2](b: mutable.Builder[B, C2])(implicit toIterableOnce: A => IterableOnce[B]): C2 = {
    val it = iterator
    while (it.hasNext) {
      b ++= toIterableOnce(it.next())
    }
    b.result()
  }

  /** Returns a new collection of pairs formed from this collection and another
   *  iterable collection by combining corresponding elements.
   *
   *  Overrides the default view-based implementation to fill a strict builder
   *  directly, which is slightly faster. As with the default implementation,
   *  if one of the two collections is longer than the other, its remaining
   *  elements are ignored.
   *
   *  @tparam B the type of the second half of the returned pairs
   *  @param that the iterable providing the second half of each result pair
   *  @return a new collection containing pairs consisting of corresponding
   *          elements of this collection and `that`, whose length is the
   *          minimum of the lengths of the two collections
   */
  override def zip[B](that: IterableOnce[B]^): CC[(A @uncheckedVariance, B)] =
    strictOptimizedZip(that, iterableFactory.newBuilder[(A, B)])

  /**
   *  @tparam B Type of elements of the second collection (e.g. `String`)
   *  @tparam C2 Type of the resulting collection (e.g. `List[(Int, String)]`)
   *  @param that Collection to zip with this collection
   *  @param b Builder to use to build the resulting collection
   *  @return The resulting collection
   */
  @inline protected final def strictOptimizedZip[B, C2](that: IterableOnce[B]^, b: mutable.Builder[(A, B), C2]^): C2 = {
    val it1 = iterator
    val it2 = that.iterator
    while (it1.hasNext && it2.hasNext) {
      b += ((it1.next(), it2.next()))
    }
    b.result()
  }

  /** Returns a collection of pairs of each element of this collection with its index,
   *  counting from 0, filling a strict builder in a single traversal instead of going
   *  through a view.
   */
  override def zipWithIndex: CC[(A @uncheckedVariance, Int)] = {
    val b = iterableFactory.newBuilder[(A, Int)]
    var i = 0
    val it = iterator
    while (it.hasNext) {
      b += ((it.next(), i))
      i += 1
    }
    b.result()
  }

  /** Produces a collection containing the cumulative results of applying the
   *  operator going left to right, including the initial value.
   *
   *  Overrides the default view-based implementation to fill a strict builder
   *  directly, which is slightly faster.
   *
   *  @tparam B the element type of the returned collection
   *  @param z the initial value
   *  @param op the binary operator applied to the intermediate result and the element
   *  @return a new collection containing the intermediate results of inserting
   *          `op` between consecutive elements of this collection, going left
   *          to right with the start value `z` on the left, so one element
   *          longer than this collection
   */
  override def scanLeft[B](z: B)(op: (B, A) => B): CC[B] = {
    val b = iterableFactory.newBuilder[B]
    b.sizeHint(this, delta = 0)
    var acc = z
    b += acc
    val it = iterator
    while (it.hasNext) {
      acc = op(acc, it.next())
      b += acc
    }
    b.result()
  }

  /** Selects all elements of this collection that satisfy a predicate.
   *
   *  Overrides the default view-based implementation to fill a strict builder
   *  directly, via [[filterImpl]].
   *
   *  @param pred the predicate used to test elements
   *  @return a new collection containing all elements of this collection that
   *          satisfy `pred`, in the order they appear in this collection
   */
  override def filter(pred: A => Boolean): C = filterImpl(pred, isFlipped = false)

  /** Selects all elements of this collection that do not satisfy a predicate.
   *
   *  Overrides the default view-based implementation to fill a strict builder
   *  directly, via [[filterImpl]].
   *
   *  @param pred the predicate used to test elements
   *  @return a new collection containing all elements of this collection that
   *          do not satisfy `pred`, in the order they appear in this collection
   */
  override def filterNot(pred: A => Boolean): C = filterImpl(pred, isFlipped = true)

  protected[collection] def filterImpl(pred: A => Boolean, isFlipped: Boolean): C = {
    val b = newSpecificBuilder
    val it = iterator
    while (it.hasNext) {
      val elem = it.next()
      if (pred(elem) != isFlipped) {
        b += elem
      }
    }
    b.result()
  }

  // Optimized, push-based version of `partitionMap`
  /** Applies a function returning an `Either` to each element of this
   *  collection and collects the `Left` and `Right` results into two separate
   *  collections.
   *
   *  Overrides the default implementation to fill two strict builders in a
   *  single traversal of this collection, applying `f` once per element.
   *
   *  @tparam A1 the element type of the first resulting collection
   *  @tparam A2 the element type of the second resulting collection
   *  @param f the function mapping each element to a `Left(_)` or a `Right(_)`
   *  @return a pair of collections: the first containing the values wrapped in
   *          `Left`, the second containing the values wrapped in `Right`
   */
  override def partitionMap[A1, A2](f: A => Either[A1, A2]): (CC[A1], CC[A2]) = {
    val l = iterableFactory.newBuilder[A1]
    val r = iterableFactory.newBuilder[A2]
    foreach { x =>
      f(x) match {
        case Left(x1) => l += x1
        case Right(x2) => r += x2
      }
    }
    (l.result(), r.result())
  }

  // Optimization avoids creation of second collection
  /** Applies a side-effecting function to each element of this collection and
   *  returns this collection itself.
   *
   *  Overrides the default implementation to call `f` eagerly on every element
   *  and return this collection unchanged, instead of creating a second,
   *  mapped collection.
   *
   *  @tparam U the return type of `f`, ignored
   *  @param f the side-effecting function to apply to each element
   *  @return this collection, unchanged
   */
  override def tapEach[U](f: A => U): C^{this}  = {
    foreach(f)
    coll
  }

  /** A collection containing the last `n` elements of this collection.
   *  $willForceEvaluation
   *
   *  @param n the number of elements to take from the end of this collection
   *  @return a new collection containing the last `n` elements of this collection, or all elements if `n` is greater than the size, or an empty collection if `n` is non-positive
   */
  override def takeRight(n: Int): C = {
    val b = newSpecificBuilder
    b.sizeHintBounded(n, toIterable: @nowarn("cat=deprecation"))
    val lead = iterator drop n
    val it = iterator
    while (lead.hasNext) {
      lead.next()
      it.next()
    }
    while (it.hasNext) b += it.next()
    b.result()
  }

  /** The rest of the collection without its `n` last elements. For
   *  linear, immutable collections this should avoid making a copy.
   *  $willForceEvaluation
   *
   *  @param n the number of elements to drop from the end of this collection
   *  @return a new collection containing all elements of this collection except the last `n`, or an empty collection if `n` is greater than or equal to the size, or all elements if `n` is non-positive
   */
  override def dropRight(n: Int): C = {
    val b = newSpecificBuilder
    if (n >= 0) b.sizeHint(this, delta = -n)
    val lead = iterator drop n
    val it = iterator
    while (lead.hasNext) {
      b += it.next()
      lead.next()
    }
    b.result()
  }
}
