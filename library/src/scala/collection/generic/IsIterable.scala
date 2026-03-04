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
package generic

import scala.language.`2.13`
import language.experimental.captureChecking
import caps.unsafe.untrackedCaptures

/** A trait which can be used to avoid code duplication when defining extension
 *  methods that should be applicable both to existing Scala collections (i.e.,
 *  types extending `Iterable`) as well as other (potentially user-defined)
 *  types that could be converted to a Scala collection type. This trait
 *  makes it possible to uniformly treat Scala collections and types that can be implicitly
 *  converted to a collection type. For example, one can provide
 *  extension methods that work both on collection types and on `String`s. (`String`s
 *  do not extend `Iterable`, but can be converted to `Iterable`.)
 *
 *  `IsIterable` provides three members:
 *
 *  1. type member `A`, which represents the element type of the target `Iterable[A]`
 *  1. type member `C`, which represents the type returned by transformation operations that preserve the collection’s element type
 *  1. method `apply`, which provides a way to convert between the type we wish to add extension methods to, `Repr`, and `IterableOps[A, Iterable, C]`.
 *
 *  ### Usage
 *
 *  One must provide `IsIterable` as an implicit parameter of the extension method.
 *  Its usage is shown below. Our objective in the following example
 *  is to provide a generic extension method `mapReduce` for any type that extends
 *  or can be converted to `Iterable`, such as `String`.
 *
 *  ```
 *    import scala.collection.generic.IsIterable
 *
 *    extension [Repr, I <: IsIterable[Repr]](coll: Repr)(using it: I)
 *      def mapReduce[B](mapper: it.A => B)(reducer: (B, B) => B): B = {
 *        val iter = it(coll).iterator
 *        var res = mapper(iter.next())
 *        while (iter.hasNext)
 *          res = reducer(res, mapper(iter.next()))
 *        res
 *      }
 *
 *  // See it in action!
 *  List(1, 2, 3).mapReduce(_ * 2)(_ + _) // res0: Int = 12
 *  "Yeah, well, you know, that's just, like, your opinion, man.".mapReduce(x => 1)(_ + _) // res1: Int = 59
 *  ```
 *
 *  The extension method takes a receiver `coll` of type `Repr`, where
 *  `Repr` typically represents the collection type, and an argument `it` of a subtype of `IsIterable[Repr]`.
 *
 *  The body of the method starts by converting the `coll` argument to an `IterableOps` in order to
 *  call the `iterator` method on it.
 *  The rest of the implementation is straightforward.
 *
 *  The `mapReduce` extension method is available
 *  on any type `Repr` for which there is an implicit `IsIterable[Repr]` instance.
 *  The companion object for `IsIterable` provides an instance for types
 *  that are already an `IterableOps`.
 *
 *  ### Implementing `IsIterable` for New Types
 *
 *  For a custom type, one need only provide an implicit value of type `IsIterable`
 *  that specifies the element type, the collection type, and an implementation
 *  of `apply` that converts the collection to an `IterableOps`.
 *
 *  Below is an example implementation of the `IsIterable` trait
 *  where the `Repr` type is `Range`. In practice, `IsIterable[Range]` is already provided by
 *  the implicit value for any `IterableOps`, as for `List` in the previous example.
 *  Similarly, the instance for `String` was available because the library provides an `IsSeq[String]`.
 *
 *  ```
 *  implicit val rangeRepr: IsIterable[Range] { type A = Int; type C = IndexedSeq[Int] } =
 *   new IsIterable[Range] {
 *     type A = Int
 *     type C = IndexedSeq[Int]
 *     def apply(coll: Range): IterableOps[Int, IndexedSeq, IndexedSeq[Int]] = coll
 *   }
 *  ```
 *
 *  (Note that in practice the `IsIterable[Range]` instance is already provided by
 *  the standard library, and it is defined as an `IsSeq[Range]` instance)
 *
 *  @tparam Repr the representation type (e.g. `String`, `Array[Int]`) that can be converted to an `Iterable`
 */
transparent trait IsIterable[Repr] extends IsIterableOnce[Repr] {

  /** The type returned by transformation operations that preserve the same elements
   *  type (e.g. `filter`, `take`).
   *
   *  In practice, this type is often `Repr` itself, except in the case
   *  of `SeqView[A]` (and other `View[A]` subclasses), where it is `View[A]`.
   */
  type C

  @deprecated("'conversion' is now a method named 'apply'", "2.13.0")
  @untrackedCaptures
  override val conversion: Repr => IterableOps[A, Iterable, C] = apply(_)

  /** A conversion from the type `Repr` to `IterableOps[A, Iterable, C]`.
   *
   *  @param coll the collection or value to convert to `IterableOps[A, Iterable, C]`
   */
  def apply(coll: Repr): IterableOps[A, Iterable, C]

}

object IsIterable extends IsIterableLowPriority {

  // Straightforward case: IterableOps subclasses
  implicit def iterableOpsIsIterable[A0, CC0[X] <: IterableOps[X, Iterable, CC0[X]]]: IsIterable[CC0[A0]] { type A = A0; type C = CC0[A0] } =
    new IsIterable[CC0[A0]] {
      type A = A0
      type C = CC0[A0]
      def apply(coll: CC0[A]): IterableOps[A, Iterable, C] = coll
    }

  // The `BitSet` type can not be unified with the `CC0` parameter of
  // the above definition because it does not take a type parameter.
  // Hence the need for a separate case:
  implicit def bitSetOpsIsIterable[C0 <: BitSet & BitSetOps[C0]]: IsIterable[C0] { type A = Int; type C = C0 } =
    new IsIterable[C0] {
      type A = Int
      type C = C0
      def apply(coll: C0): IterableOps[Int, Iterable, C0] = coll
    }

}

transparent trait IsIterableLowPriority {

  // Makes `IsSeq` instances visible in `IsIterable` companion
  implicit def isSeqLikeIsIterable[Repr](implicit
    isSeqLike: IsSeq[Repr]
  ): IsIterable[Repr] { type A = isSeqLike.A; type C = isSeqLike.C } = isSeqLike

  // Makes `IsMap` instances visible in `IsIterable` companion
  implicit def isMapLikeIsIterable[Repr](implicit
    isMapLike: IsMap[Repr]
  ): IsIterable[Repr] { type A = isMapLike.A; type C = isMapLike.C } = isMapLike

}
