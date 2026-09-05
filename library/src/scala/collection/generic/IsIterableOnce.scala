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
package generic

import scala.language.`2.13`
import language.experimental.captureChecking
import caps.unsafe.untrackedCaptures

/** Type class witnessing that a collection representation type `Repr` has
 *  elements of type `A` and has a conversion to `IterableOnce[A]`.
 *
 *  This type enables simple enrichment of `IterableOnce`s with extension
 *  methods which can make full use of the mechanics of the Scala collections
 *  framework in their implementation.
 *
 *  Example usage,
 *  ```scala sc-name:import-buildfrom sc-hidden
 *    import scala.collection.BuildFrom
 *  ```
 *  ```scala sc-compile-with:import-buildfrom
 *    extension [Repr, I <: IsIterableOnce[Repr]](coll: Repr)(using it: I) {
 *      final def filterMap[B, That](f: it.A => Option[B])(using bf: BuildFrom[Repr, B, That]): That = {
 *        val b = bf.newBuilder(coll)
 *        for(e <- it(coll).iterator) f(e).foreach(b += _)
 *        b.result()
 *      }
 *    }
 *
 *    List(1, 2, 3, 4, 5).filterMap(i => if(i % 2 == 0) Some(i) else None)
 *    // == List(2, 4)
 *  ```
 *
 *  @tparam Repr the collection representation type that can be converted to `IterableOnce`
 */
transparent trait IsIterableOnce[Repr] {

  /** The type of elements we can traverse over (e.g. `Int`). */
  type A

  /** A function that converts a `Repr` to an `IterableOnce[A]`, equivalent to `apply`. */
  @deprecated("'conversion' is now a method named 'apply'", "2.13.0")
  @untrackedCaptures
  val conversion: Repr => IterableOnce[A] = apply(_)

  /** A conversion from the representation type `Repr` to an `IterableOnce[A]`.
   *
   *  @param coll the representation type instance to view as an `IterableOnce[A]`
   *  @return an `IterableOnce[A]` view of `coll`
   */
  def apply(coll: Repr): IterableOnce[A]

}

object IsIterableOnce extends IsIterableOnceLowPriority {

  // Straightforward case: IterableOnce subclasses
  /** Provides an `IsIterableOnce` instance for any `IterableOnce` subclass.
   *
   *  @tparam CC0 the collection type constructor, which must be a subclass of `IterableOnce`
   *  @tparam A0 the element type of the collection
   *  @return an `IsIterableOnce` instance for `CC0[A0]`
   */
  implicit def iterableOnceIsIterableOnce[CC0[A] <: IterableOnce[A], A0]: IsIterableOnce[CC0[A0]] { type A = A0 } =
    new IsIterableOnce[CC0[A0]] {
      type A = A0
      def apply(coll: CC0[A0]): IterableOnce[A0] = coll
    }

}

transparent trait IsIterableOnceLowPriority {

  // Makes `IsIterable` instance visible in `IsIterableOnce` companion
  /** Provides an `IsIterableOnce` instance for any type that has an implicit `IsIterable` instance.
   *
   *  @tparam Repr the collection representation type
   *  @param isIterableLike the implicit `IsIterable` instance for `Repr`
   *  @return `isIterableLike` itself (an `IsIterable` is an `IsIterableOnce`), preserving its element type `A`
   */
  implicit def isIterableLikeIsIterableOnce[Repr](implicit
    isIterableLike: IsIterable[Repr]
  ): IsIterableOnce[Repr] { type A = isIterableLike.A } = isIterableLike

}
