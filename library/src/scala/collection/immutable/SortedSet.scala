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

/** Base trait for sorted sets.
 *
 *  @tparam A the element type of the sorted set, which must have an implicit `Ordering`
 */
trait SortedSet[A]
  extends Set[A]
     with collection.SortedSet[A]
     with SortedSetOps[A, SortedSet, SortedSet[A]]
     with SortedSetFactoryDefaults[A, SortedSet, Set] {

  /** Returns this set itself, with its static type widened to its unsorted counterpart `Set` */
  override def unsorted: Set[A] = this

  /** Returns the [[SortedSet$ `SortedSet`]] object, the default factory for immutable sorted sets, which creates `TreeSet`s */
  override def sortedIterableFactory: SortedIterableFactory[SortedSet] = SortedSet
}

/**
 *  @define coll immutable sorted set
 *  @define Coll `immutable.SortedSet`
 *
 *  @tparam A the element type of the sorted set
 *  @tparam CC the type constructor for the resulting sorted set (e.g., `SortedSet`)
 *  @tparam C the type of the concrete sorted set
 */
transparent trait SortedSetOps[A, +CC[X] <: SortedSet[X], +C <: SortedSetOps[A, CC, C]]
  extends SetOps[A, Set, C]
     with collection.SortedSetOps[A, CC, C] {

  /** Widens the type of this set to its unsorted counterpart. */
  def unsorted: Set[A]
}

transparent trait StrictOptimizedSortedSetOps[A, +CC[X] <: SortedSet[X], +C <: SortedSetOps[A, CC, C]]
  extends SortedSetOps[A, CC, C]
    with collection.StrictOptimizedSortedSetOps[A, CC, C]
    with StrictOptimizedSetOps[A, Set, C] {
}

/** $factoryInfo
 *  @define coll immutable sorted set
 *  @define Coll `immutable.SortedSet`
 */
@SerialVersionUID(3L)
object SortedSet extends SortedIterableFactory.Delegate[SortedSet](TreeSet) {
  /** Returns an immutable sorted set containing the elements of `it`, ordered by the given `Ordering`.
   *
   *  If `it` is already a sorted set whose ordering is equal to the requested one it is returned
   *  unchanged; otherwise its elements are copied into a new [[TreeSet]].
   *
   *  @tparam E the element type
   *  @param it the collection whose elements are to be contained
   */
  override def from[E: Ordering](it: IterableOnce[E]^): SortedSet[E] = (it: @unchecked) match {
    case ss: SortedSet[E] if Ordering[E] == ss.ordering => ss
    case _ => super.from(it)
  }
}
