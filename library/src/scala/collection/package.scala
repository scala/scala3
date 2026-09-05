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

import scala.language.`2.13`
import language.experimental.captureChecking

package object collection {
  @deprecated("Use Iterable instead of Traversable", "2.13.0")
  type Traversable[+X] = Iterable[X]
  /** Alias for the [[Iterable]] companion object, kept so that code written against
   *  the old name can still call factory methods such as `Traversable(1, 2, 3)`.
   */
  @deprecated("Use Iterable instead of Traversable", "2.13.0")
  val Traversable = Iterable
  @deprecated("Use IterableOnce instead of TraversableOnce", "2.13.0")
  type TraversableOnce[+X] = IterableOnce[X]
  /** Alias for the [[IterableOnce]] companion object, kept so that code written
   *  against the old name still resolves.
   */
  @deprecated("Use IterableOnce instead of TraversableOnce", "2.13.0")
  val TraversableOnce = IterableOnce
  @deprecated("Use SeqOps instead of SeqLike", "2.13.0")
  type SeqLike[A, T] = SeqOps[A, Seq, T]
  @deprecated("Use SeqOps (for the methods) or IndexedSeqOps (for fast indexed access) instead of ArrayLike", "2.13.0")
  type ArrayLike[A] = SeqOps[A, Seq, Seq[A]]

  @deprecated("Gen* collection types have been removed", "2.13.0")
  type GenTraversableOnce[+X] = IterableOnce[X]
  /** Alias for the [[IterableOnce]] companion object, standing in for the removed
   *  `GenTraversableOnce` companion.
   */
  @deprecated("Gen* collection types have been removed", "2.13.0")
  val GenTraversableOnce = IterableOnce
  @deprecated("Gen* collection types have been removed", "2.13.0")
  type GenTraversable[+X] = Iterable[X]
  /** Alias for the [[Iterable]] companion object, standing in for the removed
   *  `GenTraversable` companion.
   */
  @deprecated("Gen* collection types have been removed", "2.13.0")
  val GenTraversable = Iterable
  @deprecated("Gen* collection types have been removed", "2.13.0")
  type GenIterable[+X] = Iterable[X]
  /** Alias for the [[Iterable]] companion object, standing in for the removed
   *  `GenIterable` companion.
   */
  @deprecated("Gen* collection types have been removed", "2.13.0")
  val GenIterable = Iterable
  @deprecated("Gen* collection types have been removed", "2.13.0")
  type GenSeq[+X] = Seq[X]
  /** Alias for the [[Seq]] companion object, standing in for the removed
   *  `GenSeq` companion.
   */
  @deprecated("Gen* collection types have been removed", "2.13.0")
  val GenSeq = Seq
  @deprecated("Gen* collection types have been removed", "2.13.0")
  type GenSet[X] = Set[X]
  /** Alias for the [[Set]] companion object, standing in for the removed
   *  `GenSet` companion.
   */
  @deprecated("Gen* collection types have been removed", "2.13.0")
  val GenSet = Set
  @deprecated("Gen* collection types have been removed", "2.13.0")
  type GenMap[K, +V] = Map[K, V]
  /** Alias for the [[Map]] companion object, standing in for the removed
   *  `GenMap` companion.
   */
  @deprecated("Gen* collection types have been removed", "2.13.0")
  val GenMap = Map

  /** Needed to circumvent a difficulty between dotty and scalac concerning
   *  the right top type for a type parameter of kind * -> *.
   *  In Scalac, we can provide `Any`, as `Any` is kind-polymorphic. In dotty this is not allowed.
   *  In dotty, we can provide `[X] => Any`. But Scalac does not know lambda syntax.
   */
  private[scala] type AnyConstr[X] = Any

  /** An extractor used to head/tail deconstruct sequences. */
  object +: {
    /** Splits a sequence into head +: tail.
     *
     *  @tparam A the element type of the sequence
     *  @tparam CC the type constructor of the sequence (e.g., `List`, `Vector`)
     *  @tparam C the concrete sequence type, providing `SeqOps[A, CC, C]` for `head`/`tail` operations
     *  @param t the sequence to deconstruct into its head and tail
     *  @return `Some((head, tail))` if the sequence is non-empty, `None` otherwise
     */
    def unapply[A, CC[_] <: Seq[?], C <: SeqOps[A, CC, C]](t: (C & SeqOps[A, CC, C])^): Option[(A, C^{t})] =
      if(t.isEmpty) None
      else Some(t.head -> t.tail)
  }

  /** An extractor used to init/last deconstruct sequences. */
  object :+ {
    /** Splits a sequence into init :+ last.
     *
     *  @tparam A the element type of the sequence
     *  @tparam CC the type constructor of the sequence (e.g., `List`, `Vector`)
     *  @tparam C the concrete sequence type, providing `SeqOps[A, CC, C]` for `init`/`last` operations
     *  @param t the sequence to deconstruct into its init and last element
     *  @return `Some((init, last))` if the sequence is non-empty, `None` otherwise
     */
    def unapply[A, CC[_] <: Seq[?], C <: SeqOps[A, CC, C]](t: (C & SeqOps[A, CC, C])^): Option[(C^{t}, A)] =
      if(t.isEmpty) None
      else Some(t.init -> t.last)
  }
}
