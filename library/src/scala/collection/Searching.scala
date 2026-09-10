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

import scala.language.`2.13`
import scala.language.implicitConversions
import language.experimental.captureChecking

import scala.collection.generic.IsSeq

object Searching {

  /** The result of performing a search on a sorted sequence
    *
    * Example usage:
    *
    * ```scala sc:compile
    *   val list = List(1, 3, 4, 5) // list must be sorted before searching
    *   list.search(4) // Found(2)
    *   list.search(2) // InsertionPoint(1)
    * ```
    */
  sealed abstract class SearchResult {
    /** The index corresponding to the element searched for in the sequence, if it was found,
      * or the index where the element would be inserted in the sequence, if it was not in the sequence */
    def insertionPoint: Int
  }

  /** The result of performing a search on a sorted sequence, where the element was found.
    *
    * @param foundIndex the index corresponding to the element searched for in the sequence
    */
  case class Found(foundIndex: Int) extends SearchResult {
    /** Returns `foundIndex`, the index at which the searched element was found. */
    override def insertionPoint: Int = foundIndex
  }

  /** The result of performing a search on a sorted sequence, where the element was not found
    *
    * @param insertionPoint the index where the element would be inserted in the sequence
    */
  case class InsertionPoint(insertionPoint: Int) extends SearchResult

  /** A value class that formerly provided the `search` methods for sequences. The search
   *  methods are now defined directly on [[SeqOps]], so this class defines no methods of
   *  its own.
   *
   *  @tparam Repr the type of the collection being searched; never used
   *  @tparam A the element type of the wrapped sequence
   *  @param coll the wrapped sequence; never used
   */
  @deprecated("Search methods are defined directly on SeqOps and do not require scala.collection.Searching any more", "2.13.0")
  class SearchImpl[Repr, A](private val coll: SeqOps[A, AnyConstr, ?]) extends AnyVal

  /** Converts a collection to a [[SearchImpl]] over its sequence view.
   *
   *  @tparam Repr the type of the collection to convert
   *  @tparam A never used; the element type of the result is `fr.A`
   *  @param coll the collection to convert
   *  @param fr evidence that `Repr` can be viewed as a sequence, determining the
   *            element type `fr.A`
   *  @return a `SearchImpl` wrapping the sequence view `fr.conversion(coll)`
   */
  @deprecated("Search methods are defined directly on SeqOps and do not require scala.collection.Searching any more", "2.13.0")
  implicit def search[Repr, A](coll: Repr)(implicit fr: IsSeq[Repr]): SearchImpl[Repr, fr.A] =
    new SearchImpl(fr.conversion(coll))
}
