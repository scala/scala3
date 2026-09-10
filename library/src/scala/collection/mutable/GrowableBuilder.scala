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
package collection.mutable

import scala.language.`2.13`
import language.experimental.captureChecking

/** The canonical builder for collections that are growable, i.e. that support an
 *  efficient `+=` method which adds an element to the collection.
 *
 *  GrowableBuilders can produce only a single instance of the collection they are growing.
 *
 *  @define Coll `GrowingBuilder`
 *  @define coll growing builder
 *
 *  @tparam Elem the type of elements that can be added to the builder
 *  @tparam To the type of the resulting growable collection, which must be a subtype of `Growable[Elem]`
 *  @param elems the underlying growable collection that elements are added to and which is returned as the result
 */
class GrowableBuilder[Elem, To <: Growable[Elem]](protected val elems: To)
  extends Builder[Elem, To] {

  /** Removes all elements from the underlying collection. */
  def clear(): Unit = elems.clear()

  /** Returns the underlying growable collection itself, not a copy. Elements
   *  added to this builder afterwards are visible in the returned collection.
   */
  def result(): To = elems

  /** Adds a single element to the underlying collection.
   *
   *  @param elem the element to add
   *  @return this builder
   */
  def addOne(elem: Elem): this.type = { elems += elem; this }

  /** Adds all elements produced by an `IterableOnce` to the underlying collection.
   *
   *  @param xs the elements to add
   *  @return this builder
   */
  override def addAll(xs: IterableOnce[Elem]^): this.type = { elems.addAll(xs); this }

  /** Returns the number of elements in the underlying collection, if it can be
   *  cheaply computed, -1 otherwise.
   */
  override def knownSize: Int = elems.knownSize
}
