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
package mutable

import scala.language.`2.13`
import language.experimental.captureChecking

/** Reusable builder for immutable collections
 *
 *  @tparam A the element type of the collection being built
 *  @tparam C the type of the immutable collection to build (must be a subtype of `IterableOnce[?]`)
 *  @param empty the empty collection used as the initial value and reset target when the builder is cleared
 */
abstract class ImmutableBuilder[-A, C <: IterableOnce[?]](empty: C)
  extends ReusableBuilder[A, C] {

  /** The immutable collection accumulated so far. Since the collection is
   *  immutable, subclasses accumulate elements by replacing this value with a
   *  new collection extended with the added element.
   */
  protected var elems: C = empty

  /** Clears this builder by restoring `elems` to the empty collection given at construction. */
  def clear(): Unit = { elems = empty }

  /** Returns the collection accumulated so far. As this is a reusable builder, the
   *  builder remains usable afterwards, and later additions replace the accumulator
   *  rather than modifying it, so a collection already returned is unaffected. That
   *  holds for the immutable collections this is used with; a subclass whose `addOne`
   *  mutates the accumulator in place would break it.
   */
  def result(): C = elems

  /** Returns the number of elements accumulated so far, if it can be cheaply
   *  computed, -1 otherwise.
   */
  override def knownSize: Int = elems.knownSize
}
