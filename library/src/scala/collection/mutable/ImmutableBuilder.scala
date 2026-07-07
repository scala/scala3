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

  protected var elems: C = empty

  def clear(): Unit = { elems = empty }

  def result(): C = elems

  override def knownSize: Int = elems.knownSize
}
