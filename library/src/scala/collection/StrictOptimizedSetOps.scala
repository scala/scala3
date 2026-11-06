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
import language.experimental.captureChecking

/** Trait that overrides set operations to take advantage of strict builders.
 *
 *  @tparam A  Elements type
 *  @tparam CC Collection type constructor
 *  @tparam C  Collection type
 */
transparent trait StrictOptimizedSetOps[A, +CC[_], +C <: SetOps[A, CC, C]]
  extends SetOps[A, CC, C]
    with StrictOptimizedIterableOps[A, CC, C] {

  /** Returns a new set containing the elements of this set followed by the
   *  elements of `that`.
   *
   *  Overrides the default view-based implementation to fill a strict builder
   *  directly, which is slightly faster.
   *
   *  @param that the elements to add
   *  @return a new set containing all elements of this set and of `that`
   */
  override def concat(that: IterableOnce[A]^): C =
    strictOptimizedConcat(that, newSpecificBuilder)

}
