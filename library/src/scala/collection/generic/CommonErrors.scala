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

/** Some precomputed common errors to reduce the generated code size. */
private[collection] object CommonErrors {
  /** IndexOutOfBounds exception with a known max index.
   *
   *  @param index the index that was out of bounds
   *  @param max the upper bound of the valid index range
   *  @return a new `IndexOutOfBoundsException` whose message reports `index` along with the valid range from `0` to `max`
   */
  @noinline
  def indexOutOfBounds(index: Int, max: Int): IndexOutOfBoundsException = 
    new IndexOutOfBoundsException(s"$index is out of bounds (min 0, max ${max})")

  /** IndexOutOfBounds exception with an unknown max index.
   *
   *  @param index the index that was out of bounds
   *  @return a new `IndexOutOfBoundsException` with a message reporting `index` and an unknown upper bound
   */
  @noinline
  def indexOutOfBounds(index: Int): IndexOutOfBoundsException = 
    new IndexOutOfBoundsException(s"$index is out of bounds (min 0, max unknown)")
}
