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
package runtime

import scala.language.`2.13`

/** A wrapper providing the comparison methods `<`, `<=`, `>`, `>=`, and
 *  `compare` on `Boolean` values, using the ordering in which `false` is
 *  less than `true`.
 *
 *  @param self the wrapped `Boolean` value
 */
@deprecated("use the extension methods available on primitive types instead", since = "3.10.0")
final class RichBoolean(val self: Boolean) extends AnyVal with OrderedProxy[Boolean] {
  /** The `Ordering` evidence for `Boolean`, [[scala.math.Ordering.Boolean]],
   *  which orders `false` before `true`.
   */
  protected def ord: scala.math.Ordering.Boolean.type = scala.math.Ordering.Boolean
}
