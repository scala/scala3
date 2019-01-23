/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package scala.tasty.file

import scala.tasty.Reflection

trait TastyConsumer {
  def apply(reflect: Reflection)(root: reflect.Tree): Unit
}
