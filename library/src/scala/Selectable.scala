/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package scala
import scala.reflect.ClassTag

trait Selectable extends Any {
  def selectDynamic(name: String): Any
  def applyDynamic(name: String, paramClasses: ClassTag[_]*)(args: Any*): Any =
    new UnsupportedOperationException("applyDynamic")
}
