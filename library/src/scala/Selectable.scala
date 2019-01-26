/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
import scala.reflect.ClassTag

trait Selectable extends Any {
  def selectDynamic(name: String): Any
  def applyDynamic(name: String, paramClasses: ClassTag[_]*)(args: Any*): Any =
    new UnsupportedOperationException("applyDynamic")
}
