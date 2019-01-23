/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package scala.reflect.internal

import dotty.tools.dotc.core.unpickleScala2

package object pickling {
  type PickleBuffer = unpickleScala2.PickleBuffer
  val PickleFormat: unpickleScala2.PickleFormat.type = unpickleScala2.PickleFormat
}
