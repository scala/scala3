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

package scala.reflect.internal

import dotty.tools.dotc.core.unpickleScala2

package object pickling {
  type PickleBuffer = unpickleScala2.PickleBuffer
  val PickleFormat: unpickleScala2.PickleFormat.type = unpickleScala2.PickleFormat
}
