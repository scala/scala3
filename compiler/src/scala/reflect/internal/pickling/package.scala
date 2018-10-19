package scala.reflect.internal

import dotty.tools.dotc.core.unpickleScala2

package object pickling {
  type PickleBuffer = unpickleScala2.PickleBuffer
  val PickleFormat: unpickleScala2.PickleFormat.type = unpickleScala2.PickleFormat
}
