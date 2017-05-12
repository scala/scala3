package scala.reflect.internal

package object pickling {
  type PickleBuffer = dotty.tools.dotc.core.unpickleScala2.PickleBuffer
  val PickleFormat = dotty.tools.dotc.core.unpickleScala2.PickleFormat
}
