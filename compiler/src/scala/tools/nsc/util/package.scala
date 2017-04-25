package scala.tools.nsc

package object util {
  type WeakHashSet[T >: Null <: AnyRef] = dotty.tools.dotc.util.WeakHashSet[T]

  object pickling {
    type PickleBuffer = dotty.tools.dotc.core.unpickleScala2.PickleBuffer
    val PickleFormat = dotty.tools.dotc.core.unpickleScala2.PickleFormat
  }
}
