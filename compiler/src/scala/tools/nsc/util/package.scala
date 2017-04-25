package scala.tools.nsc

package object util {
  type WeakHashSet[T >: Null <: AnyRef] = dotty.tools.dotc.util.WeakHashSet[T]
}
