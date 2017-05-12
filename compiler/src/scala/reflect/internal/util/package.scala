package scala.reflect.internal

package object util {
  type WeakHashSet[T >: Null <: AnyRef] = dotty.tools.dotc.util.WeakHashSet[T]
}
