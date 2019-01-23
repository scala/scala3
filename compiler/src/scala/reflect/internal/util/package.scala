/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package scala.reflect.internal

package object util {
  type WeakHashSet[T >: Null <: AnyRef] = dotty.tools.dotc.util.WeakHashSet[T]
}
