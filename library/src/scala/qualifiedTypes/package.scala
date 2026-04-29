package scala
package qualifiedTypes

import annotation.experimental

@experimental
object internal:

  /** Placeholder for a free skolem argument reference in a qualified type annotation.
   *  `index` identifies which call-site argument this represents.
   *  Only appears inside `@qualified` annotation trees; never called at runtime.
   */
  def skolem[T](index: Int): T = ???

end internal
