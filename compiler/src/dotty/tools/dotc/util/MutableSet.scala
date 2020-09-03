package dotty.tools.dotc.util

/** A common class for lightweight mutable sets.
 */
abstract class MutableSet[T] extends ReadOnlySet[T]:

  /** Add element `x` to the set */
  def +=(x: T): Unit

  /** Like `+=` but return existing element equal to `x` of it exists,
   *  `x` itself otherwose.
   */
  def put(x: T): T

  def clear(): Unit

