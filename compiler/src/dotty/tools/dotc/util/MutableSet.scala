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

  /** Remove element `x` from the set */
  def -=(x: T): Unit

  def clear(): Unit

  def ++= (xs: IterableOnce[T]): Unit =
    xs.iterator.foreach(this += _)

  def --= (xs: IterableOnce[T]): Unit =
    xs.iterator.foreach(this -= _)

