package dotty.tools.dotc.util

/** A common class for lightweight mutable sets.
 */
abstract class MutableSet[T] {

  /** The entry in the set such that `isEqual(x, entry)`, or else `null`. */
  def lookup(x: T): T | Null

  /** Add element `x` to the set */
  def +=(x: T): Unit

  /** Like `+=` but return existing element equal to `x` of it exists,
   *  `x` itself otherwose.
   */
  def put(x: T): T

  def clear(): Unit

  def size: Int

  def iterator: Iterator[T]

  def contains(x: T): Boolean = lookup(x) != null

  def foreach[U](f: T => U): Unit = iterator foreach f

  def toList: List[T] = iterator.toList

}
