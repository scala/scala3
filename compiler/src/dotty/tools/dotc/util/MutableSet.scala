package dotty.tools.dotc.util

/** A common class for lightweight mutable sets.
 */
abstract class MutableSet[T >: Null] {

  /** The entry in the set such that `isEqual(x, entry)`, or else `null`. */
  def lookup(x: T): T /* | Null */

  def +=(x: T): Unit

  def clear(): Unit

  def size: Int

  def iterator: Iterator[T]

  def contains(x: T): Boolean = lookup(x) != null

  def foreach[U](f: T => U): Unit = iterator foreach f

  def toList: List[T] = iterator.toList

}
