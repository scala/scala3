package dottyBench.tools.dotc.util

/** A common class for lightweight sets.
 */
abstract class Set[T >: Null] {

  def findEntry(x: T): T

  def addEntry(x: T): Unit

  def iterator: Iterator[T]

  def foreach[U](f: T => U): Unit = iterator foreach f

  def apply(x: T): Boolean = contains(x)

  def contains(x: T): Boolean =
    findEntry(x) != null

  def toList: List[T] = iterator.toList

  def clear(): Unit
}
