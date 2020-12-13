package dotty.tools.dotc.util

/** A class for the readonly part of mutable sets.
 */
abstract class ReadOnlySet[T]:

  /** The entry in the set such that `isEqual(x, entry)`, or else `null`. */
  def lookup(x: T): T | Null

  def size: Int

  def iterator: Iterator[T]

  def contains(x: T): Boolean = lookup(x) != null

  def foreach[U](f: T => U): Unit = iterator.foreach(f)

  def toList: List[T] = iterator.toList

  def isEmpty = size == 0

object ReadOnlySet:
  def empty[T]: ReadOnlySet[T] = HashSet[T](4)

