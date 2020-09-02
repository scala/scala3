package dotty.tools
package dotc.util

/** A class for the reading part of mutable or immutable maps.
 */
abstract class ReadOnlyMap[Key, Value]:

  def lookup(x: Key): Value | Null

  def size: Int

  def iterator: Iterator[(Key, Value)]
  def keysIterator: Iterator[Key]

  def isEmpty: Boolean = size == 0

  def get(key: Key): Option[Value] = lookup(key) match
    case null => None
    case v => Some(v.uncheckedNN)

  def getOrElse(key: Key, value: => Value) = lookup(key) match
    case null => value
    case v => v.uncheckedNN

  def contains(key: Key): Boolean = lookup(key) != null

  def apply(key: Key): Value = lookup(key) match
    case null => throw new NoSuchElementException(s"$key")
    case v => v.uncheckedNN