package dotty.tools
package dotc.util

/** A common class for lightweight mutable maps.
 */
abstract class MutableMap[Key <: AnyRef, Value]:

  def lookup(x: Key): Value | Null

  def update(k: Key, v: Value): Unit

  def remove(k: Key): Unit

  def size: Int

  def clear(): Unit

  def iterator: Iterator[(Key, Value)]

  def get(x: Key): Option[Value] = lookup(x) match
    case null => None
    case v => Some(v.uncheckedNN)
