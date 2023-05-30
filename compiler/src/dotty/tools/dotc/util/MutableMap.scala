package dotty.tools
package dotc.util

/** A common class for lightweight mutable maps.
 */
abstract class MutableMap[Key, Value] extends ReadOnlyMap[Key, Value]:

  def update(k: Key, v: Value): Unit

  def remove(k: Key): Value | Null

  def -=(k: Key): this.type =
    remove(k)
    this

  /** Remove all bindings from this map.
   *  @param resetToInitial If true, set back to initial configuration, which includes
   *                        reallocating tables.
   */
  def clear(resetToInitial: Boolean = true): Unit

  def getOrElseUpdate(key: Key, value: => Value): Value
