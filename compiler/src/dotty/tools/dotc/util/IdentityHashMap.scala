package dotty.tools.dotc.util

/** A specialized implementation of GenericHashMap with identity hash and `eq`
 *  as comparison.
 */
class IdentityHashMap[Key <: AnyRef, Value >: Null <: AnyRef]
    (initialCapacity: Int = 8, capacityMultiple: Int = 3)
extends GenericHashMap[Key, Value](initialCapacity, capacityMultiple):
  import GenericHashMap.DenseLimit

  /** Hashcode, by default `System.identityHashCode`, but can be overriden */
  final def hash(x: Key): Int = System.identityHashCode(x)

  /** Equality, by default `eq`,  but can be overridden */
  final def isEqual(x: Key, y: Key): Boolean = x eq y

  // The following methdods are duplicated from GenericHashMap
  // to avoid polymorphic dispatches

  /** Turn hashcode `x` into a table index */
  private def index(x: Int): Int = x & (table.length - 2)

  private def firstIndex(key: Key) = if isDense then 0 else index(hash(key))
  private def nextIndex(idx: Int) = index(idx + 2)

  private def keyAt(idx: Int): Key = table(idx).asInstanceOf[Key]
  private def valueAt(idx: Int): Value = table(idx + 1).asInstanceOf[Value]

  override def lookup(key: Key): Value =
    var idx = firstIndex(key)
    var k = keyAt(idx)
    while k != null do
      if isEqual(k, key) then return valueAt(idx)
      idx = nextIndex(idx)
      k = keyAt(idx)
    null

  override def update(key: Key, value: Value): Unit =
    var idx = firstIndex(key)
    var k = keyAt(idx)
    while k != null do
      if isEqual(k, key) then
        table(idx + 1) = value
        return
      idx = nextIndex(idx)
      k = keyAt(idx)
    table(idx) = key
    table(idx + 1) = value
    used += 1
    if used > limit then growTable()
end IdentityHashMap
