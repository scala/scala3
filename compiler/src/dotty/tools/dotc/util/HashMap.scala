package dotty.tools.dotc.util

/** A specialized implementation of GenericHashMap with standard hashCode and equals
 *  as comparison
 */
class HashMap[Key <: AnyRef, Value]
    (initialCapacity: Int = 8, capacityMultiple: Int = 2)
extends GenericHashMap[Key, Value](initialCapacity, capacityMultiple):
  import GenericHashMap.DenseLimit

  /** Hashcode is left-shifted by 1, so lowest bit is not lost
   *  when taking the index.
   */
  final def hash(x: Key): Int = x.hashCode << 1

  final def isEqual(x: Key, y: Key): Boolean = x.equals(y)

  // The following methods are duplicated from GenericHashMap
  // to avoid polymorphic dispatches

  override def lookup(key: Key): Value | Null =
    Stats.record(statsItem("lookup"))
    var idx = firstIndex(key)
    var k = keyAt(idx)
    while k != null do
      if isEqual(k, key) then return valueAt(idx)
      idx = nextIndex(idx)
      k = keyAt(idx)
    null

  override def update(key: Key, value: Value): Unit =
    Stats.record(statsItem("update"))
    var idx = firstIndex(key)
    var k = keyAt(idx)
    while k != null do
      if isEqual(k, key) then
        setTable(idx + 1, value)
        return
      idx = nextIndex(idx)
      k = keyAt(idx)
    table(idx) = key
    setTable(idx + 1, value)
    used += 1
    if used > limit then growTable()

  private def addOld(key: Key, value: AnyRef): Unit =
    Stats.record(statsItem("re-enter"))
    var idx = firstIndex(key)
    var k = keyAt(idx)
    while k != null do
      idx = nextIndex(idx)
      k = keyAt(idx)
    table(idx) = key
    table(idx + 1) = value

  override def copyFrom(oldTable: Array[AnyRef]): Unit =
    if isDense then
      Array.copy(oldTable, 0, table, 0, oldTable.length)
    else
      var idx = 0
      while idx < oldTable.length do
        val key = oldTable(idx).asInstanceOf[Key]
        if key != null then addOld(key, oldTable(idx + 1))
        idx += 2
end HashMap
