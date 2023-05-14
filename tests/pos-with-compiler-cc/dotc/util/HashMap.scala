package dotty.tools.dotc.util

import scala.language.unsafeNulls

/** A specialized implementation of GenericHashMap with standard hashCode and equals
 *  as comparison
 */
class HashMap[Key, Value]
    (initialCapacity: Int = 8, capacityMultiple: Int = 2)
extends GenericHashMap[Key, Value](initialCapacity, capacityMultiple):

  /** Hashcode is left-shifted by 1, so lowest bit is not lost
   *  when taking the index.
   */
  final def hash(key: Key): Int =
    val h = key.hashCode
    // Part of the MurmurHash3 32 bit finalizer
    val i = (h ^ (h >>> 16)) * 0x85EBCA6B
    val j = (i ^ (i >>> 13)) & 0x7FFFFFFF
    (if j==0 then 0x41081989 else j) << 1

  final def isEqual(x: Key, y: Key): Boolean = x.equals(y)

  // The following methods are duplicated from GenericHashMap
  // to avoid polymorphic dispatches
  private def index(x: Int): Int = x & (table.length - 2)

  private def firstIndex(key: Key) = if isDense then 0 else index(hash(key))
  private def nextIndex(idx: Int) =
    Stats.record(statsItem("miss"))
    index(idx + 2)

  private def keyAt(idx: Int): Key = table(idx).asInstanceOf[Key]
  private def valueAt(idx: Int): Value = table(idx + 1).asInstanceOf[Value]

  private def setKey(idx: Int, key: Key) =
    table(idx) = key.asInstanceOf[AnyRef]
  private def setValue(idx: Int, value: Value) =
    table(idx + 1) = value.asInstanceOf[AnyRef]

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
        setValue(idx, value)
        return
      idx = nextIndex(idx)
      k = keyAt(idx)
    setKey(idx, key)
    setValue(idx, value)
    used += 1
    if used > limit then growTable()

  private def addOld(key: Key, value: Value): Unit =
    Stats.record(statsItem("re-enter"))
    var idx = firstIndex(key)
    var k = keyAt(idx)
    while k != null do
      idx = nextIndex(idx)
      k = keyAt(idx)
    setKey(idx, key)
    setValue(idx, value)

  override def copyFrom(oldTable: Array[AnyRef | Null]): Unit =
    if isDense then
      Array.copy(oldTable, 0, table, 0, oldTable.length)
    else
      var idx = 0
      while idx < oldTable.length do
        val key = oldTable(idx).asInstanceOf[Key]
        if key != null then addOld(key, oldTable(idx + 1).asInstanceOf[Value])
        idx += 2
end HashMap
