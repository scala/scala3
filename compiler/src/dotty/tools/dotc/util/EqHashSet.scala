package dotty.tools.dotc.util

import dotty.tools.uncheckedNN

object EqHashSet:

  def from[T](xs: IterableOnce[T]): EqHashSet[T] =
    val set = new EqHashSet[T]()
    set ++= xs
    set

/** A hash set that allows some privileged protected access to its internals
 *  @param  initialCapacity  Indicates the initial number of slots in the hash table.
 *                           The actual number of slots is always a power of 2, so the
 *                           initial size of the table will be the smallest power of two
 *                           that is equal or greater than the given `initialCapacity`.
 *                           Minimum value is 4.
 *  @param  capacityMultiple The minimum multiple of capacity relative to used elements.
 *                           The hash table will be re-sized once the number of elements
 *                           multiplied by capacityMultiple exceeds the current size of the hash table.
 *                           However, a table of size up to DenseLimit will be re-sized only
 *                           once the number of elements reaches the table's size.
 */
class EqHashSet[T](initialCapacity: Int = 8, capacityMultiple: Int = 2) extends GenericHashSet[T](initialCapacity, capacityMultiple) {

  /** System's identity hashcode left shifted by 1 */
  final def hash(key: T): Int =
    System.identityHashCode(key) << 1

  /** reference equality */
  final def isEqual(x: T, y: T): Boolean = x.asInstanceOf[AnyRef] eq y.asInstanceOf[AnyRef]

  /** Turn hashcode `x` into a table index */
  private def index(x: Int): Int = x & (table.length - 1)

  private def firstIndex(x: T) = if isDense then 0 else index(hash(x))
  private def nextIndex(idx: Int) =
    Stats.record(statsItem("miss"))
    index(idx + 1)

  private def entryAt(idx: Int): T | Null = table(idx).asInstanceOf[T | Null]
  private def setEntry(idx: Int, x: T) = table(idx) = x.asInstanceOf[AnyRef | Null]

  override def lookup(x: T): T | Null =
    Stats.record(statsItem("lookup"))
    var idx = firstIndex(x)
    var e: T | Null = entryAt(idx)
    while e != null do
      if isEqual(e.uncheckedNN, x) then return e
      idx = nextIndex(idx)
      e = entryAt(idx)
    null

  /** Add entry at `x` at index `idx` */
  private def addEntryAt(idx: Int, x: T): T =
    Stats.record(statsItem("addEntryAt"))
    setEntry(idx, x)
    used += 1
    if used > limit then growTable()
    x

  /** attempts to put `x` in the Set, if it was not entered before, return true, else return false. */
  override def add(x: T): Boolean =
    Stats.record(statsItem("enter"))
    var idx = firstIndex(x)
    var e: T | Null = entryAt(idx)
    while e != null do
      if isEqual(e.uncheckedNN, x) then return false // already entered
      idx = nextIndex(idx)
      e = entryAt(idx)
    addEntryAt(idx, x)
    true // first entry

  override def put(x: T): T =
    Stats.record(statsItem("put"))
    var idx = firstIndex(x)
    var e: T | Null = entryAt(idx)
    while e != null do
      // TODO: remove uncheckedNN when explicit-nulls is enabled for regule compiling
      if isEqual(e.uncheckedNN, x) then return e.uncheckedNN
      idx = nextIndex(idx)
      e = entryAt(idx)
    addEntryAt(idx, x)

  override def +=(x: T): Unit = put(x)

  private def addOld(x: T) =
    Stats.record(statsItem("re-enter"))
    var idx = firstIndex(x)
    var e = entryAt(idx)
    while e != null do
      idx = nextIndex(idx)
      e = entryAt(idx)
    setEntry(idx, x)

  override def copyFrom(oldTable: Array[AnyRef | Null]): Unit =
    if isDense then
      Array.copy(oldTable, 0, table, 0, oldTable.length)
    else
      var idx = 0
      while idx < oldTable.length do
        val e: T | Null = oldTable(idx).asInstanceOf[T | Null]
        if e != null then addOld(e.uncheckedNN)
        idx += 1
}
