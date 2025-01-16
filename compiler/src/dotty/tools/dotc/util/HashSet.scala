package dotty.tools.dotc.util

import dotty.tools.uncheckedNN

object HashSet:

  def from[T](xs: IterableOnce[T]): HashSet[T] =
    val set = new HashSet[T]()
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
class HashSet[T](initialCapacity: Int = 8, capacityMultiple: Int = 2) extends GenericHashSet[T](initialCapacity, capacityMultiple) {

  /** Hashcode, by default a processed `x.hashCode`, can be overridden */
  protected def hash(key: T): Int =
    val h = key.hashCode
    // Part of the MurmurHash3 32 bit finalizer
    val i = (h ^ (h >>> 16)) * 0x85EBCA6B
    val j = (i ^ (i >>> 13)) & 0x7FFFFFFF
    if j==0 then 0x41081989 else j

  /** Hashcode, by default `equals`, can be overridden */
  protected def isEqual(x: T, y: T): Boolean = x.equals(y)

  /** Turn hashcode `x` into a table index */
  protected def index(x: Int): Int = x & (table.length - 1)

  protected def firstIndex(x: T) = if isDense then 0 else index(hash(x))
  protected def nextIndex(idx: Int) =
    Stats.record(statsItem("miss"))
    index(idx + 1)

  protected def entryAt(idx: Int): T | Null = table(idx).asInstanceOf[T | Null]
  protected def setEntry(idx: Int, x: T) = table(idx) = x.asInstanceOf[AnyRef | Null]

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
  protected def addEntryAt(idx: Int, x: T): T =
    Stats.record(statsItem("addEntryAt"))
    setEntry(idx, x)
    used += 1
    if used > limit then growTable()
    x

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
