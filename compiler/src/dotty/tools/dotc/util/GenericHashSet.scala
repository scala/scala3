package dotty.tools.dotc.util

import dotty.tools.uncheckedNN

import scala.compiletime.uninitialized

object GenericHashSet:

  /** The number of elements up to which dense packing is used.
   *  If the number of elements reaches `DenseLimit` a hash table is used instead
   */
  inline val DenseLimit = 8

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
abstract class GenericHashSet[T](initialCapacity: Int = 8, capacityMultiple: Int = 2) extends MutableSet[T] {
  import GenericHashSet.DenseLimit

  protected var used: Int = uninitialized
  protected var limit: Int = uninitialized
  protected var table: Array[AnyRef | Null] = uninitialized

  clear()

  private def allocate(capacity: Int) =
    table = new Array[AnyRef | Null](capacity)
    limit = if capacity <= DenseLimit then capacity - 1 else capacity / capacityMultiple

  private def roundToPower(n: Int) =
    if n < 4 then 4
    else 1 << (32 - Integer.numberOfLeadingZeros(n - 1))

  def clear(resetToInitial: Boolean): Unit =
    used = 0
    if resetToInitial then allocate(roundToPower(initialCapacity))
    else java.util.Arrays.fill(table, null)

  /** The number of elements in the set */
  def size: Int = used

  protected def isDense = limit < DenseLimit

  /** Hashcode, to be implemented in subclass */
  protected def hash(key: T): Int

  /** Equality, to be implemented in subclass */
  protected def isEqual(x: T, y: T): Boolean

  /** Turn hashcode `x` into a table index */
  private def index(x: Int): Int = x & (table.length - 1)

  protected def currentTable: Array[AnyRef | Null] = table

  private def firstIndex(x: T) = if isDense then 0 else index(hash(x))
  private def nextIndex(idx: Int) =
    Stats.record(statsItem("miss"))
    index(idx + 1)

  private def entryAt(idx: Int): T | Null = table(idx).asInstanceOf[T | Null]
  private def setEntry(idx: Int, x: T) = table(idx) = x.asInstanceOf[AnyRef | Null]

  def lookup(x: T): T | Null =
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

  def put(x: T): T =
    Stats.record(statsItem("put"))
    var idx = firstIndex(x)
    var e: T | Null = entryAt(idx)
    while e != null do
      // TODO: remove uncheckedNN when explicit-nulls is enabled for regule compiling
      if isEqual(e.uncheckedNN, x) then return e.uncheckedNN
      idx = nextIndex(idx)
      e = entryAt(idx)
    addEntryAt(idx, x)

  def +=(x: T): Unit = put(x)

  def remove(x: T): Boolean =
    Stats.record(statsItem("remove"))
    var idx = firstIndex(x)
    var e: T | Null = entryAt(idx)
    while e != null do
      if isEqual(e.uncheckedNN, x) then
        var hole = idx
        while
          idx = nextIndex(idx)
          e = entryAt(idx)
          e != null
        do
          val eidx = index(hash(e.uncheckedNN))
          if isDense
            || index(eidx - (hole + 1)) > index(idx - (hole + 1))
               // entry `e` at `idx` can move unless `index(hash(e))` is in
               // the (ring-)interval [hole + 1 .. idx]
          then
            setEntry(hole, e.uncheckedNN)
            hole = idx
        table(hole) = null
        used -= 1
        return true
      idx = nextIndex(idx)
      e = entryAt(idx)
    false

  def -=(x: T): Unit =
    remove(x)

  private def addOld(x: T) =
    Stats.record(statsItem("re-enter"))
    var idx = firstIndex(x)
    var e = entryAt(idx)
    while e != null do
      idx = nextIndex(idx)
      e = entryAt(idx)
    setEntry(idx, x)

  def copyFrom(oldTable: Array[AnyRef | Null]): Unit =
    if isDense then
      Array.copy(oldTable, 0, table, 0, oldTable.length)
    else
      var idx = 0
      while idx < oldTable.length do
        val e: T | Null = oldTable(idx).asInstanceOf[T | Null]
        if e != null then addOld(e.uncheckedNN)
        idx += 1

  protected def growTable(): Unit =
    val oldTable = table
    val newLength =
      if oldTable.length == DenseLimit then DenseLimit * 2 * roundToPower(capacityMultiple)
      else table.length * 2
    allocate(newLength)
    copyFrom(oldTable)

  abstract class EntryIterator extends Iterator[T]:
    def entry(idx: Int): T | Null
    private var idx = 0
    def hasNext =
      while idx < table.length && table(idx) == null do idx += 1
      idx < table.length
    def next() =
      require(hasNext)
      try entry(idx).uncheckedNN finally idx += 1

  def iterator: Iterator[T] = new EntryIterator():
    def entry(idx: Int) = entryAt(idx)

  override def toString: String =
    iterator.mkString("HashSet(", ", ", ")")

  protected def statsItem(op: String) =
    val prefix = if isDense then "HashSet(dense)." else "HashSet."
    val suffix = getClass.getSimpleName
    s"$prefix$op $suffix"
}
