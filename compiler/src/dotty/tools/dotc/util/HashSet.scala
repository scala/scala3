package dotty.tools.dotc.util

object HashSet:

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
class HashSet[T >: Null <: AnyRef](initialCapacity: Int = 8, capacityMultiple: Int = 2) extends MutableSet[T] {
  import HashSet.DenseLimit

  private var used: Int = _
  private var limit: Int = _
  private var table: Array[AnyRef] = _

  clear()

  private def allocate(capacity: Int) =
    table = new Array[AnyRef](capacity)
    limit = if capacity <= DenseLimit then capacity - 1 else capacity / capacityMultiple

  private def roundToPower(n: Int) =
    if n < 4 then 4
    else if Integer.bitCount(n) == 1 then n
    else 1 << (32 - Integer.numberOfLeadingZeros(n))

  /** Remove all elements from this set and set back to initial configuration */
  def clear(): Unit = {
    used = 0
    allocate(roundToPower(initialCapacity))
  }

  /** The number of elements in the set */
  def size: Int = used

  protected def isDense = limit < DenseLimit

  /** Hashcode, by defualt `x.hashCode`, can be overridden */
  protected def hash(x: T): Int = x.hashCode

  /** Hashcode, by default `equals`, can be overridden */
  protected def isEqual(x: T, y: T): Boolean = x.equals(y)

  /** Turn hashcode `x` into a table index */
  protected def index(x: Int): Int = x & (table.length - 1)

  protected def currentTable: Array[AnyRef] = table

  protected def firstIndex(x: T) = if isDense then 0 else index(hash(x))
  protected def nextIndex(idx: Int) =
    Stats.record(statsItem("miss"))
    index(idx + 1)

  protected def entryAt(idx: Int) = table(idx).asInstanceOf[T]

  def lookup(x: T): T =
    Stats.record(statsItem("lookup"))
    var idx = firstIndex(x)
    var e = entryAt(idx)
    while e != null do
      if isEqual(e, x) then return e
      idx = nextIndex(idx)
      e = entryAt(idx)
    null

/** Add entry at `x` at index `idx` */
  protected def addEntryAt(idx: Int, x: T): T =
    Stats.record(statsItem("addEntryAt"))
    table(idx) = x
    used += 1
    if used > limit then growTable()
    x

  def put(x: T): T =
    Stats.record(statsItem("put"))
    var idx = firstIndex(x)
    var e = entryAt(idx)
    while e != null do
      if isEqual(e, x) then return e
      idx = nextIndex(idx)
      e = entryAt(idx)
    addEntryAt(idx, x)

  def +=(x: T): Unit = put(x)

  def -= (x: T): Unit =
    Stats.record(statsItem("remove"))
    var idx = firstIndex(x)
    var e = entryAt(idx)
    while e != null do
      if isEqual(e, x) then
        var hole = idx
        while
          idx = nextIndex(idx)
          e = entryAt(idx)
          e != null && (isDense || index(hash(e)) != idx)
        do
          if isDense
            || index(hole - index(hash(e))) < limit
               // hash(k) is then logically at or before hole; can be moved forward to fill hole
          then
            table(hole) = e
            hole = idx
        table(hole) = null
        used -= 1
        return
      idx = nextIndex(idx)
      e = entryAt(idx)

  private def addOld(x: T) =
    Stats.record(statsItem("re-enter"))
    var idx = firstIndex(x)
    var e = entryAt(idx)
    while e != null do
      idx = nextIndex(idx)
      e = entryAt(idx)
    table(idx) = x

  def copyFrom(oldTable: Array[AnyRef]): Unit =
    if isDense then
      Array.copy(oldTable, 0, table, 0, oldTable.length)
    else
      var idx = 0
      while idx < oldTable.length do
        val e = oldTable(idx).asInstanceOf[T]
        if e != null then addOld(e)
        idx += 1

  protected def growTable(): Unit =
    val oldTable = table
    val newLength =
      if oldTable.length == DenseLimit then DenseLimit * 2 * roundToPower(capacityMultiple)
      else table.length * 2
    allocate(newLength)
    copyFrom(oldTable)

  abstract class EntryIterator extends Iterator[T]:
    def entry(idx: Int): T
    private var idx = 0
    def hasNext =
      while idx < table.length && table(idx) == null do idx += 1
      idx < table.length
    def next() =
      require(hasNext)
      try entry(idx) finally idx += 1

  def iterator: Iterator[T] = new EntryIterator():
    def entry(idx: Int) = entryAt(idx)

  override def toString: String =
    iterator.mkString("HashSet(", ", ", ")")

  protected def statsItem(op: String) =
    val prefix = if isDense then "HashSet(dense)." else "HashSet."
    val suffix = getClass.getSimpleName
    s"$prefix$op $suffix"
}
