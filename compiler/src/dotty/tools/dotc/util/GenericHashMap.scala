package dotty.tools.dotc.util

object GenericHashMap:

  /** The number of elements up to which dense packing is used.
   *  If the number of elements reaches `DenseLimit` a hash table is used instead
   */
  inline val DenseLimit = 8

/** A hash table using open hashing with linear scan which is also very space efficient
 *  at small sizes. The implementations of `hash` and `isEqual` are left open. They have
 *  to be provided by subclasses.
 *
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
abstract class GenericHashMap[Key >: Null <: AnyRef, Value >: Null <: AnyRef]
    (protected val initialCapacity: Int = 8,
     protected val capacityMultiple: Int = 3) extends Map[Key, Value]:
  import GenericHashMap.DenseLimit

  protected var used: Int = _
  protected var limit: Int = _
  protected var table: Array[AnyRef] = _
  clear()

  private def allocate(capacity: Int) =
    table = new Array[AnyRef](capacity * 2)
    limit = if capacity <= DenseLimit then capacity - 1 else capacity / capacityMultiple

  private def roundToPower(n: Int) =
    if n < 4 then 4
    else if Integer.bitCount(n) == 1 then n
    else 1 << (32 - Integer.numberOfLeadingZeros(n))

  /** Remove all elements from this table and set back to initial configuration */
  def clear(): Unit =
    used = 0
    allocate(roundToPower(initialCapacity))

  /** The number of elements in the set */
  def size: Int = used

  protected def isDense = limit < DenseLimit

  /** Hashcode, to be implemented in subclass */
  protected def hash(x: Key): Int

  /** Equality, to be implemented in subclass */
  protected def isEqual(x: Key, y: Key): Boolean

  /** Turn hashcode `x` into a table index */
  private def index(x: Int): Int = x & (table.length - 2)

  private def firstIndex(key: Key) = if isDense then 0 else index(hash(key))
  private def nextIndex(idx: Int) = index(idx + 2)

  private def keyAt(idx: Int): Key = table(idx).asInstanceOf[Key]
  private def valueAt(idx: Int): Value = table(idx + 1).asInstanceOf[Value]

  /** Find entry such that `isEqual(x, entry)`. If it exists, return it.
   *  If not, enter `x` in set and return `x`.
   */
  def lookup(key: Key): Value =
    var idx = firstIndex(key)
    var k = keyAt(idx)
    while k != null do
      if isEqual(k, key) then return valueAt(idx)
      idx = nextIndex(idx)
      k = keyAt(idx)
    null

  def update(key: Key, value: Value): Unit =
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

  def remove(key: Key): Unit =
    var idx = firstIndex(key)
    var k = keyAt(idx)
    while k != null do
      if isEqual(k, key) then
        var hole = idx
        while
          idx = nextIndex(idx)
          k = keyAt(idx)
          k != null && (isDense || index(hash(k)) != idx)
        do
          table(hole) = k
          table(hole + 1) = valueAt(idx)
          hole = idx
        table(hole) = null
        used -= 1
        return
      idx = nextIndex(idx)
      k = keyAt(idx)

  private def addOld(key: Key, value: AnyRef): Unit =
    var idx = firstIndex(key)
    var k = keyAt(idx)
    while k != null do
      idx = nextIndex(idx)
      k = keyAt(idx)
    table(idx) = key
    table(idx + 1) = value

  protected def growTable(): Unit =
    val oldTable = table
    val newLength =
      if oldTable.length == DenseLimit then DenseLimit * 2 * roundToPower(capacityMultiple)
      else table.length
    allocate(newLength)
    if isDense then
      Array.copy(oldTable, 0, table, 0, oldTable.length)
    else
      var idx = 0
      while idx < oldTable.length do
        val key = oldTable(idx).asInstanceOf[Key]
        if key != null then addOld(key, oldTable(idx + 1))
        idx += 2

  def iterator: Iterator[(Key, Value)] =
    for idx <- (0 until table.length by 2).iterator
        if keyAt(idx) != null
    yield (keyAt(idx), valueAt(idx))

  override def toString: String =
    iterator.map((k, v) => s"$k -> $v").mkString("LinearTable(", ", ", ")")
end GenericHashMap
