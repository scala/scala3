package dotty.tools.dotc.util

object HashTable:
  inline val MaxDense = 8

/** A hash table using open hashing with linear scan which is also very space efficient
 *  at small sizes.
 *  @param  initialCapacity  Indicates the initial number of slots in the hash table.
 *                           The actual number of slots is always a power of 2, so the
 *                           initial size of the table will be the smallest power of two
 *                           that is equal or greater than the given `initialCapacity`.
 *                           Minimum value is 4.
 *  @param  loadFactor       The maximum fraction of used elements relative to capacity.
 *                           The hash table will be re-sized once the number of elements exceeds
 *                           the current size of the hash table multiplied by loadFactor.
 *                           However, a table of size up to MaxDense will be re-sized to only
 *                           once the number of elements reaches the table's size.
 */
class HashTable[Key >: Null <: AnyRef, Value >: Null <: AnyRef]
    (initialCapacity: Int = 8: Int, loadFactor: Float = 0.33f):
  import HashTable.MaxDense
  private var used: Int = _
  private var limit: Int = _
  private var table: Array[AnyRef] = _
  clear()

  private def allocate(capacity: Int) =
    table = new Array[AnyRef](capacity * 2)
    limit = if capacity <= MaxDense then capacity - 1 else (capacity * loadFactor).toInt

  private def roundToPower(n: Int) =
    if Integer.bitCount(n) == 1 then n
    else
      def recur(n: Int): Int =
        if n == 1 then 2
        else recur(n >>> 1) << 1
      recur(n)

  /** Remove all elements from this table and set back to initial configuration */
  def clear(): Unit =
    used = 0
    allocate(roundToPower(initialCapacity max 4))

  /** The number of elements in the set */
  def size: Int = used

  private def isDense = limit < MaxDense

  /** Hashcode, by default `System.identityHashCode`, but can be overriden */
  protected def hash(x: Key): Int = System.identityHashCode(x)

  /** Equality, by default `eq`,  but can be overridden */
  protected def isEqual(x: Key, y: Key): Boolean = x eq y

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

  def enter(key: Key, value: Value): Unit =
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

  def invalidate(key: Key): Unit =
    var idx = firstIndex(key)
    var k = keyAt(idx)
    while k != null do
      if isEqual(k, key) then
        var hole = idx
        if !isDense then
          while
            idx = nextIndex(idx)
            k = keyAt(idx)
            k != null && index(hash(k)) != idx
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

  private def growTable(): Unit =
    val oldTable = table
    allocate(table.length)
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
end HashTable
