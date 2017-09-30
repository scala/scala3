package dotty.tools.dotc.util

/** A hash set that allows some privileged protected access to its internals
 */
class HashSet[T >: Null <: AnyRef](powerOfTwoInitialCapacity: Int, loadFactor: Float = 0.25f) extends Set[T] {
  private var used: Int = _
  private var limit: Int = _
  private var table: Array[AnyRef] = _

  clear()

  /** The number of elements in the set */
  def size: Int = used

  private def allocate(size: Int) = {
    table = new Array[AnyRef](size)
    limit = (size * loadFactor).toInt
  }

  /** Remove all elements from this set and set back to initial configuration */
  def clear(): Unit = {
    used = 0
    allocate(powerOfTwoInitialCapacity)
  }

  /** Turn hashcode `x` into a table index */
  private def index(x: Int): Int = x & (table.length - 1)

  /** Hashcode, can be overridden */
  def hash(x: T): Int = x.hashCode

  /** Find entry such that `x equals entry`. If it exists, return it.
   *  If not, enter `x` in set and return `x`.
   */
  def findEntryOrUpdate(x: T): T = {
    var h = index(hash(x))
    var entry = table(h)
    while (entry ne null) {
      if (x equals entry) return entry.asInstanceOf[T]
      h = index(h + 1)
      entry = table(h)
    }
    addEntryAt(h, x)
  }

  /** Add entry at `x` at index `idx` */
  private def addEntryAt(idx: Int, x: T) = {
    table(idx) = x
    used += 1
    if (used > limit) growTable()
    x
  }

  /** The entry in the set such that `x equals entry`, or else `null`. */
  def findEntry(x: T): T = {
    var h = index(hash(x))
    var entry = table(h)
    while ((entry ne null) && !(x equals entry)) {
      h = index(h + 1)
      entry = table(h)
    }
    entry.asInstanceOf[T]
  }

  private var rover: Int = -1

  /** Add entry `x` to set */
  def addEntry(x: T): Unit = {
    var h = index(hash(x))
    var entry = table(h)
    while (entry ne null) {
      if (x equals entry) return
      h = index(h + 1)
      entry = table(h)
    }
    table(h) = x
    used += 1
    if (used > (table.length >> 2)) growTable()
  }

  /** Add all entries in `xs` to set */
  def addEntries(xs: TraversableOnce[T]): Unit = {
    xs foreach addEntry
  }

  /** The iterator of all elements in the set */
  def iterator = new Iterator[T] {
    private var i = 0
    def hasNext: Boolean = {
      while (i < table.length && (table(i) eq null)) i += 1
      i < table.length
    }
    def next(): T =
      if (hasNext) { i += 1; table(i - 1).asInstanceOf[T] }
      else null
  }

  /** Privileged access: Find first entry with given hashcode */
  protected def findEntryByHash(hashCode: Int): T = {
    rover = index(hashCode)
    nextEntryByHash(hashCode)
  }

  /** Privileged access: Find next entry with given hashcode. Needs to immediately
   *  follow a `findEntryByhash` or `nextEntryByHash` operation.
   */
  protected def nextEntryByHash(hashCode: Int): T = {
    var entry = table(rover)
    while (entry ne null) {
      rover = index(rover + 1)
      if (hash(entry.asInstanceOf[T]) == hashCode) return entry.asInstanceOf[T]
      entry = table(rover)
    }
    null
  }

  /** Privileged access: Add entry `x` at the last position where an unsuccsessful
   *  `findEntryByHash` or `nextEntryByhash` operation returned. Needs to immediately
   *  follow a `findEntryByhash` or `nextEntryByHash` operation that was unsucessful,
   *  i.e. that returned `null`.
   */
  protected def addEntryAfterScan(x: T): T = addEntryAt(rover, x)

  private def addOldEntry(x: T): Unit = {
    var h = index(hash(x))
    var entry = table(h)
    while (entry ne null) {
      h = index(h + 1)
      entry = table(h)
    }
    table(h) = x
  }

  private def growTable(): Unit = {
    val oldtable = table
    allocate(table.length * 2)
    var i = 0
    while (i < oldtable.length) {
      val entry = oldtable(i)
      if (entry ne null) addOldEntry(entry.asInstanceOf[T])
      i += 1
    }
  }

  override def toString() = "HashSet(%d / %d)".format(used, table.length)
}
