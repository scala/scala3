package dotty.tools.dotc.util

/** A mutable identity set that preserves insertion order.
 *
 *  Adds and membership tests are amortized O(1) (identity hash table with doubling),
 *  iteration preserves insertion order, and removal is O(1) with occasional
 *  compaction. Elements are compared by reference, so the set is safe for elements
 *  whose `equals`/`hashCode` are structural (e.g. case classes).
 *
 *  Unlike `EqHashSet`, iteration order is deterministic (insertion order rather than
 *  hash-table order), which matters when the set is part of compiler state whose
 *  traversal order must be reproducible.
 */
final class MutableIdentitySet[E <: AnyRef] extends MutableSet[E]:
  private var elems: Array[AnyRef | Null] = new Array[AnyRef | Null](4)

  // Open addressing table: 0 = empty, -1 = deleted, otherwise position in `elems` + 1.
  private var table: Array[Int] = new Array[Int](8)

  /** Number of slots in `elems` that have been used (including holes left by `-=`). */
  private var end = 0

  /** Number of live elements. */
  private var mySize = 0

  /** Number of nonzero slots in `table` (live entries + tombstones). */
  private var used = 0

  def size: Int = mySize
  override def isEmpty: Boolean = mySize == 0

  /** The smallest power of two >= `n` (the table size must be a power of two
   *  for the mask-based probing to work). */
  private def nextPowerOfTwo(n: Int): Int =
    if n >= (1 << 30) then 1 << 30
    else
      var p = 1
      while p < n do p <<= 1
      p

  /** Position of `x` in `elems` + 1, or 0 if `x` is not an element. */
  private def positionPlus1(x: AnyRef): Int =
    var i = System.identityHashCode(x) & (table.length - 1)
    var t = table(i)
    while t != 0 do
      if t > 0 && (elems(t - 1) eq x) then return t
      i = (i + 1) & (table.length - 1)
      t = table(i)
    0

  override def contains(x: E): Boolean = positionPlus1(x) != 0

  override def lookup(x: E): E | Null =
    val t = positionPlus1(x)
    if t == 0 then null else elems(t - 1).asInstanceOf[E]

  /** Double the `elems` array, keeping holes. */
  private def growElems(): Unit =
    val elems1 = new Array[AnyRef | Null](elems.length * 2)
    System.arraycopy(elems, 0, elems1, 0, end)
    elems = elems1

  /** Rebuild the hash table from `elems[0..end)` at double the size, dropping
   *  holes and tombstones. */
  private def growTable(): Unit =
    val table1 = new Array[Int](table.length * 2)
    val oldTable = table
    table = table1
    used = 0
    var i = 0
    while i < end do
      val e = elems(i)
      if e != null then
        used += 1
        var j = System.identityHashCode(e) & (table1.length - 1)
        while table1(j) != 0 do j = (j + 1) & (table1.length - 1)
        table1(j) = i + 1
      i += 1

  /** Register `elems(posPlus1 - 1)` in the hash table, reusing the first
   *  tombstone on the probe path if there is one. Reusing a tombstone is safe
   *  because lookups only stop at empty (`0`) slots, never at tombstones. */
  private def insert(posPlus1: Int): Unit =
    var i = System.identityHashCode(elems(posPlus1 - 1)) & (table.length - 1)
    while table(i) != 0 && table(i) != -1 do
      i = (i + 1) & (table.length - 1)
    if table(i) == 0 then used += 1
    table(i) = posPlus1

  private def insertNew(x: E): Unit =
    if end == elems.length then growElems()
    if used + 1 > table.length / 2 then growTable()
    elems(end) = x
    insert(end + 1)
    end += 1
    mySize += 1

  override def += (x: E): Unit =
    if positionPlus1(x) == 0 then insertNew(x)

  override def add(x: E): Boolean =
    if positionPlus1(x) != 0 then false
    else
      insertNew(x)
      true

  override def put(x: E): E =
    // Elements are compared by identity, so an existing entry is always `x` itself.
    if positionPlus1(x) == 0 then insertNew(x)
    x

  /** Remove all elements. If `resetToInitial` is false, keep the backing arrays. */
  override def clear(resetToInitial: Boolean = true): Unit =
    if resetToInitial then
      elems = new Array[AnyRef | Null](4)
      table = new Array[Int](8)
    else
      java.util.Arrays.fill(elems, null)
      java.util.Arrays.fill(table, 0)
    end = 0
    mySize = 0
    used = 0

  /** Rebuild both arrays without holes and tombstones, keeping the relative
   *  order of live elements (so iteration still follows insertion order). */
  private def compact(): Unit =
    val elems1 = new Array[AnyRef | Null](math.max(4, mySize * 2))
    val table1 = new Array[Int](nextPowerOfTwo(math.max(8, mySize * 4)))
    var from = 0
    var to = 0
    while from < end do
      val e = elems(from)
      if e != null then
        elems1(to) = e
        var j = System.identityHashCode(e) & (table1.length - 1)
        while table1(j) != 0 do j = (j + 1) & (table1.length - 1)
        table1(j) = to + 1
        to += 1
      from += 1
    elems = elems1
    table = table1
    end = to
    used = to

  override def -= (x: E): Unit =
    val t = positionPlus1(x)
    if t != 0 then
      elems(t - 1) = null
      var i = System.identityHashCode(x) & (table.length - 1)
      while table(i) != t do i = (i + 1) & (table.length - 1)
      table(i) = -1
      mySize -= 1
      // `used` stays the same: the slot is now a tombstone, not empty
      if mySize * 4 < end && end > 8 then compact()

  override def foreach[U](f: E => U): Unit =
    var i = 0
    while i < end do
      val e = elems(i)
      if e != null then f(e.asInstanceOf[E])
      i += 1

  override def iterator: Iterator[E] = new Iterator[E]:
    private var idx = 0
    private def skipHoles(): Unit =
      while idx < end && elems(idx) == null do idx += 1
    def hasNext: Boolean =
      skipHoles()
      idx < end
    def next(): E =
      if hasNext then
        val e = elems(idx)
        idx += 1
        e.asInstanceOf[E]
      else Iterator.empty.next()

  def forall(p: E => Boolean): Boolean =
    var i = 0
    while i < end do
      val e = elems(i)
      if e != null && !p(e.asInstanceOf[E]) then return false
      i += 1
    true

  def foldLeft[A](z: A)(f: (A, E) => A): A =
    var acc = z
    foreach(e => acc = f(acc, e))
    acc

  override def toString: String = iterator.mkString("{", ", ", "}")
end MutableIdentitySet
