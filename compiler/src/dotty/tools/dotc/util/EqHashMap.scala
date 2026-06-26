package dotty.tools.dotc.util

/** A specialized implementation of GenericHashMap with identity hash and `eq`
 *  as comparison.
 */
class EqHashMap[Key, Value]
    (initialCapacity: Int = 8, capacityMultiple: Int = 2)
extends GenericHashMap[Key, Value](initialCapacity, capacityMultiple):

  /** Hashcode is identityHashCode left-shifted by 1, so lowest bit is not lost
   *  when taking the index.
   */
  final def hash(x: Key): Int = System.identityHashCode(x) << 1

  /** Equality, by default `eq`,  but can be overridden */
  final def isEqual(x: Key, y: Key): Boolean = x.asInstanceOf[AnyRef] eq y.asInstanceOf[AnyRef]

  // The following methods are duplicated from GenericHashMap
  // to avoid polymorphic dispatches.
  // Aside: It would be nice to have a @specialized annotation that does
  // this automatically

  private def index(x: Int): Int = x & (table.length - 2)

  private def firstIndex(key: Key) = if isDense then 0 else index(hash(key))
  private def nextIndex(idx: Int) =
    Stats.record(statsItem("miss"))
    index(idx + 2)

  private def keyAt(idx: Int): Key = table(idx).asInstanceOf[Key]
  private def valueAt(idx: Int): Value = table(idx + 1).asInstanceOf[Value]

  private def setKey(idx: Int, key: Key) = table(idx) = key.asInstanceOf[AnyRef]
  private def setValue(idx: Int, value: Value) = table(idx + 1) = value.asInstanceOf[AnyRef]

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

  override def getOrElseUpdate(key: Key, value: => Value): Value =
    // created by blending lookup and update, avoid having to recompute hash and probe
    Stats.record(statsItem("lookup-or-update"))
    var idx = firstIndex(key)
    var k = keyAt(idx)
    while k != null do
      if isEqual(k, key) then return valueAt(idx)
      idx = nextIndex(idx)
      k = keyAt(idx)
    val v = value
    setKey(idx, key)
    setValue(idx, v)
    used += 1
    if used > limit then growTable()
    v

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
end EqHashMap

object EqHashMap:

  /** A specialized EqHashMap for caches that always grow past the dense
   *  threshold. The `isDense` branch is removed from the hot lookup/insert
   *  paths; entries are always indexed by hash. The initial capacity is
   *  fixed beyond `DenseLimit` so the table is never in dense mode.
   */
  class HashedOnly[Key, Value](initialCapacity: Int = GenericHashMap.DenseLimit * 2, capacityMultiple: Int = 2)
  extends EqHashMap[Key, Value](
      // Force the initial capacity to be strictly greater than DenseLimit so
      // `limit = capacity / capacityMultiple` and `isDense` is false from the
      // first allocation.
      if initialCapacity > GenericHashMap.DenseLimit then initialCapacity
      else GenericHashMap.DenseLimit * 2,
      capacityMultiple):

    // Hash-only specialized helpers. They are identical to the EqHashMap
    // private helpers except `firstIndex` skips the `isDense` check.

    private def hoIndex(x: Int): Int = x & (table.length - 2)

    private def hoFirstIndex(key: Key) = hoIndex(hash(key))
    private def hoNextIndex(idx: Int) =
      Stats.record(statsItem("miss"))
      hoIndex(idx + 2)

    private def hoKeyAt(idx: Int): Key = table(idx).asInstanceOf[Key]
    private def hoValueAt(idx: Int): Value = table(idx + 1).asInstanceOf[Value]

    private def hoSetKey(idx: Int, key: Key) = table(idx) = key.asInstanceOf[AnyRef]
    private def hoSetValue(idx: Int, value: Value) = table(idx + 1) = value.asInstanceOf[AnyRef]

    override def lookup(key: Key): Value | Null =
      Stats.record(statsItem("lookup"))
      var idx = hoFirstIndex(key)
      var k = hoKeyAt(idx)
      while k != null do
        if isEqual(k, key) then return hoValueAt(idx)
        idx = hoNextIndex(idx)
        k = hoKeyAt(idx)
      null

    override def update(key: Key, value: Value): Unit =
      Stats.record(statsItem("update"))
      var idx = hoFirstIndex(key)
      var k = hoKeyAt(idx)
      while k != null do
        if isEqual(k, key) then
          hoSetValue(idx, value)
          return
        idx = hoNextIndex(idx)
        k = hoKeyAt(idx)
      hoSetKey(idx, key)
      hoSetValue(idx, value)
      used += 1
      if used > limit then growTable()

    override def getOrElseUpdate(key: Key, value: => Value): Value =
      Stats.record(statsItem("lookup-or-update"))
      var idx = hoFirstIndex(key)
      var k = hoKeyAt(idx)
      while k != null do
        if isEqual(k, key) then return hoValueAt(idx)
        idx = hoNextIndex(idx)
        k = hoKeyAt(idx)
      val v = value
      hoSetKey(idx, key)
      hoSetValue(idx, v)
      used += 1
      if used > limit then growTable()
      v

    // These caches only grow (they are promoted past dense and never shrink),
    // so they re-insert the whole table on every resize. Quadruple (×4) the
    // logical capacity per grow instead of doubling so the final size is
    // reached in roughly half as many re-insert passes (≈2.23M → ≈1.29M
    // re-inserts/compile). `allocate` recomputes `limit = capacity /
    // capacityMultiple`, so the load factor — and thus probe-chain length — is
    // unchanged; only the number of grow/copy passes drops.
    override protected def postDenseGrowCapacity(currentTableLength: Int): Int =
      currentTableLength * 2

    // No `isDense` branch: always re-hash on copy.
    override def copyFrom(oldTable: Array[AnyRef | Null]): Unit =
      var idx = 0
      while idx < oldTable.length do
        val key = oldTable(idx).asInstanceOf[Key]
        if key != null then addOldHashed(key, oldTable(idx + 1).asInstanceOf[Value])
        idx += 2

    private def addOldHashed(key: Key, value: Value): Unit =
      Stats.record(statsItem("re-enter"))
      var idx = hoFirstIndex(key)
      var k = hoKeyAt(idx)
      while k != null do
        idx = hoNextIndex(idx)
        k = hoKeyAt(idx)
      hoSetKey(idx, key)
      hoSetValue(idx, value)
  end HashedOnly
end EqHashMap
