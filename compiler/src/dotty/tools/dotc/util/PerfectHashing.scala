package dotty.tools.dotc.util

import scala.compiletime.uninitialized

object PerfectHashing:

  /** The number of elements up to which dense packing is used.
   *  If the number of elements reaches `DenseLimit` a hash table is used instead
   */
  inline val DenseLimit = 16

/** A map that maps keys to unique integers in a dense interval starting at 0.
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
class PerfectHashing[Key](initialCapacity: Int = 8, capacityMultiple: Int = 2):
  import PerfectHashing.DenseLimit

  private var used: Int = uninitialized
  private var table: Array[Int] = uninitialized
  private var keys: Array[AnyRef] = uninitialized

  clear()

  protected def allocate(capacity: Int) =
    keys = new Array[AnyRef](capacity)
    if !isDense then
      table = new Array[Int](capacity * roundToPower(capacityMultiple))

  private def roundToPower(n: Int) =
    if Integer.bitCount(n) == 1 then n
    else 1 << (32 - Integer.numberOfLeadingZeros(n))

  /** Remove keys from this map and set back to initial configuration */
  def clear(): Unit =
    used = 0
    allocate(roundToPower(initialCapacity max 4))

  /** The number of keys */
  final def size: Int = used

  /** The number of keys that can be stored without growing the tables */
  final def capacity: Int = keys.length

  private final def isDense = capacity <= DenseLimit

  /** Hashcode, by default a post-processed versoon of `k.hashCode`,
   *  can be overridden
   */
  protected def hash(k: Key): Int =
    val h = k.hashCode
    // Part of the MurmurHash3 32 bit finalizer
    val i = (h ^ (h >>> 16)) * 0x85EBCA6B
    val j = (i ^ (i >>> 13)) & 0x7FFFFFFF
    if (j==0) 0x41081989 else j

  /** Equality test, by default `equals`, can be overridden */
  protected def isEqual(x: Key, y: Key): Boolean = x.equals(y)

  private def matches(entry: Int, k: Key) = isEqual(key(entry), k)

  private def tableIndex(x: Int): Int = x & (table.length - 1)
  private def firstIndex(k: Key) = tableIndex(hash(k))
  private def nextIndex(idx: Int) = tableIndex(idx + 1)

  /** The key at index `idx` */
  def key(idx: Int) = keys(idx).asInstanceOf[Key]
  private def setKey(e: Int, k: Key) = keys(e) = k.asInstanceOf[AnyRef]

  private def entry(idx: Int): Int = table(idx) - 1
  private def setEntry(idx: Int, entry: Int) = table(idx) = entry + 1

  /** An index `idx` such that `key(idx) == k`, or -1 if no such index exists */
  def index(k: Key): Int =
    if isDense then
      var e = 0
      while e < used do
        if matches(e, k) then return e
        e += 1
      -1
    else
      var idx = firstIndex(k)
      var e = entry(idx)
      while e >= 0 && !matches(e, k) do
        idx = nextIndex(idx)
        e = entry(idx)
      e

  /** An index `idx` such that key(idx) == k.
   *  If no such index exists, create an entry with an index one
   *  larger than the previous one.
   */
  def add(k: Key): Int =
    if isDense then
      var e = 0
      while e < used do
        if matches(e, k) then return e
        e += 1
    else
      var idx = firstIndex(k)
      var e = entry(idx)
      while e >= 0 do
        if matches(e, k) then return e
        idx = nextIndex(idx)
        e = entry(idx)
      setEntry(idx, used)
    end if
    setKey(used, k)
    used = used + 1
    if used == capacity then growTable()
    used - 1

  private def rehash(): Unit =
    var e = 0
    while e < used do
      var idx = firstIndex(key(e))
      while entry(idx) >= 0 do idx = nextIndex(idx)
      setEntry(idx, e)
      e += 1

  /** Grow backing arrays */
  protected def growTable(): Unit =
    val oldKeys = keys
    allocate(capacity * 2)
    Array.copy(oldKeys, 0, keys, 0, oldKeys.length)
    if !isDense then rehash()

  def keysIterator: Iterator[Key] =
    keys.iterator.take(used).asInstanceOf[Iterator[Key]]
end PerfectHashing
