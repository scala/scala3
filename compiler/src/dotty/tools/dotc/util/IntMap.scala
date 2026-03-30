package dotty.tools.dotc.util

import scala.compiletime.uninitialized

/** A dense map from some `Key` type to `Int`. Dense means: All keys and values
 *  are stored in arrays from 0 up to the size of the map. Keys and values
 *  can be obtained by index using `key(index)` and `value(index)`. Values
 *  can also be stored using `setValue(index, value)`.
 *
 *  ome privileged protected access to its internals
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
final class IntMap[Key](initialCapacity: Int = 8, capacityMultiple: Int = 2)
extends PerfectHashing[Key](initialCapacity, capacityMultiple):
  private var values: Array[Int] = uninitialized

  def default: Int = -1

  protected override def allocate(capacity: Int) =
    super.allocate(capacity)
    values = new Array[Int](capacity)

  /** The value associated with key `k`, or else `default`. */
  def apply(k: Key): Int =
    val idx = index(k)
    if idx < 0 then default else values(idx)

  /** Associate key `k` with value `v` */
  def update(k: Key, v: Int): Unit =
    val idx = add(k) // don't merge the two statements, `add` might change `values`.
    values(idx) = v

  protected override def growTable() =
    val oldValues = values
    super.growTable()
    Array.copy(oldValues, 0, values, 0, oldValues.length)

  def valuesIterator = values.iterator.take(size)

  def iterator: Iterator[(Key, Int)] = keysIterator.zip(valuesIterator)

  /** The value stored at index `i` */
  def value(i: Int) = values(i)

  /** Change the value stored at index `i` to `v` */
  def setValue(i: Int, v: Int) = values(i) = v

  override def toString =
    iterator.map((k, v) => s"$k -> $v").mkString("IntMap(", ", ", ")")
end IntMap
