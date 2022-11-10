package dotty.tools.dotc.util

import scala.language.unsafeNulls

import reflect.ClassTag
import annotation.tailrec

/** A least-recently-used cache for Key -> Value computations
 *  It currently keeps the last 8 associations, but this can be
 *  changed to anywhere between 2 and 16 by changing `LRUCache.Retained`.
 *
 *  Implementation: We keep a ring of eight places, linked
 *  with the `next` data structure. The ring models a priority queue.
 *  `last` points to the last element of the queue, and
 *  `next(last)` to the first one. Lookups compare keys
 *  sequentially from first to last. Elements with successful lookup
 *  get promoted to be first in the queue. Elements are evicted
 *  at the `last` position.
 */
class LRUCache[Key >: Null <: AnyRef : ClassTag, Value >: Null: ClassTag] {
  import LRUCache._
  val keys: Array[Key] = new Array[Key](Retained)
  val values: Array[Value] = new Array(Retained)
  var next: SixteenNibbles = new SixteenNibbles(initialRing.bits)
  var last: Int = Retained - 1 // value is arbitrary
  var lastButOne: Int = last - 1

  def first: Int = next(last)

  /** Lookup key, returning value or `null` for not found.
   *  As a side effect, sets `lastButOne` to the element before `last`
   *  if key was not found.
   */
  def lookup(key: Key): Value = {
    @tailrec
    def lookupNext(prev: Int, current: Int, nx: SixteenNibbles): Value = {
      val follow = nx(current)
      if (keys(current) eq key) {
        // arrange so that found element is at position `first`.
        if (current == last) last = prev
        else if (prev != last) {
          next = next.updated(prev, follow)
          next = next.updated(current, first)
          next = next.updated(last, current)
        }
        values(current)
      }
      else if (current == last) {
        lastButOne = prev
        null
      }
      else
        lookupNext(current, follow, nx)
    }
    lookupNext(last, first, next)
  }

  /** Enter key/value in cache at position `last`.
   *  As a side effect, sets `last` to `lastButOne`.
   *  If `lastButOne` was set by a preceding unsuccessful `lookup`
   *  for the same key, this means that the new element is now the
   *  first in the queue. If there was no preceding lookup, the element
   *  is inserted at a random position in the queue.
   */
  def enter(key: Key, value: Value): Unit = {
    keys(last) = key
    values(last) = value
    last = lastButOne
  }

  /** Invalidate key. The invalidated element becomes
   *  the last in the queue.
   */
  def invalidate(key: Key): Unit =
    if (lookup(key) != null) {
      keys(first) = null
      last = first
    }

  def indices: Iterator[Int] = Iterator.iterate(first)(next.apply)

  def keysIterator: Iterator[Key] =
    indices take Retained map keys filter (_ != null)

  override def toString: String = {
    val assocs = keysIterator
      .toList  // double reverse so that lookups do not perturb order
      .reverse
      .map(key => s"$key -> ${lookup(key)}")
      .reverse
    s"LRUCache(${assocs.mkString(", ")})"
  }
}

object LRUCache {

  /** The number of retained elements in the cache; must be at most 16. */
  val Retained: Int = 16

  /** The initial ring: 0 -> 1 -> ... -> 7 -> 0 */
  val initialRing: SixteenNibbles =
    (0 until Retained).foldLeft(new SixteenNibbles(0L))((nibbles, idx) =>
      nibbles.updated(idx, (idx + 1) % Retained))
}
