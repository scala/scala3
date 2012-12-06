package dotty.tools.dotc.util

import reflect.ClassTag

class LRU8Cache[Key >: Null, Value >: Null] {

  import LRU8Cache._

  private var key0, key1, key2, key3, key4, key5, key6, key7: Key = null
  private var val0, val1, val2, val3, val4, val5, val6, val7: Value = null
  private var hits = 0
  private var entered = 0

  private def incr(n: Int): Unit = {
    val shift = n * width
    hits = (hits ^ (mask << shift)) | (next((hits >>> shift) & mask) << shift)
  }

  private def decr(n: Int): Unit = {
    val shift = n * width
    hits = (hits ^ (mask << shift)) | (prev((hits >>> shift) & mask) << shift)
  }

  private def init(n: Int): Unit = {
    val shift = n * width
    hits = (hits ^ (mask << shift)) | (1 << shift)
  }

  private def clear(n: Int): Unit = {
    val shift = n * width
    hits = (hits ^ (mask << shift))
  }

  private def evict(): Int = {
    var min = hits & mask
    var minIdx = 0
    hits = hits >>> width
    if ((hits & mask) < min) { min = hits & mask; minIdx = 1 }
    hits = hits >>> width
    if ((hits & mask) < min) { min = hits & mask; minIdx = 2 }
    hits = hits >>> width
    if ((hits & mask) < min) { min = hits & mask; minIdx = 3 }
    hits = hits >>> width
    if ((hits & mask) < min) { min = hits & mask; minIdx = 4 }
    hits = hits >>> width
    if ((hits & mask) < min) { min = hits & mask; minIdx = 5 }
    hits = hits >>> width
    if ((hits & mask) < min) { min = hits & mask; minIdx = 6 }
    hits = hits >>> width
    if ((hits & mask) < min) { min = hits & mask; minIdx = 7 }
    minIdx
  }

  /** Return value associated with key, or `null` if key not present.
   *  Key must be different from `null`.
   */
  def lookup(key: Key): Value = {
    if (key == key0) { incr(0); return val0 }
    if (key == key1) { incr(1); return val1 }
    if (key == key2) { incr(2); return val2 }
    if (key == key3) { incr(3); return val3 }
    if (key == key4) { incr(4); return val4 }
    if (key == key5) { incr(5); return val5 }
    if (key == key6) { incr(6); return val6 }
    if (key == key7) { incr(7); return val7 }
    null
  }

  /** Enter key/value association in cache */
  def enter(key: Key, value: Value): Unit = {
    val idx = if ((entered & 7) == entered) entered else evict()
    idx match {
      case 0 => key0 = key; val0 = value
      case 1 => key1 = key; val1 = value
      case 2 => key2 = key; val2 = value
      case 3 => key3 = key; val3 = value
      case 4 => key4 = key; val4 = value
      case 5 => key5 = key; val5 = value
      case 6 => key6 = key; val6 = value
      case 7 => key7 = key; val7 = value
    }
    init(idx)
    entered += 1
    if (entered % 8 == 0) {
      var i = 0
      while (i < 8) { decr(i); i += 1 }
    }
  }

  /** Remove entry for `key` from cache if it was present */
  def invalidate(key: Key): Unit = {
    if (key == key0) { clear(0); key0 = null }
    if (key == key1) { clear(1); key1 = null }
    if (key == key2) { clear(2); key2 = null }
    if (key == key3) { clear(3); key3 = null }
    if (key == key4) { clear(4); key4 = null }
    if (key == key5) { clear(5); key5 = null }
    if (key == key6) { clear(6); key6 = null }
    if (key == key7) { clear(7); key7 = null }
  }


}

object LRU8Cache {
  private final val width = 32 / 8        // width of a counter in bits
  private final val mask = (1 << width) - 1

  private val next: Array[Int] = (1 to mask).toArray :+ mask
  private val prev: Array[Int] = 0 +: (0 until mask).toArray
}