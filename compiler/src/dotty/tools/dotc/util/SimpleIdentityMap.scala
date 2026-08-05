package dotty.tools.dotc.util

import scala.collection.mutable.ListBuffer

/** A simple linked map with `eq` as the key comparison, optimized for small maps.
 * It has linear complexity for `apply`, `updated`, and `remove`.
 */
final class SimpleIdentityMap[K <: AnyRef, +V <: AnyRef](bindings: Array[AnyRef]) extends (K => V | Null) {
  private def key(i: Int): K = bindings(i).asInstanceOf[K]

  private def value(i: Int): V = bindings(i + 1).asInstanceOf[V]

  def isEmpty: Boolean =
    bindings.length == 0

  def size: Int = bindings.length / 2

  Stats.record(s"SimpleIdentityMap/$size")

  def apply(k: K): V | Null = {
    var i = 0
    while (i < bindings.length) {
      if (bindings(i) eq k) return value(i)
      i += 2
    }
    null
  }

  def remove(k: K): SimpleIdentityMap[K, V] = {
    var i = 0
    while (i < bindings.length) {
      if (bindings(i) eq k)
        return {
          if (size == SimpleIdentityMap.CompactifyThreshold) {
            var m: SimpleIdentityMap[K, V] = SimpleIdentityMap.empty[K]
            for (j <- 0 until bindings.length by 2)
              if (j != i) m = m.updated(key(j), value(j))
            m
          }
          else {
            val bindings1 = new Array[AnyRef](bindings.length - 2)
            System.arraycopy(bindings, 0, bindings1, 0, i)
            System.arraycopy(bindings, i + 2, bindings1, i, bindings1.length - i)
            new SimpleIdentityMap(bindings1)
          }
        }
      i += 2
    }
    this
  }

  def updated[V1 >: V <: AnyRef](k: K, v: V1): SimpleIdentityMap[K, V] = {
    var i = 0
    while (i < bindings.length) {
      if (bindings(i) eq k)
        return {
          if (v eq bindings(i + 1)) this
          else {
            val bindings1 = bindings.clone
            bindings1(i + 1) = v
            new SimpleIdentityMap(bindings1)
          }
        }
      i += 2
    }
    val bindings2 = new Array[AnyRef](bindings.length + 2)
    System.arraycopy(bindings, 0, bindings2, 0, bindings.length)
    bindings2(bindings.length) = k
    bindings2(bindings.length + 1) = v
    new SimpleIdentityMap(bindings2)
  }

  def contains(k: K): Boolean = {
    var i = 0
    while (i < bindings.length) {
      if (bindings(i) eq k) return true
      i += 2
    }
    false
  }

  def mapValuesNow[V1 >: V <: AnyRef](f: (K, V1) => V1): SimpleIdentityMap[K, V1] = {
    var bindings1: Array[AnyRef] = bindings
    var i = 0
    while (i < bindings.length) {
      val v = value(i)
      val v1 = f(key(i), v)
      if ((v1 ne v) && (bindings1 eq bindings))
        bindings1 = bindings.clone
      bindings1(i) = bindings(i)
      bindings1(i + 1) = v1
      i += 2
    }
    if (bindings1 eq bindings) this else new SimpleIdentityMap(bindings1)
  }

  def foreachBinding(f: (K, V) => Unit): Unit = {
    var i = 0
    while (i < bindings.length) {
      f(key(i), value(i))
      i += 2
    }
  }

  def forallBinding(f: (K, V) => Boolean): Boolean = {
    var i = 0
    while (i < bindings.length) {
      if (!f(key(i), value(i)))
        return false
      i += 2
    }
    true
  }

  private def map2[T](f: (K, V) => T): List[T] = {
    val buf = new ListBuffer[T]
    foreachBinding((k, v) => buf += f(k, v))
    buf.toList
  }

  def keys: List[K] = map2((k, v) => k)

  def toList: List[(K, V)] = map2((k, v) => (k, v))

  override def toString(): String = {
    def assocToString(key: K, value: V) = s"$key -> $value"

    map2(assocToString) mkString("(", ", ", ")")
  }

}

object SimpleIdentityMap {
  private val emptyMap = new SimpleIdentityMap(Array.empty[AnyRef])

  private val CompactifyThreshold = 4

  def empty[K <: AnyRef]: SimpleIdentityMap[K, Nothing] = emptyMap.asInstanceOf[SimpleIdentityMap[K, Nothing]]
}
