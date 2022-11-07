package dotty.tools.dotc.util

import collection.mutable.ListBuffer

/** A simple linked map with `eq` as the key comparison, optimized for small maps.
 *  It has linear complexity for `apply`, `updated`, and `remove`.
 */
abstract class SimpleIdentityMap[K <: AnyRef, +V <: AnyRef] extends (K => V | Null) {
  final def isEmpty: Boolean = this eq SimpleIdentityMap.myEmpty
  def size: Int
  def apply(k: K): V | Null
  def remove(k: K): SimpleIdentityMap[K, V]
  def updated[V1 >: V <: AnyRef](k: K, v: V1): SimpleIdentityMap[K, V1]
  def contains(k: K): Boolean = apply(k) != null
  def mapValuesNow[V1 >: V <: AnyRef](f: (K, V1) => V1): SimpleIdentityMap[K, V1]
  def foreachBinding(f: (K, V) => Unit): Unit
  def forallBinding(f: (K, V) => Boolean): Boolean
  def map2[T](f: (K, V) => T): List[T] = {
    val buf = new ListBuffer[T]
    foreachBinding((k, v) => buf += f(k, v))
    buf.toList
  }
  def keys: List[K] = map2((k, v) => k)
  def toList: List[(K, V)] = map2((k, v) => (k, v))
  override def toString: String = {
    def assocToString(key: K, value: V) = s"$key -> $value"
    map2(assocToString) mkString ("(", ", ", ")")
  }
}

object SimpleIdentityMap {

  private val CompactifyThreshold = 4

  private object myEmpty extends SimpleIdentityMap[AnyRef, Nothing] {
    def size = 0
    def apply(k: AnyRef) = null
    def remove(k: AnyRef) = this
    def updated[V1 <: AnyRef](k: AnyRef, v: V1) = new Map1(k, v)
    def mapValuesNow[V1 <: AnyRef](f: (AnyRef, V1) => V1) = this
    def foreachBinding(f: (AnyRef, Nothing) => Unit) = ()
    def forallBinding(f: (AnyRef, Nothing) => Boolean) = true
  }

  def empty[K <: AnyRef]: SimpleIdentityMap[K, Nothing] = myEmpty.asInstanceOf[SimpleIdentityMap[K, Nothing]]

  class Map1[K <: AnyRef, +V <: AnyRef] (k1: K, v1: V) extends SimpleIdentityMap[K, V] {
    def size: Int = 1
    def apply(k: K): V | Null =
      if (k eq k1) v1
      else null
    def remove(k: K): SimpleIdentityMap[K, V] =
      if (k eq k1) empty
      else this
    def updated[V1 >: V <: AnyRef](k: K, v: V1): SimpleIdentityMap[K, V1] =
      if (k eq k1) new Map1(k, v)
      else new Map2(k1, v1, k, v)
    def mapValuesNow[V1 >: V <: AnyRef](f: (K, V1) => V1): SimpleIdentityMap[K, V1] = {
      val w1 = f(k1, v1)
      if (v1 eq w1) this else new Map1(k1, w1)
    }
    def foreachBinding(f: (K, V) => Unit): Unit = f(k1, v1)
    def forallBinding(f: (K, V) => Boolean): Boolean = f(k1, v1)
  }

  class Map2[K <: AnyRef, +V <: AnyRef] (k1: K, v1: V, k2: K, v2: V) extends SimpleIdentityMap[K, V] {
    def size: Int = 2
    def apply(k: K): V | Null =
      if (k eq k1) v1
      else if (k eq k2) v2
      else null
    def remove(k: K): SimpleIdentityMap[K, V] =
      if (k eq k1) new Map1(k2, v2)
      else if (k eq k2) new Map1(k1, v1)
      else this
    def updated[V1 >: V <: AnyRef](k: K, v: V1): SimpleIdentityMap[K, V1] =
      if (k eq k1) new Map2(k, v, k2, v2)
      else if (k eq k2) new Map2(k1, v1, k, v)
      else new Map3(k1, v1, k2, v2, k, v)
    def mapValuesNow[V1 >: V <: AnyRef](f: (K, V1) => V1): SimpleIdentityMap[K, V1] = {
      val w1 = f(k1, v1); val w2 = f(k2, v2)
      if ((v1 eq w1) && (v2 eq w2)) this
      else new Map2(k1, w1, k2, w2)
    }
    def foreachBinding(f: (K, V) => Unit): Unit = { f(k1, v1); f(k2, v2) }
    def forallBinding(f: (K, V) => Boolean): Boolean = f(k1, v1) && f(k2, v2)
  }

  class Map3[K <: AnyRef, +V <: AnyRef] (k1: K, v1: V, k2: K, v2: V, k3: K, v3: V) extends SimpleIdentityMap[K, V] {
    def size: Int = 3
    def apply(k: K): V | Null =
      if (k eq k1) v1
      else if (k eq k2) v2
      else if (k eq k3) v3
      else null
    def remove(k: K): SimpleIdentityMap[K, V] =
      if (k eq k1) new Map2(k2, v2, k3, v3)
      else if (k eq k2) new Map2(k1, v1, k3, v3)
      else if (k eq k3) new Map2(k1, v1, k2, v2)
      else this
    def updated[V1 >: V <: AnyRef](k: K, v: V1): SimpleIdentityMap[K, V1] =
      if (k eq k1) new Map3(k, v, k2, v2, k3, v3)
      else if (k eq k2) new Map3(k1, v1, k, v, k3, v3)
      else if (k eq k3) new Map3(k1, v1, k2, v2, k, v)
      else new Map4(k1, v1, k2, v2, k3, v3, k, v)
    def mapValuesNow[V1 >: V <: AnyRef](f: (K, V1) => V1): SimpleIdentityMap[K, V1] = {
      val w1 = f(k1, v1); val w2 = f(k2, v2); val w3 = f(k3, v3)
      if ((v1 eq w1) && (v2 eq w2) && (v3 eq w3)) this
      else new Map3(k1, w1, k2, w2, k3, w3)
    }
    def foreachBinding(f: (K, V) => Unit): Unit = { f(k1, v1); f(k2, v2); f(k3, v3) }
    def forallBinding(f: (K, V) => Boolean): Boolean = f(k1, v1) && f(k2, v2) && f(k3, v3)
  }

  class Map4[K <: AnyRef, +V <: AnyRef] (k1: K, v1: V, k2: K, v2: V, k3: K, v3: V, k4: K, v4: V) extends SimpleIdentityMap[K, V] {
    def size: Int = 4
    def apply(k: K): V | Null =
      if (k eq k1) v1
      else if (k eq k2) v2
      else if (k eq k3) v3
      else if (k eq k4) v4
      else null
    def remove(k: K): SimpleIdentityMap[K, V] =
      if (k eq k1) new Map3(k2, v2, k3, v3, k4, v4)
      else if (k eq k2) new Map3(k1, v1, k3, v3, k4, v4)
      else if (k eq k3) new Map3(k1, v1, k2, v2, k4, v4)
      else if (k eq k4) new Map3(k1, v1, k2, v2, k3, v3)
      else this
    def updated[V1 >: V <: AnyRef](k: K, v: V1): SimpleIdentityMap[K, V1] =
      if (k eq k1) new Map4(k, v, k2, v2, k3, v3, k4, v4)
      else if (k eq k2) new Map4(k1, v1, k, v, k3, v3, k4, v4)
      else if (k eq k3) new Map4(k1, v1, k2, v2, k, v, k4, v4)
      else if (k eq k4) new Map4(k1, v1, k2, v2, k3, v3, k, v)
      else new MapMore(Array[AnyRef](k1, v1, k2, v2, k3, v3, k4, v4, k, v))
    def mapValuesNow[V1 >: V <: AnyRef](f: (K, V1) => V1): SimpleIdentityMap[K, V1] = {
      val w1 = f(k1, v1); val w2 = f(k2, v2); val w3 = f(k3, v3); val w4 = f(k4, v4)
      if ((v1 eq w1) && (v2 eq w2) && (v3 eq w3) && (v4 eq w4)) this
      else new Map4(k1, w1, k2, w2, k3, w3, k4, w4)
    }
    def foreachBinding(f: (K, V) => Unit): Unit = { f(k1, v1); f(k2, v2); f(k3, v3); f(k4, v4) }
    def forallBinding(f: (K, V) => Boolean): Boolean = f(k1, v1) && f(k2, v2) && f(k3, v3) && f(k4, v4)
  }

  class MapMore[K <: AnyRef, +V <: AnyRef](bindings: Array[AnyRef]) extends SimpleIdentityMap[K, V] {
    private def key(i: Int): K = bindings(i).asInstanceOf[K]
    private def value(i: Int): V = bindings(i + 1).asInstanceOf[V]

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
            if (size == CompactifyThreshold) {
              var m: SimpleIdentityMap[K, V] = empty[K]
              for (j <- 0 until bindings.length by 2)
                if (j != i) m = m.updated(key(j), value(j))
              m
            }
            else {
              val bindings1 = new Array[AnyRef](bindings.length - 2)
              System.arraycopy(bindings, 0, bindings1, 0, i)
              System.arraycopy(bindings, i + 2, bindings1, i, bindings1.length - i)
              new MapMore(bindings1)
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
              new MapMore(bindings1)
            }
          }
        i += 2
      }
      val bindings2 = new Array[AnyRef](bindings.length + 2)
      System.arraycopy(bindings, 0, bindings2, 0, bindings.length)
      bindings2(bindings.length) = k
      bindings2(bindings.length + 1) = v
      new MapMore(bindings2)
    }

    override def contains(k: K): Boolean = {
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
      if (bindings1 eq bindings) this else new MapMore(bindings1)
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
      return true
    }
  }
}
