package dotty.tools.dotc.util

import collection.mutable.ListBuffer

abstract class SimpleMap[K, +V >: Null] {
  def apply(k: K): V
  def remove(k: K): SimpleMap[K, V]
  def updated[V1 >: V](k: K, v: V1): SimpleMap[K, V1]
  def contains(k: K): Boolean = apply(k) != null
  def mapValues[V1 >: V](f: V1 => V1): SimpleMap[K, V1]
  def foreachKey(f: K => Unit): Unit
  def map2[T](f: (K, V) => T): List[T] = {
    val buf = new ListBuffer[T]
    foreachKey { key =>
      val k = key.asInstanceOf[K]
      buf += f(k, this(k))
    }
    buf.toList
  }
  override def toString = {
    def assocToString(key: K, value: V) = s"$key -> $value"
    map2(assocToString) mkString ("(", ", ", ")")
  }
}

object SimpleMap {

  private object myEmpty extends SimpleMap[Any, Null] {
    def apply(k: Any) = null
    def remove(k: Any) = this
    def updated[V1 >: Null](k: Any, v: V1) = new Map1(k, v)
    def mapValues[V1 >: Null](f: V1 => V1) = this
    def foreachKey(f: Any => Unit) = ()
  }

  def Empty[K] = myEmpty.asInstanceOf[SimpleMap[K, Null]]

  class Map1[K, +V >: Null] (k1: K, v1: V) extends SimpleMap[K, V] {
    def apply(k: K) =
      if (k == k1) v1
      else null
    def remove(k: K) =
      if (k == k1) Empty.asInstanceOf[SimpleMap[K, V]]
      else this
    def updated[V1 >: V](k: K, v: V1) =
      if (k == k1) new Map1(k, v)
      else new Map2(k1, v1, k, v)
    def mapValues[V1 >: V](f: V1 => V1) =
      new Map1(k1, f(v1))
    def foreachKey(f: K => Unit) = f(k1)
  }

  class Map2[K, +V >: Null] (k1: K, v1: V, k2: K, v2: V) extends SimpleMap[K, V] {
    def apply(k: K) =
      if (k == k1) v1
      else if (k == k2) v2
      else null
    def remove(k: K) =
      if (k == k1) new Map1(k2, v2)
      else if (k == k2) new Map1(k1, v1)
      else this
    def updated[V1 >: V](k: K, v: V1) =
      if (k == k1) new Map2(k, v, k2, v2)
      else if (k == k2) new Map2(k1, v1, k, v)
      else new Map3(k1, v1, k2, v2, k, v)
    def mapValues[V1 >: V](f: V1 => V1) =
      new Map2(k1, f(v1), k2, f(v2))
    def foreachKey(f: K => Unit) = { f(k1); f(k2) }
  }

  class Map3[K, +V >: Null] (k1: K, v1: V, k2: K, v2: V, k3: K, v3: V) extends SimpleMap[K, V] {
    def apply(k: K) =
      if (k == k1) v1
      else if (k == k2) v2
      else if (k == k3) v3
      else null
    def remove(k: K) =
      if (k == k1) new Map2(k2, v2, k3, v3)
      else if (k == k2) new Map2(k1, v1, k3, v3)
      else if (k == k3) new Map2(k1, v1, k2, v2)
      else this
    def updated[V1 >: V](k: K, v: V1) =
      if (k == k1) new Map3(k, v, k2, v2, k3, v3)
      else if (k == k2) new Map3(k1, v1, k, v, k3, v3)
      else if (k == k3) new Map3(k1, v1, k2, v2, k, v)
      else new Map4(k1, v1, k2, v2, k3, v3, k, v)
    def mapValues[V1 >: V](f: V1 => V1) =
      new Map3(k1, f(v1), k2, f(v2), k3, f(v3))
    def foreachKey(f: K => Unit) = { f(k1); f(k2); f(k3) }
  }

  class Map4[K, +V >: Null] (k1: K, v1: V, k2: K, v2: V, k3: K, v3: V, k4: K, v4: V) extends SimpleMap[K, V] {
    def apply(k: K) =
      if (k == k1) v1
      else if (k == k2) v2
      else if (k == k3) v3
      else if (k == k4) v4
      else null
    def remove(k: K) =
      if (k == k1) new Map3(k2, v2, k3, v3, k4, v4)
      else if (k == k2) new Map3(k1, v1, k3, v3, k4, v4)
      else if (k == k3) new Map3(k1, v1, k2, v2, k4, v4)
      else if (k == k4) new Map3(k1, v1, k2, v2, k3, v3)
      else this
    def updated[V1 >: V](k: K, v: V1) =
      if (k == k1) new Map4(k, v, k2, v2, k3, v3, k4, v4)
      else if (k == k2) new Map4(k1, v1, k, v, k3, v3, k4, v4)
      else if (k == k3) new Map4(k1, v1, k2, v2, k, v, k4, v4)
      else if (k == k4) new Map4(k1, v1, k2, v2, k3, v3, k, v)
      else new Map5(k1, v1, k2, v2, k3, v3, k4, v4, k, v)
    def mapValues[V1 >: V](f: V1 => V1) =
      new Map4(k1, f(v1), k2, f(v2), k3, f(v3), k4, f(v4))
    def foreachKey(f: K => Unit) = { f(k1); f(k2); f(k3); f(k4) }
  }

  class Map5[K, +V >: Null] (k1: K, v1: V, k2: K, v2: V, k3: K, v3: V, k4: K, v4: V, k5: K, v5: V) extends SimpleMap[K, V] {
    def apply(k: K) =
      if (k == k1) v1
      else if (k == k2) v2
      else if (k == k3) v3
      else if (k == k4) v4
      else if (k == k5) v5
      else null
    def remove(k: K) =
      if (k == k1) new Map4(k2, v2, k3, v3, k4, v4, k5, v5)
      else if (k == k2) new Map4(k1, v1, k3, v3, k4, v4, k5, v5)
      else if (k == k3) new Map4(k1, v1, k2, v2, k4, v4, k5, v5)
      else if (k == k4) new Map4(k1, v1, k2, v2, k3, v3, k5, v5)
      else if (k == k5) new Map4(k1, v1, k2, v2, k3, v3, k4, v4)
      else this
    def updated[V1 >: V](k: K, v: V1) =
      if (k == k1) new Map5(k, v, k2, v2, k3, v3, k4, v4, k5, v5)
      else if (k == k2) new Map5(k1, v1, k, v, k3, v3, k4, v4, k5, v5)
      else if (k == k3) new Map5(k1, v1, k2, v2, k, v, k4, v4, k5, v5)
      else if (k == k4) new Map5(k1, v1, k2, v2, k3, v3, k, v, k5, v5)
      else if (k == k5) new Map5(k1, v1, k2, v2, k3, v3, k4, v4, k, v)
      else new MapMore(Map(k1 -> v1, k2 -> v2, k3 -> v3, k4 -> v4, k5 -> v5, k -> v))
    def mapValues[V1 >: V](f: V1 => V1) =
      new Map5(k1, f(v1), k2, f(v2), k3, f(v3), k4, f(v4), k5, f(v5))
    def foreachKey(f: K => Unit) = { f(k1); f(k2); f(k3); f(k4); f(k5) }
  }

  class MapMore[K, +V >: Null] (m: Map[K, V]) extends SimpleMap[K, V] {
    def apply(k: K) = m get k match {
      case Some(v) => v
      case None => null
    }
    def remove(k: K) = {
      val m1 = m - k
      if (m1.size > 5) new MapMore(m1)
      else {
        val List((k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5)) = m1.toList
        new Map5(k1, v1, k2, v2, k3, v3, k4, v4, k5, v5)
      }
    }
    def updated[V1 >: V](k: K, v: V1) =
      new MapMore(m.updated(k, v))
    override def contains(k: K) = m contains k
    def mapValues[V1 >: V](f: V1 => V1) =
      new MapMore(m mapValues f)
    def foreachKey(f: K => Unit) = { m.keysIterator foreach f }
  }
}
