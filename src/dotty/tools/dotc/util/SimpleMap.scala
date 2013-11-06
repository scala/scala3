package dotty.tools.dotc.util

import collection.mutable.ListBuffer

abstract class SimpleMap[K, +V >: Null <: AnyRef] extends (K => V) {
  def size: Int
  def apply(k: K): V
  def remove(k: K): SimpleMap[K, V]
  def updated[V1 >: V <: AnyRef](k: K, v: V1): SimpleMap[K, V1]
  def contains(k: K): Boolean = apply(k) != null
  def mapValues[V1 >: V <: AnyRef](f: V1 => V1): SimpleMap[K, V1]
  def foreachKey(f: K => Unit): Unit
  def map2[T](f: (K, V) => T): List[T] = {
    val buf = new ListBuffer[T]
    foreachKey { key =>
      val k = key.asInstanceOf[K]
      buf += f(k, this(k))
    }
    buf.toList
  }
  def keys: List[K] = map2((k, v) => k)
  def toList: List[(K, V)] = map2((k, v) => (k, v))
  override def toString = {
    def assocToString(key: K, value: V) = s"$key -> $value"
    map2(assocToString) mkString ("(", ", ", ")")
  }
}

object SimpleMap {

  private object myEmpty extends SimpleMap[Any, Null] {
    def size = 0
    def apply(k: Any) = null
    def remove(k: Any) = this
    def updated[V1 >: Null <: AnyRef](k: Any, v: V1) = new Map1(k, v)
    def mapValues[V1 >: Null <: AnyRef](f: V1 => V1) = this
    def foreachKey(f: Any => Unit) = ()
  }

  def Empty[K] = myEmpty.asInstanceOf[SimpleMap[K, Null]]

  class Map1[K, +V >: Null <: AnyRef] (k1: K, v1: V) extends SimpleMap[K, V] {
    def size = 1
    def apply(k: K) =
      if (k == k1) v1
      else null
    def remove(k: K) =
      if (k == k1) Empty.asInstanceOf[SimpleMap[K, V]]
      else this
    def updated[V1 >: V <: AnyRef](k: K, v: V1) =
      if (k == k1) new Map1(k, v)
      else new Map2(k1, v1, k, v)
    def mapValues[V1 >: V <: AnyRef](f: V1 => V1) = {
      val w1 = f(v1)
      if (v1 eq w1) this else new Map1(k1, w1)
    }
    def foreachKey(f: K => Unit) = f(k1)
  }

  class Map2[K, +V >: Null <: AnyRef] (k1: K, v1: V, k2: K, v2: V) extends SimpleMap[K, V] {
    def size = 2
    def apply(k: K) =
      if (k == k1) v1
      else if (k == k2) v2
      else null
    def remove(k: K) =
      if (k == k1) new Map1(k2, v2)
      else if (k == k2) new Map1(k1, v1)
      else this
    def updated[V1 >: V <: AnyRef](k: K, v: V1) =
      if (k == k1) new Map2(k, v, k2, v2)
      else if (k == k2) new Map2(k1, v1, k, v)
      else new Map3(k1, v1, k2, v2, k, v)
    def mapValues[V1 >: V <: AnyRef](f: V1 => V1) = {
      val w1 = f(v1); val w2 = f(v2)
      if ((v1 eq w1) && (v2 eq w2)) this
      else new Map2(k1, w1, k2, w2)
    }
    def foreachKey(f: K => Unit) = { f(k1); f(k2) }
  }

  class Map3[K, +V >: Null <: AnyRef] (k1: K, v1: V, k2: K, v2: V, k3: K, v3: V) extends SimpleMap[K, V] {
    def size = 3
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
    def updated[V1 >: V <: AnyRef](k: K, v: V1) =
      if (k == k1) new Map3(k, v, k2, v2, k3, v3)
      else if (k == k2) new Map3(k1, v1, k, v, k3, v3)
      else if (k == k3) new Map3(k1, v1, k2, v2, k, v)
      else new Map4(k1, v1, k2, v2, k3, v3, k, v)
    def mapValues[V1 >: V <: AnyRef](f: V1 => V1) = {
      val w1 = f(v1); val w2 = f(v2); val w3 = f(v3)
      if ((v1 eq w1) && (v2 eq w2) && (v3 eq w3)) this
      else new Map3(k1, w1, k2, w2, k3, w3)
    }
    def foreachKey(f: K => Unit) = { f(k1); f(k2); f(k3) }
  }

  class Map4[K, +V >: Null <: AnyRef] (k1: K, v1: V, k2: K, v2: V, k3: K, v3: V, k4: K, v4: V) extends SimpleMap[K, V] {
    def size = 4
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
    def updated[V1 >: V <: AnyRef](k: K, v: V1) =
      if (k == k1) new Map4(k, v, k2, v2, k3, v3, k4, v4)
      else if (k == k2) new Map4(k1, v1, k, v, k3, v3, k4, v4)
      else if (k == k3) new Map4(k1, v1, k2, v2, k, v, k4, v4)
      else if (k == k4) new Map4(k1, v1, k2, v2, k3, v3, k, v)
      else new Map5(k1, v1, k2, v2, k3, v3, k4, v4, k, v)
    def mapValues[V1 >: V <: AnyRef](f: V1 => V1) = {
      val w1 = f(v1); val w2 = f(v2); val w3 = f(v3); val w4 = f(v4)
      if ((v1 eq w1) && (v2 eq w2) && (v3 eq w3) && (v4 eq w4)) this
      else new Map4(k1, w1, k2, w2, k3, w3, k4, w4)
    }
    def foreachKey(f: K => Unit) = { f(k1); f(k2); f(k3); f(k4) }
  }

  class Map5[K, +V >: Null <: AnyRef] (k1: K, v1: V, k2: K, v2: V, k3: K, v3: V, k4: K, v4: V, k5: K, v5: V) extends SimpleMap[K, V] {
    def size = 5
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
    def updated[V1 >: V <: AnyRef](k: K, v: V1) =
      if (k == k1) new Map5(k, v, k2, v2, k3, v3, k4, v4, k5, v5)
      else if (k == k2) new Map5(k1, v1, k, v, k3, v3, k4, v4, k5, v5)
      else if (k == k3) new Map5(k1, v1, k2, v2, k, v, k4, v4, k5, v5)
      else if (k == k4) new Map5(k1, v1, k2, v2, k3, v3, k, v, k5, v5)
      else if (k == k5) new Map5(k1, v1, k2, v2, k3, v3, k4, v4, k, v)
      else new MapMore(Map(k1 -> v1, k2 -> v2, k3 -> v3, k4 -> v4, k5 -> v5, k -> v))
    def mapValues[V1 >: V <: AnyRef](f: V1 => V1) = {
      val w1 = f(v1); val w2 = f(v2); val w3 = f(v3); val w4 = f(v4); val w5 = f(v5)
      if ((v1 eq w1) && (v2 eq w2) && (v3 eq w3) && (v4 eq w4) && (v5 eq w5)) this
      else new Map5(k1, w1, k2, w2, k3, w3, k4, w4, k5, w5)
    }
    def foreachKey(f: K => Unit) = { f(k1); f(k2); f(k3); f(k4); f(k5) }
  }

  class MapMore[K, +V >: Null <: AnyRef] (m: Map[K, V]) extends SimpleMap[K, V] {
    def size = m.size
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
    def updated[V1 >: V <: AnyRef](k: K, v: V1) = {
      assert(m.size < 12) // !!!DEBUG - want to see when maps blow up
      new MapMore(m.updated(k, v))
    }
    override def contains(k: K) = m contains k
    def mapValues[V1 >: V <: AnyRef](f: V1 => V1) = {
      val assocs = m.toList
      val assocs1 = assocs mapConserve {
        case assoc @ (k, v) =>
          val w = f(v)
          if (w eq v) assoc else (k, w)
      }
      if (assocs1 eq assocs) this else new MapMore(assocs1.toMap)
    }
    def foreachKey(f: K => Unit) = { m.keysIterator foreach f }
  }
}
