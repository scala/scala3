package dotty.tools.dotc.util

abstract class SimpleMap[-K, +V >: Null] {
  def apply(k: K): V
  def remove(k: K): SimpleMap[K, V]
  def updated[V1 >: V](k: K, v: V1): SimpleMap[K, V1]
  def contains(k: K): Boolean = apply(k) != null
}

object SimpleMap {

  object Empty extends SimpleMap[Any, Null] {
    def apply(k: Any) = null
    def remove(k: Any) = this
    def updated[V1 >: Null](k: Any, v: V1) = new Map1(k, v)
  }

  class Map1[-K, +V >: Null] (k1: K, v1: V) extends SimpleMap[K, V] {
    def apply(k: K) =
      if (k == k1) v1
      else null
    def remove(k: K) =
      if (k == k1) Empty
      else this
    def updated[V1 >: V](k: K, v: V1) =
      if (k == k1) new Map1(k, v)
      else new Map2(k1, v1, k, v)
  }

  class Map2[-K, +V >: Null] (k1: K, v1: V, k2: K, v2: V) extends SimpleMap[K, V] {
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
  }

  class Map3[-K, +V >: Null] (k1: K, v1: V, k2: K, v2: V, k3: K, v3: V) extends SimpleMap[K, V] {
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
  }

  class Map4[-K, +V >: Null] (k1: K, v1: V, k2: K, v2: V, k3: K, v3: V, k4: K, v4: V) extends SimpleMap[K, V] {
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
  }

  class Map5[-K, +V >: Null] (k1: K, v1: V, k2: K, v2: V, k3: K, v3: V, k4: K, v4: V, k5: K, v5: V) extends SimpleMap[K, V] {
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
  }

  class MapMore[-K, +V >: Null] (m: Map[K, V]) extends SimpleMap[K, V] {
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
  }
}
