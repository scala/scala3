package dotty.tools.dotc.util

import collection.immutable

/** A linear map is a map where after an `updated` the previous map
 *  value cannot be used anymore. The map is implemented as an immutable
 *  map for sizes <= 4 (where immutable maps have specialized, compact
 *  representations) and as a HashMap for larger sizes.
 */
opaque type LinearMap[K <: AnyRef, V >: Null <: AnyRef] =
  immutable.Map[K, V] | HashMap[K, V]

object LinearMap:

  def Empty[K <: AnyRef, V >: Null <: AnyRef]: LinearMap[K, V] =
    immutable.Map.empty[K, V]

  extension [K <: AnyRef, V >: Null <: AnyRef](m: LinearMap[K, V]):

    def apply(key: K): V /*| Null*/ = m match
      case m: immutable.Map[K, V] @unchecked =>
        if m.contains(key) then m(key) else null
      case m: HashMap[K, V] @unchecked =>
        m.get(key)

    def updated(key: K, value: V): LinearMap[K, V] = m match
      case m: immutable.Map[K, V] @unchecked =>
        if m.size < 4 then
          m.updated(key, value)
        else
          val m1 = HashMap[K, V]()
          m.foreach(m1.put(_, _))
          m1.put(key, value)
          m1
      case m: HashMap[K, V] @unchecked =>
        m.put(key, value)
        m

    def size = m match
      case m: immutable.Map[K, V] @unchecked => m.size
      case m: HashMap[K, V] @unchecked => m.size

end LinearMap