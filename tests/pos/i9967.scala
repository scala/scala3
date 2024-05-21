import collection.mutable

class MaxSizeMap[K, V](maxSize: Int)(using o: Ordering[K]):
  val sortedMap: mutable.TreeMap[K, V] = mutable.TreeMap.empty[K, V](using o)

  export sortedMap._
