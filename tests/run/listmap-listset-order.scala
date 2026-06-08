object Test {
  def main(args: Array[String]): Unit = {
    // ListMap iterator should preserve insertion order
    val m = scala.collection.immutable.ListMap(1 -> "a", 2 -> "b", 3 -> "c")
    val iterList = m.iterator.toList
    assert(iterList == List(1 -> "a", 2 -> "b", 3 -> "c"),
      s"ListMap iterator should preserve insertion order, got $iterList")

    // ListMap hashCode should be deterministic and order-independent
    val m2 = scala.collection.immutable.ListMap(3 -> "c", 2 -> "b", 1 -> "a")
    val m3 = scala.collection.immutable.ListMap(1 -> "a", 2 -> "b", 3 -> "c")
    // mapHash is unordered so different insertion orders should give same hash
    assert(m2.hashCode == m3.hashCode,
      s"ListMap hashCode should be order-independent: ${m2.hashCode} vs ${m3.hashCode}")

    // ListSet iterator should preserve insertion order
    val s = scala.collection.immutable.ListSet(1, 2, 3)
    val sIterList = s.iterator.toList
    assert(sIterList == List(1, 2, 3),
      s"ListSet iterator should preserve insertion order, got $sIterList")

    println("OK")
  }
}
