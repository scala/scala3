object Test {
  def main(args: Array[String]): Unit = {
    val m = scala.collection.immutable.ListMap(1 -> "a", 2 -> "b", 3 -> "c")
    assert(m.iterator.toList == List(1 -> "a", 2 -> "b", 3 -> "c"))

    val m2 = scala.collection.immutable.ListMap(3 -> "c", 2 -> "b", 1 -> "a")
    val m3 = scala.collection.immutable.ListMap(1 -> "a", 2 -> "b", 3 -> "c")
    assert(m2.hashCode == m3.hashCode)

    val s = scala.collection.immutable.ListSet(1, 2, 3)
    assert(s.iterator.toList == List(1, 2, 3))
  }
}
