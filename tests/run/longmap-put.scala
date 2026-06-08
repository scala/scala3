object Test {
  def main(args: Array[String]): Unit = {
    import scala.collection.mutable.LongMap

    // LongMap.put should return Some for existing keys
    val m = LongMap.empty[Int]
    m.put(0L, 1)
    assert(m.put(0L, 2) == Some(1), "put on existing key 0 should return Some(1)")

    m.put(1L, 10)
    assert(m.put(1L, 20) == Some(10), "put on existing key 1 should return Some(10)")

    // Regression test: LongMap.put returning None for existing Long.MinValue key
    // The bug was in extraKeys bit check: (extraKeys&2) == 1 instead of == 2
    m.put(Long.MinValue, 100)
    assert(m.put(Long.MinValue, 200) == Some(100),
      s"put on existing Long.MinValue key should return Some(100), got ${m.put(Long.MinValue, 200)}")

    // Long.MaxValue
    m.put(Long.MaxValue, 300)
    assert(m.put(Long.MaxValue, 400) == Some(300),
      s"put on existing Long.MaxValue key should return Some(300), got ${m.put(Long.MaxValue, 400)}")

    // put on non-existing key should return None
    assert(m.put(999L, 1) == None, "put on non-existing key should return None")

    println("OK")
  }
}
