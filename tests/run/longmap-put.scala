object Test {
  def main(args: Array[String]): Unit = {
    import scala.collection.mutable.LongMap

    val m = LongMap.empty[Int]
    m.put(0L, 1)
    assert(m.put(0L, 2) == Some(1))

    m.put(1L, 10)
    assert(m.put(1L, 20) == Some(10))

    // regression: LongMap.put returning None for existing Long.MinValue key
    m.put(Long.MinValue, 100)
    assert(m.put(Long.MinValue, 200) == Some(100))

    m.put(Long.MaxValue, 300)
    assert(m.put(Long.MaxValue, 400) == Some(300))

    assert(m.put(999L, 1) == None)
  }
}
