object Test {
  def main(args: Array[String]) = {
    val m1: Map[String, Int] = Map("a" -> 1, "b" -> 2)
    val m2: Map[String, Int] = Map(("a", 1), ("b", 2))
    assert(m1 == m2)
  }
}
