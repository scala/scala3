object Asserts {
  def assertEquals(expected: Any, actual: Any): Unit = {
    println(1)
    assert(expected.equals(actual), s"expected $expected but got $actual")
  }

  def assertEquals(expected: Long, actual: Long): Unit = {
    println(2)
    assert(expected == actual, s"expected $expected but got $actual")
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    def foo(): Long = 42L

    Asserts.assertEquals(42, foo()) // an Int and a Long
  }
}