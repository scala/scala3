
object Test {

  def main(args: Array[String]): Unit = {
    posTests()
    negTests()
  }

  def posTests() = {
    import Foo._
    assertEquals(foo"abc${"123"}def", "abc123def")
  }

  def negTests() = {
    import TestFooErrors._
    assertEquals(foo"a#c${"123"}def", List((0, 256, 259, "Cannot use #")))
    assertEquals(foo"abc${"123"}#ef", List((1, 342, 345, "Cannot use #")))
    assertEquals(foo"a#c${"123"}#ef", List((0, 406, 409, "Cannot use #"), (1, 417, 420, "Cannot use #")))
  }

  def assertEquals(actual: Any, expected: Any): Unit = {
    assert(actual == expected, s"actual: $actual\nbut expected: $expected")
  }
}
