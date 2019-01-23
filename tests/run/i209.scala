case class Foo(a: Int) {
  def copy(i: Int = 9): Foo = Foo(i)
}

case class Bar(a: Int, b: Int) {
  def copy(i: Int = 4, j: Int = 6): Bar = Bar(i, j)
}

object Test {
  def main(args: Array[String]): Unit = {
    val a = Foo(2)
    assert(a == Foo(2))
    assert(a.copy(5) == Foo(5))
    assert(a.copy() == Foo(9))

    val b = Bar(2, 3)
    assert(b == Bar(2, 3))
    assert(b.copy(5, 7) == Bar(5, 7))
    assert(b.copy(5) == Bar(5, 6))
    assert(b.copy(j = 5) == Bar(4, 5))
    assert(b.copy() == Bar(4, 6))
  }
}
