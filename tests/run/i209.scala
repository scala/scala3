case class Foo(a: Int) {
  def copy(i: Int = 9): Foo = Foo(i)
}

case class Bar(a: Int, b: Int) {
  def copy(i: Int = 4, j: Int = 6): Bar = Bar(i, j)
}

case class Baz(a: Int) {
  def copy(i: Int): Baz = Baz(2 * i)
}

case class PBaz(a: Int) {
  private def copy(i: Int): PBaz = PBaz(2 * i)
  def copy2(i: Int): PBaz = copy(i)
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

    val c = Baz(2)
    assert(c == Baz(2))
    assert(c.copy(3) == Baz(6))

    val d = PBaz(2)
    assert(d == PBaz(2))
    assert(d.copy2(3) == PBaz(6))
  }
}
