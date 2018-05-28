object Test {
  private[this] var count = 0

  def test(x: Int) = { count += 1; true }

  object Foo {
    def unapply(x: Int): Option[Int] = { count += 1; Some(x) }
  }

  def main(args: Array[String]): Unit = {
    val res = List(1, 2).collect { case x if test(x) => x }
    assert(count == 2)

    count = 0
    val res2 = List(1, 2).collect { case Foo(x) => x }
    assert(count == 2)
  }
}
