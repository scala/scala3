object Foo {
  private[this] var x: Int = 1

  private def x(n: String): Int = n.toInt

  transparent def foo: Int = x + x + x("22")

  transparent def bar = {
    x += 1
    x += 1
  }

  transparent def baz = { x += x("11") }
}
