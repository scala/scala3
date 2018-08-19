object Foo {
  private[this] var x: Int = 1

  private def x(n: String): Int = n.toInt

  rewrite def foo: Int = x + x + x("22")

  rewrite def bar = {
    x += 1
    x += 1
  }

  rewrite def baz = { x += x("11") }
}
