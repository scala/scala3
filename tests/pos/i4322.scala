object Foo {
  private[this] var x: Int = 1

  private def x(n: String): Int = n.toInt

  inline def foo: Int = x + x + x("22")

  inline def bar = {
    x += 1
    x += 1
  }

  inline def baz = { x += x("11") }
}
