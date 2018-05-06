object Foo {
  private[this] var x: Int = 1

  inline def foo: Int = x + x + x

  inline def bar = {
    x += 1
    x += 1
  }
}
