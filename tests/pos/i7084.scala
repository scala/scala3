object Test {

  type Foo

  extension of (y: Any) with {
    def g(given Foo): Any = ???
  }

  def f(x: Any)(given Foo): Any = {
    val y = x.g
    y.g

    x.g.g
  }
}