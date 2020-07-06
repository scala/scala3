object Test {

  type Foo

  extension (y: Any) {
    def g(using Foo): Any = ???
  }

  def f(x: Any)(using Foo): Any = {
    val y = x.g
    y.g

    x.g.g
  }
}