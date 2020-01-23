object Test {

  type Foo

  extension on (y: Any) {
    def g with Foo : Any = ???
  }

  def f(x: Any) with Foo : Any = {
    val y = x.g
    y.g

    x.g.g
  }
}