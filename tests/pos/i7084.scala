object Test {

  type Foo

  given {
    def (y: Any) g given Foo: Any = ???
  }

  def f(x: Any) given Foo: Any = {
    val y = x.g
    y.g

    x.g.g
  }
}