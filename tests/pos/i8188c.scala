class Foo {
  def f(x: String)(using DummyImplicit): String = x * 2
  def f(x: String)(using DummyImplicit)(color: Boolean): String = x * 3
}

@main def Test = println(new Foo().f("foo"))
