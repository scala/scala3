class Foo {
  def f(x: String)(using DummyImplicit): String = x * 2
  def f(x: String)(color: Boolean)(using DummyImplicit): String = x * 3
}

@main def Test = println(new Foo().f("foo"))
