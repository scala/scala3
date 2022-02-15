class Bar:
  @hello
  object Foo:
    @double
    def foo(x: Int) = x + 1

@main def Test =
  assert((new Bar).Foo.foo(3) == 6)
