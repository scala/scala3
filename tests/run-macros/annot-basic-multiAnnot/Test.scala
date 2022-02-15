class Foo:
  @callHello @hello
  def bar = 3

@main def Test =
  val foo = new Foo
  assert(foo.bar == 3)
