object Bar:
  class Foo:
    protected[Bar] def foo = 23
  class Qux extends Foo:
    val qux = foo

def test(foo: Bar.Foo) =
  foo.foo // error
