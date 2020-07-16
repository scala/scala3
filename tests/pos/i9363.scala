class Foo:
  protected inline def fooProtected: Int = 12

object Foo extends Foo:
  inline def foo: Int = fooProtected

object Bar:
  def bar: Int = Foo.foo
