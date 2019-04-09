class Foo {
  inline def foo(inline x: Int): Int = x
  def bar: Int = foo(foo(4))
}
