object Foo {
  extension (self: Int) inline def foo(that: Int): Int = 5
  extension (self: Int) def bar: Int = self
  1.foo(2).bar
}