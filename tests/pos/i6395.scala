object Foo {
  inline def (self: Int) foo (that: Int): Int = 5
  def (self: Int) bar: Int = self
  1.foo(2).bar
}