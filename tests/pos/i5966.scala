object Test {
  def foo = (using v: Int) => (x: Int) => v + x
  given myInt as Int = 4

  foo.apply(1)
  foo(using 2)
  foo(3)
}
