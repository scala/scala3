object Test {
  def foo = given (v: Int) => (x: Int) => v + x
  implied myInt for Int = 4

  foo.apply(1)
  foo given 2
  foo(3)
}
