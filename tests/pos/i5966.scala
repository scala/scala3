object Test {
  def foo = given (v: Int) => (x: Int) => v + x
  given myInt as Int = 4

  foo.apply(1)
  foo given 2
  foo(3)
}
