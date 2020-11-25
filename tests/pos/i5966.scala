object Test {
  def foo = (v: Int) ?=> (x: Int) => v + x
  given Int as myInt = 4

  foo.apply(1)
  foo(using 2)
  foo(3)
}
