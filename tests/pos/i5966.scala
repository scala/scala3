object Test {
  def foo = (v: Int) ?=> (x: Int) => v + x
  given myInt: Int = 4

  foo.apply(1)
  foo(using 2)
  foo(3)
}
