class Test {
  def foo(x: PartialFunction[Int, Int]) = x(0)

  foo({ case i => i})
}
