class A:
  @check
  def foo(x: Int) = 1

@main def Test =
  (new A).foo(3)
