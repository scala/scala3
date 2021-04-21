class Test:
  def foo(a: Int, b: Int) = a + b

  Map(1 -> 2).map(foo _)
  Map(1 -> 2).map(foo)
