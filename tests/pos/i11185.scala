class Test:
  def foo(a: Int, b: Int) = a + b

  Map(1 -> 2).map(foo _)
  Map(1 -> 2).map(foo)

class Test2:
  def foo(a: Int, b: Int) = a + b

  def bar(f: ((Int, Int)) => Int) = "ok"
  def bar(f: ((Int, Int)) => String)(using Int) = "ok"

  bar(foo)
  bar(foo _)
