package covtest

class ContextFunctions2:
  def f1(a: Int)(b: Int)(c: Int) = a+b+c
  def f2 = (a: Int) => (b: Int) => (c: Int) =>
    a+b+c

  def g1 = (a: Int) ?=> (b: Int) ?=> (c: Int) ?=>
    a+b+c

  def g2(using a: Int)(using b: Int)(using c: Int) = a+b+c

  f1(0)(1)(2)
  f2(0)(1)(2)
  g1(using 0)(using 1)(using 2)
  g2(using 0)(using 1)(using 2)
