inline def foo(xs: => (Int, Int)): Unit = { val a: Int = xs._1; }
def bar =
  foo(1 *: 2 *: EmptyTuple)
