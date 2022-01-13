inline def foo(xs: (Int, Int)): Unit = { val a: Int = xs._1; }
def fail = foo(1 *: 2 *: EmptyTuple)
def ok = foo((1, 2))
