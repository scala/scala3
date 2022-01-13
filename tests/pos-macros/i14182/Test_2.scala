inline def foo(inline xs: (Int, Int)): Unit = ${ fooImpl('xs) }
def fail = foo(1 *: 2 *: EmptyTuple)
def ok = foo((1, 2))
