inline def foo(inline xs: (Int, Int)): xs.type = { val a: Int = xs._1; xs }
def bar =
  val tup: 1 *: 2 *: EmptyTuple = ???
  val tup2: tup.type = foo(tup)
