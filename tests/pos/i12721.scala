def bar(t: Any): Int = 1
def foo(t: AnyRef): Unit =
  (??? : Tuple).toList.map(bar)
  (??? : EmptyTuple).toList.map(bar)
  (??? : NonEmptyTuple).toList.map(bar)
  (??? : *:[?, ?]).toList.map(bar)
  (??? : (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)).toList.map(bar)