  def bar(t: Any): Int = 1
  def foo(t: AnyRef): Unit =
    t.asInstanceOf[NonEmptyTuple].toList.map(bar)
