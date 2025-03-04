object Test:

  def foo(x: Object^, ys: List[Object^]) = ???
  def test(io: Object^, async: Object^): Unit =
    val v: Object^{io} = ???
    foo(v, List(async))
