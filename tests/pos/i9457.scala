object inlinetuple with
  def test: Int =
    f((1, 2))
  inline def f(inline p: (Int, Int)): Int =
    val (a, b) = p
    a + b
