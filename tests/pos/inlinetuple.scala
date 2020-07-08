object Test:

  def g(x: Int, y: Int) = x + y
  inline def f(inline x: (Int, Int)) = g(x._1, x._2)

  val x = f((1, 2))

