object Test:

  def f(x: => Boolean) = x

  f(x = true)
