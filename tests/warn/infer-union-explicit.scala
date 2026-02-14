//> using options -Winfer-union

object TestExplicit:
  def f(x: Int | Null) = g(x)
  def g[T](x: T) = x