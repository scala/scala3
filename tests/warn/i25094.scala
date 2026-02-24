//> using options -Winfer-union

object TestInferredUnion:
  def g[T](x: T) = x
  def f1(x: Int | Null) = g(x)
  def f2(x: Int) = g(if x > 0 then x else null) // warn