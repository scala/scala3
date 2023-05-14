class Test:
  def f[X <: String](x: List[X]): String = ???
  def f(x: List[Int]): String = ??? // error

