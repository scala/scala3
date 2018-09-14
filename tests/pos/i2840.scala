object Test {
  class Type
  def f(x: Type): Type = ???
  def g(xs: List[Type]): Int = {
    val ys = xs.map(f)
    ys.toSet.size
  }
}
