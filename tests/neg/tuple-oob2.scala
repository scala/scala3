object Test {
  def elem(xs: (Int, String), n: Int) = xs(n) // error: argument to transparent parameter must be a constant expression
}