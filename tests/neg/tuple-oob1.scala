object Test {
  def elem(xs: (Int, String)) = xs(2) // error: index out of bounds: 2
}