def Test = {
  val avs = ((0d, BigDecimal(1)), (1d, BigDecimal(2)))

  val ((y1, g1), (y2, g2)) = avs

  val ((x1, f1), (x2, f2)): (
    (Double, BigDecimal),
    (Double, BigDecimal)
  ) = avs

  val (a, b, c: Double): (Int, String, Double) = ((42, "hello, world", 3.14): Any).runtimeChecked
}
