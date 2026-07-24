//> using options -source:future

val avs = ((0d, BigDecimal(1)), (1d, BigDecimal(2)))

val ((x1, f1), (x2, f2)): ( // error
  (Double, BigDecimal),
  (Double, BigDecimal)
) = avs
