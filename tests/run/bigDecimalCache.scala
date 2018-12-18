object Test {
  def main(args: Array[String]): Unit = {
    val bd5a = BigDecimal(5)
    val mc = java.math.MathContext.DECIMAL32.nn
    val bd5b = BigDecimal(5,mc)

    assert(bd5b.mc == mc)
  }
}
